{
  Copyright 2017-2017 Michalis Kamburelis.

  This file is part of "Wyrd Forest".

  "Wyrd Forest" is free software; see the file LICENSE.txt,
  included in this distribution, for details about the copyright.

  "Wyrd Forest" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display and edit a terrain (TTerrain). }
unit GameTerrain;

interface

uses SysUtils, Classes,
  CastleWindowTouch, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleUIState, CastleSceneManager,
  CastleCameras, X3DNodes, X3DFields, CastleRendererBaseTypes,
  CastleTransform, CastleVectors, CastleTriangles,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications;

type
  TFixCameraEvent = procedure of object;

  { Display and edit a terrain. }
  TTerrain = class(TComponent)
  strict private
    FScene: TCastleScene;

    { Shader parameters. }
    Effect: TEffectNode;
    TextureHeights: array [0..3] of Single;
    TextureHeightsFields: array [0..3] of TSFFloat;
    UVScale: array [1..3] of Single;
    UVScaleFields: array [1..3] of TSFFloat;
    TextureMix, NormalDark, NormalDarkening: Single;

    { Terrain noise parameters. }
    GridCount: Cardinal;
    GridStep: Single;
    Octaves: Single;
    Smoothness: Single;
    Amplitude: Single;
    Frequency: Single;
    Heterogeneous: Single;

    procedure UpdateScene(Sender: TObject);
    procedure UpdateShader(Sender: TObject);
  public
    SceneManager: TCastleSceneManager;
    OnFixCamera: TFixCameraEvent;
    constructor Create(AOwner: TComponent; const InitialGridCount: Cardinal); reintroduce;
    procedure AddSlidersToMenu(OnScreenMenu: TCastleOnScreenMenu);
    procedure CreateScene;
    property Scene: TCastleScene read FScene;
  end;

implementation

uses Math;

{ TTerrain ------------------------------------------------------------------- }

constructor TTerrain.Create(AOwner: TComponent; const InitialGridCount: Cardinal);
begin
  inherited Create(AOwner);

  GridCount := InitialGridCount;
  GridStep := 0.57;
  Octaves := 6.94;
  Smoothness := 1.63; //1.9;
  Amplitude := 7.85;
  Frequency := 0.04;
  Heterogeneous := 0.64;

  TextureHeights[0] := 0.42;
  TextureHeights[1] := 1.94;
  TextureHeights[2] := 5;
  TextureHeights[3] := 10;
  UVScale[1] := 0.11;
  UVScale[2] := 0.26;
  UVScale[3] := 0.36;
  TextureMix := 1;
  NormalDark := 0.94;
  NormalDarkening := 0.3;
end;

procedure TTerrain.CreateScene;
begin
  FScene := TCastleScene.Create(Self);
  { need ssDynamicCollisions, because we change terrain scene through OnScreenMenu }
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  { we change uniforms through OnScreenMenu, so the scene should react to events }
  Scene.ProcessEvents := true;
  Scene.Name := 'TerrainScene'; // for nicer debugging
  SceneManager.Items.Add(Scene);

  UpdateScene(nil);
end;

procedure TTerrain.AddSlidersToMenu(OnScreenMenu: TCastleOnScreenMenu);

  procedure IntSlider(const Name: string; const ValuePointer: PInteger; const Min, Max: Integer;
    const OnChange: TNotifyEvent);
  begin
    OnScreenMenu.Add(Name, TCastleIntegerSlider.Create(Self,
      ValuePointer, Min, Max, OnChange));
  end;

  procedure FloatSlider(const Name: string; const ValuePointer: PSingle; const Min, Max: Single;
    const OnChange: TNotifyEvent);
  begin
    OnScreenMenu.Add(Name, TCastleFloatSlider.Create(Self,
      ValuePointer, Min, Max, OnChange));
  end;

var
  I: Integer;
begin
  OnScreenMenu.Add('Terrain shape:');

  IntSlider('Grid Count', @GridCount, 10, 200, @UpdateScene);
  FloatSlider('Grid Size', @GridStep, 0.1, 2, @UpdateScene);
  FloatSlider('Octaves', @Octaves, 0, 20, @UpdateScene);
  FloatSlider('Smoothness', @Smoothness, 1, 10, @UpdateScene);
  FloatSlider('Amplitude', @Amplitude, 0.1, 10, @UpdateScene);
  FloatSlider('Frequency', @Frequency, 0.001, 0.1, @UpdateScene);
  FloatSlider('Heterogeneous', @Heterogeneous, 0, 2, @UpdateScene);

  OnScreenMenu.Add('Terrain shader:');

  for I := Low(TextureHeights) to High(TextureHeights) do
    FloatSlider('Texture Height ' + IntToStr(I), @TextureHeights[I], 0, 10, @UpdateShader);
  for I := Low(UVScale) to High(UVScale) do
    FloatSlider('UV Scale ' + IntToStr(I), @UVScale[I], 0.01, 0.5, @UpdateShader);
  FloatSlider('Texture Mix', @TextureMix, 0, 1, @UpdateShader);
  FloatSlider('Normal Dark', @NormalDark, 0, 1, @UpdateShader);
  FloatSlider('Normal Darkening', @NormalDarkening, 0, 1, @UpdateShader);
end;

procedure TTerrain.UpdateShader(Sender: TObject);
var
  I: Integer;
begin
  { We could just call UpdateScene now, but that would be very inefficient.
    Instead we can pass new value to the existing shader. }
  for I := Low(TextureHeights) to High(TextureHeights) do
    TextureHeightsFields[I].Send(TextureHeights[I]);
  for I := Low(UVScale) to High(UVScale) do
    UVScaleFields[I].Send(UVScale[I]);
  (Effect.Field('texture_mix') as TSFFloat).Send(TextureMix);
  (Effect.Field('normal_dark') as TSFFloat).Send(NormalDark);
  (Effect.Field('normal_darkening') as TSFFloat).Send(NormalDarkening);
end;

procedure TTerrain.UpdateScene(Sender: TObject);

  procedure AdjustAppearance(Appearance: TAppearanceNode);
  var
    VertexPart, FragmentPart: TEffectPartNode;
    Tex1, Tex2, Tex3: TImageTextureNode;
    I: Integer;
  begin
    { initialize Effect node, for a shader effect }
    Effect := TEffectNode.Create;
    Effect.Language := slGLSL;
    Appearance.SetEffects([Effect]);

    { pass textures to shader effect }
    Tex1 := TImageTextureNode.Create;
    Tex1.SetUrl([ApplicationData('terrain/textures/island_sand2_d.jpg')]);
    Tex2 := TImageTextureNode.Create;
    Tex2.SetUrl([ApplicationData('terrain/textures/ground_mud2_d.jpg')]);
    Tex3 := TImageTextureNode.Create;
    Tex3.SetUrl([ApplicationData('terrain/textures/mntn_white_d.jpg')]);

    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_1', [], Tex1));
    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_2', [], Tex2));
    Effect.AddCustomField(TSFNode.Create(Effect, false, 'tex_3', [], Tex3));

    { pass uniforms to shader effect }
    for I := Low(TextureHeights) to High(TextureHeights) do
    begin
      TextureHeightsFields[I] := TSFFloat.Create(
        Effect, true, 'h' + IntToStr(I), TextureHeights[I]);
      Effect.AddCustomField(TextureHeightsFields[I]);
    end;
    for I := Low(UVScale) to High(UVScale) do
    begin
      UVScaleFields[I] := TSFFloat.Create(
        Effect, true, 'uv_scale_' + IntToStr(I), UVScale[I]);
      Effect.AddCustomField(UVScaleFields[I]);
    end;
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'texture_mix', TextureMix));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_dark', NormalDark));
    Effect.AddCustomField(TSFFloat.Create(Effect, true, 'normal_darkening', NormalDarkening));

    { initialize 2 EffectPart nodes (one for vertex shader, one for fragment shader) }
    FragmentPart := TEffectPartNode.Create;
    FragmentPart.ShaderType := stFragment;
    FragmentPart.SetUrl([ApplicationData('terrain/shaders/terrain.fs')]);

    VertexPart := TEffectPartNode.Create;
    VertexPart.ShaderType := stVertex;
    VertexPart.SetUrl([ApplicationData('terrain/shaders/terrain.vs')]);

    Effect.SetParts([FragmentPart, VertexPart]);
  end;

  function CreateRigidBody: TRigidBody;
  var
    Collider: TMeshCollider;
  begin
    Result := TRigidBody.Create(Self);
    Result.Dynamic := false;

    Collider := TMeshCollider.Create(Result);
    Collider.Scene := Scene;
    Collider.Restitution := 0.3;
  end;

var
  TerrainNoise: TTerrainNoise;
  Shape: TShapeNode;
  Root: TX3DRootNode;
  Size: Single;
  MoveLimit: TBox3D;
begin
  TerrainNoise := TTerrainNoise.Create;
  try
    TerrainNoise.Interpolation := niSpline; // slowest but best quality
    TerrainNoise.Octaves := Octaves;
    TerrainNoise.Smoothness := Smoothness;
    TerrainNoise.Amplitude := Amplitude;
    TerrainNoise.Frequency := Frequency;
    TerrainNoise.Heterogeneous := Heterogeneous;

    { User controls Size only implicitly, by GridCount,
      and controls explicitly GridStep.
      This is most comfortable usually, you have 2 parameters that
      behave orthogonally, and neither of them affects the "true" shape
      of the underlying terrain (represented by TerrainNoise).

      - grid size allows you to see more detail, but does not affect FPS.
      - distance does not increase the details visible close to you,
        but affects FPS.

      Both of them allow you to see further, but in different ways
      (one of them sacrifices details, the other one increases mesh density).
    }
    Size := GridCount * GridStep;

    Shape := TerrainNoise.CreateNode(GridCount, Size,
      Vector2(0, Size), Vector2(0, Size));

    AdjustAppearance(Shape.Appearance);
  finally FreeAndNil(TerrainNoise) end;

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);
  Scene.Load(Root, true);

  WritelnLog('Bounding box of terrain ' + Scene.BoundingBox.ToString);

  OnFixCamera;

  // make gravity work even if your position is over the world bbox
  MoveLimit := SceneManager.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  SceneManager.MoveLimit := MoveLimit;

  Scene.RigidBody.Free;
  Scene.RigidBody := CreateRigidBody;
end;

end.
