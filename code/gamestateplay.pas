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

{ TStatePlay, responsible for playing the game. }
unit GameStatePlay;

interface

uses SysUtils, Classes,
  CastleWindowTouch, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleUIState, CastleSceneManager,
  CastleCameras, X3DNodes, X3DFields, CastleRendererBaseTypes,
  CastleTransform, CastleVectors, CastleTriangles,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications;

var
  SceneManager: TCastleSceneManager;

type
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
    Divisions: Cardinal;
    GridSize: Single;
    Octaves: Single;
    Smoothness: Single;
    Amplitude: Single;
    Frequency: Single;
    Heterogeneous: Single;

    procedure UpdateScene(Sender: TObject);
    procedure UpdateShader(Sender: TObject);
  public
    constructor Create(AOwner: TComponent; const InitialDivisions: Cardinal); reintroduce;
    procedure AddSlidersToMenu(OnScreenMenu: TCastleOnScreenMenu);
    procedure CreateScene;
    property Scene: TCastleScene read FScene;
  end;

  TStatePlay = class(TUIState)
  strict private
    Status: TCastleLabel;
    OnScreenMenu: TCastleOnScreenMenu;
    Terrain: TTerrain;
    Notifications: TCastleNotifications;
  public
    InitialDivisions: Cardinal;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses Math,
  GameStateMainMenu;

{ TTerrain ------------------------------------------------------------------- }

constructor TTerrain.Create(AOwner: TComponent; const InitialDivisions: Cardinal);
begin
  inherited Create(AOwner);

  Divisions := InitialDivisions;
  GridSize := 0.57;
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
  IntSlider('Divisions', @Divisions, 10, 200, @UpdateScene);
  FloatSlider('Grid Size', @GridSize, 0.1, 2, @UpdateScene);
  FloatSlider('Octaves', @Octaves, 0, 20, @UpdateScene);
  FloatSlider('Smoothness', @Smoothness, 1, 10, @UpdateScene);
  FloatSlider('Amplitude', @Amplitude, 0.1, 10, @UpdateScene);
  FloatSlider('Frequency', @Frequency, 0.001, 0.1, @UpdateScene);
  FloatSlider('Heterogeneous', @Heterogeneous, 0, 2, @UpdateScene);

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
    Tex1.SetUrl([ApplicationData('textures/island_sand2_d.jpg')]);
    Tex2 := TImageTextureNode.Create;
    Tex2.SetUrl([ApplicationData('textures/ground_mud2_d.jpg')]);
    Tex3 := TImageTextureNode.Create;
    Tex3.SetUrl([ApplicationData('textures/mntn_white_d.jpg')]);

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
    FragmentPart.SetUrl([ApplicationData('shaders/terrain.fs')]);

    VertexPart := TEffectPartNode.Create;
    VertexPart.ShaderType := stVertex;
    VertexPart.SetUrl([ApplicationData('shaders/terrain.vs')]);

    Effect.SetParts([FragmentPart, VertexPart]);
  end;

  { fix SceneManager camera position to stand on ground }
  procedure FixCamera;
  var
    RayCollision: TRayCollision;
    P: TVector3;
  begin
    P := SceneManager.WalkCamera.Position;
    P.Y := 1000 * 1000;
    RayCollision := SceneManager.Items.WorldRay(P, Vector3(0, -1, 0));
    try
      if RayCollision <> nil then
      begin
        P.Y := RayCollision[0].Point.Y + 2;
        SceneManager.WalkCamera.Position := P;
      end else
      begin
        WritelnLog('Camera stands outside of terrain, fixing');
        SceneManager.WalkCamera.Position := SceneManager.Items.BoundingBox.Center;
        FixCamera;
      end;
    finally FreeAndNil(RayCollision) end;
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

    { User controls Size only implicitly, by Divisions,
      and controls explicitly GridSize.
      This is most comfortable usually, you have 2 parameters that
      behave orthogonally, and neither of them affects the "true" shape
      of the underlying terrain (represented by TerrainNoise).

      - grid size allows you to see more detail, but does not affect FPS.
      - distance does not increase the details visible close to you,
        but affects FPS.

      Both of them allow you to see further, but in different ways
      (one of them sacrifices details, the other one increases mesh density).
    }
    Size := Divisions * GridSize;

    Shape := TerrainNoise.CreateNode(Divisions, Size,
      Vector2(0, Size), Vector2(0, Size), nil);

    AdjustAppearance(Shape.Appearance);
  finally FreeAndNil(TerrainNoise) end;

  Root := TX3DRootNode.Create;
  Root.AddChildren(Shape);
  Scene.Load(Root, true);

  WritelnLog('Bounding box of terrain ' + Scene.BoundingBox.ToString);

  FixCamera;

  // make gravity work even if your position is over the world bbox
  MoveLimit := SceneManager.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  SceneManager.MoveLimit := MoveLimit;
end;

{ TStatePlay ----------------------------------------------------------------- }

procedure TStatePlay.Start;
var
  EnvironmentScene: TCastleScene;
begin
  inherited;

  SceneManager := TCastleSceneManager.Create(FreeAtStop);
  InsertBack(SceneManager);

  SceneManager.NavigationType := ntWalk;
  SceneManager.WalkCamera.PreferredHeight := 2;
  SceneManager.WalkCamera.SetView(
    Vector3(1, 20, 1), // Vector3(50, 0, 50);
    // look in the direction that shrinks / grows when you change Divisions
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  SceneManager.WalkCamera.MoveSpeed := 10;

  EnvironmentScene := TCastleScene.Create(FreeAtStop);
  EnvironmentScene.Load(ApplicationData('environment.x3dv'));
  EnvironmentScene.ProcessEvents := true;
  SceneManager.Items.Add(EnvironmentScene);
  SceneManager.MainScene := EnvironmentScene;

  OnScreenMenu := TCastleOnScreenMenu.Create(FreeAtStop);
  //OnScreenMenu.Exists := false;
  OnScreenMenu.Anchor(hpRight, -10);
  OnScreenMenu.Anchor(vpBottom, 10);
  InsertFront(OnScreenMenu);

  Notifications := TCastleNotifications.Create(Owner);
  Notifications.Anchor(hpMiddle);
  Notifications.Anchor(vpBottom, 10);
  Notifications.TextAlignment := hpMiddle;
  Notifications.Color := Yellow;
  InsertFront(Notifications);

  { Show a label with FPS }
  Status := TCastleLabel.Create(FreeAtStop);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could use "Vector4(1, 1, 0, 1)" instead of Yellow
  InsertFront(Status);

  Terrain := TTerrain.Create(FreeAtStop, InitialDivisions);
  Terrain.CreateScene;
  Terrain.AddSlidersToMenu(OnScreenMenu);
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  Status.Caption := Format(
    'FPS: %f' +NL+
    'Move speed (change by [-] [+]): %f' + NL +
    '[F4] Toggle mouse look' +NL+
    '[F5] Screenshot' +NL+
    '[F6] Save terrain to X3D file' +NL+
    '[F10] Toggle controls to tweak display' +NL+
    '[Escape] Back to main menu',
    [Container.Fps.RealTime,
     SceneManager.WalkCamera.MoveSpeed]);
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;
var
  S: string;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_F4) then
    SceneManager.WalkCamera.MouseLook := not SceneManager.WalkCamera.MouseLook;
  if Event.IsKey(K_F5) then
  begin
    S := FileNameAutoInc('wyrd_forest_screen_%d.png');
    Container.SaveScreen(S);
    Notifications.Show('Saved screen to ' + S);
  end;
  if Event.IsKey(K_F6) then
  begin
    S := FileNameAutoInc('terrain_%d.x3dv');
    Terrain.Scene.Save(S);
    Notifications.Show('Saved terrain 3D model to ' + S);
  end;
  if Event.IsKey(K_F10) then
    OnScreenMenu.Exists := not OnScreenMenu.Exists;
  if Event.IsKey(K_Escape) then
    TUIState.Current := StateMainMenu;
end;

end.
