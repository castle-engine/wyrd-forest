{
  Copyright 2017-2022 Michalis Kamburelis.

  This file is part of "Wyrd Forest".

  "Wyrd Forest" is free software; see the file LICENSE.txt,
  included in this distribution, for details about the copyright.

  "Wyrd Forest" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Display and edit a terrain (TMyTerrain). }
unit GameTerrain;

interface

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleUIState, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, CastleRendererBaseTypes,
  CastleTransform, CastleVectors, CastleTriangles, CastleRectangles,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications;

type
  TFixCameraEvent = procedure of object;

  { Display and edit a terrain. }
  TMyTerrain = class(TComponent)
  strict private
    FTerrain: TCastleTerrain;
  public
    Viewport: TCastleViewport;
    OnFixCamera: TFixCameraEvent;
    InitialGridCount: Cardinal;
    procedure Initialize; //< Assing fields before calling this
    property Terrain: TCastleTerrain read FTerrain;
  end;

implementation

uses Math;

{ TMyTerrain ------------------------------------------------------------------- }

procedure TMyTerrain.Initialize;

  function CreateRigidBody: TRigidBody;
  var
    Collider: TMeshCollider;
  begin
    Result := TRigidBody.Create(Self);
    Result.Dynamic := false;

    Collider := TMeshCollider.Create(Result);
    { TODO: it's a hack to access internal TCastleScene under terrain this way,
      better solution will come from new TCastleMeshCollider. }
    Collider.Scene := FTerrain.Items[0] as TCastleScene;
    Collider.Restitution := 0.3;
  end;

var
  TerrainNoise: TCastleTerrainNoise;
  GridCount: Cardinal;
  GridStep: Single;
  MoveLimit: TBox3D;
begin
  TerrainNoise := TCastleTerrainNoise.Create(Self);
  TerrainNoise.Interpolation := niSpline; // slowest but best quality
  TerrainNoise.Octaves := 6.94;
  TerrainNoise.Smoothness := 1.63; //1.9;
  TerrainNoise.Amplitude := 7.85;
  TerrainNoise.Frequency := 0.04;
  TerrainNoise.Heterogeneous := 0.64;

  GridCount := InitialGridCount;
  GridStep := 0.57;

  FTerrain := TCastleTerrain.Create(Self);
  FTerrain.Subdivisions := GridCount;
  FTerrain.PreciseCollisions := true;
  FTerrain.Name := 'FTerrain';
  FTerrain.Texture1 := 'castle-data:/terrain/textures/island_sand2_d.jpg';
  FTerrain.Texture2 := 'castle-data:/terrain/textures/ground_mud2_d.jpg';
  FTerrain.Texture3 := 'castle-data:/terrain/textures/mntn_white_d.jpg';
  FTerrain.Height0 := 0.42;
  FTerrain.Height1 := 1.94;
  FTerrain.Height2 := 5;
  FTerrain.Height3 := 10;
  FTerrain.UvScale1 := 0.11;
  FTerrain.UvScale2 := 0.26;
  FTerrain.UvScale3 := 0.36;
  FTerrain.TextureMix := 1;
  FTerrain.NormalDark := 0.94;
  FTerrain.NormalDarkening := 0.3;
  FTerrain.Data := TerrainNoise;

  { User controls Size only implicitly, by GridCount,
    and controls explicitly GridStep.
    This is most comfortable usually, you have 2 parameters that
    behave orthogonally, and neither of them affects the "true" shape
    of the underlying terrain (represented by TerrainNoise).

    - GridStep allows you to see more or less detail.
      It does not affect FPS.

    - Changing GridCount does not change the details visible close to you.
      The details stay the same.
      Increasing GridCount just means that terrain is larger.
      This affects FPS.

    Both of them allow you to see further, but in different ways
    (one of them sacrifices details, the other one increases mesh density).
  }
  FTerrain.Size := GridCount * GridStep;

  WritelnLog('Bounding box of terrain ' + FTerrain.BoundingBox.ToString);

  Viewport.Items.Add(FTerrain);

  OnFixCamera;

  // make gravity work even if your position is over the world bbox
  MoveLimit := Viewport.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  Viewport.Items.MoveLimit := MoveLimit;

  FTerrain.RigidBody.Free;
  FTerrain.RigidBody := CreateRigidBody;
end;

end.
