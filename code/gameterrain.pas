{
  Copyright 2017-2023 Michalis Kamburelis.

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
  CastleUIControls, CastleTerrain, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, CastleRenderOptions,
  CastleTransform, CastleVectors, CastleTriangles, CastleRectangles,
  CastleUtils, CastleBoxes, CastleNotifications;

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
var
  TerrainNoise: TCastleTerrainNoise;
  GridCount: Cardinal;
  GridStep: Single;
  MoveLimit: TBox3D;
  Body: TCastleRigidBody;
  Collider: TCastleMeshCollider;
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
  FTerrain.Subdivisions := Vector2(GridCount, GridCount);
  FTerrain.PreciseCollisions := true;
  FTerrain.Name := 'FTerrain';
  FTerrain.Layer1.Texture := 'castle-data:/terrain/textures/island_sand2_d.jpg';
  FTerrain.Layer2.Texture := 'castle-data:/terrain/textures/ground_mud2_d.jpg';
  FTerrain.Layer3.Texture := 'castle-data:/terrain/textures/mntn_white_d.jpg';
  FTerrain.Layer4.Texture := 'castle-data:/terrain/textures/snow_mud_d.jpg';
  FTerrain.Layer1.UvScale := 0.11;
  FTerrain.Layer2.UvScale := 0.26;
  FTerrain.Layer3.UvScale := 0.36;
  FTerrain.Layer4.UvScale := 0.36;
  FTerrain.Data := TerrainNoise;

  Body := TCastleRigidBody.Create(Self);
  FTerrain.AddBehavior(Body);

  Collider := TCastleMeshCollider.Create(Self);
  Collider.Mesh := FTerrain;
  Collider.Restitution := 0.3;
  FTerrain.AddBehavior(Collider);

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
  FTerrain.Size := Vector2(
    GridCount * GridStep,
    GridCount * GridStep);

  WritelnLog('Bounding box of terrain ' + FTerrain.BoundingBox.ToString);

  Viewport.Items.Add(FTerrain);

  OnFixCamera;

  // make gravity work even if your position is over the world bbox
  MoveLimit := Viewport.Items.BoundingBox;
  MoveLimit.Max := MoveLimit.Max + Vector3(0, 1000, 0);
  Viewport.Items.MoveLimit := MoveLimit;
end;

end.
