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

{ Enemies list and enemy logic. }
unit GameEnemies;

interface

uses Classes,
  CastleVectors, CastleTransform, CastleTimeUtils, CastleScene, CastleSceneManager;

type
  THeightAboveTerrainEvent = function (Pos: TVector3; out Y: Single): boolean of object;

  { Enemies list. It is a TCastleTransform descendant, and it should
    also be added to the SceneManager.Items.
    It automatically takes care of spawning the enemies. }
  TEnemies = class(TCastleTransform)
  private
    EnemyLastSpawn: TTimerResult;
    EnemyTemplate: TCastleScene;
    procedure TryEnemySpawn;
  public
    SceneManager: TCastleSceneManager;
    OnHeightAboveTerrain: THeightAboveTerrainEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses Math,
  CastleFilesUtils, CastleUtils,
  GameSpawnable;

constructor TEnemies.Create(AOwner: TComponent);
begin
  inherited;

  EnemyTemplate := TCastleScene.Create(Self);
  EnemyTemplate.Load(ApplicationData('evil_squirrel/evil-squirrel-board.castle-anim-frames'));

  EnemyLastSpawn := Timer;
end;

procedure TEnemies.TryEnemySpawn;

  { Test is it flat around Pos.
    Also, update Pos.Y to be correct (stand firmly on the terrain)
    (it is updated regardless of the final result). }
  function SpawnPositionFlat(var Pos: TVector3): boolean;
  const
    TestStep = 0.5;
    MaxTolerableDifference = 1.0;
  var
    MinY, MaxY: Single;
    TestPos: TVector3;
    TestY: Single;
    I, J: Integer;
  begin
    if not OnHeightAboveTerrain(Pos, TestY) then Exit(false);
    Pos.Y := TestY;
    MinY := TestY;
    MaxY := TestY;

    for I := -2 to 2 do
      for J := -2 to 2 do
        // no need to check I = J = 0 case
        if (I <> 0) or (J <> 0) then
        begin
          TestPos := Pos + Vector3(I * TestStep, 0, J * TestStep);
          if not OnHeightAboveTerrain(TestPos, TestY) then Exit(false);
          MinVar(MinY, TestY);
          MaxVar(MaxY, TestY);
        end;

    Result := MaxY - MinY < MaxTolerableDifference;
    // Writeln('Test result: ', Result, ' with ', (MaxY - MinY):1:2);
  end;

  function FindSpawnPosition(out Pos: TVector3): boolean;
  const
    SearchSpawnPositionTries = 10;
  var
    I: Integer;
    Dir, Side: TVector3;
  begin
    for I := 0 to SearchSpawnPositionTries - 1 do
    begin
      Pos := SceneManager.WalkCamera.Position;
      Dir := SceneManager.WalkCamera.DirectionInGravityPlane;
      Side := TVector3.CrossProduct(Dir, SceneManager.GravityUp);

      Pos := Pos +
        Dir * RandomFloatRange(2, 10) +
        Side * RandomFloatRange(-5, 5);
      if SpawnPositionFlat(Pos) then
        Exit(true);
    end;
    Result := false;
  end;

var
  Spawn: TSpawnable;
  Pos, Dir: TVector3;
begin
  if FindSpawnPosition(Pos) then
  begin
    Spawn := TSpawnable.Create(Self);
    Spawn.Spawn(EnemyTemplate);
    Spawn.Translation := Pos;

    { make the enemy face player }
    Dir := Pos - SceneManager.WalkCamera.Position;
    if not VectorsParallel(Dir, SceneManager.GravityUp) then
    begin
      MakeVectorsOrthoOnTheirPlane(Dir, SceneManager.GravityUp);
      Spawn.Direction := Dir;
    end;

    Add(Spawn);
  end;
end;

procedure TEnemies.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  EnemySpawnDelay = 2.0;
  EnemyMaxCount = 20;
begin
  inherited;

  if EnemyLastSpawn.ElapsedTime > EnemySpawnDelay then
  begin
    { update EnemyLastSpawn even if EnemyMaxCount check was unsatisfied.
      This way we avoid synchronizing next enemy spawn with shooting previous
      enemy. }
    EnemyLastSpawn := Timer;
    if Count < EnemyMaxCount then
      TryEnemySpawn;
  end;
end;

end.
