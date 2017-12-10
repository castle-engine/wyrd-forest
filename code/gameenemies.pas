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
  CastleVectors, CastleTransform, CastleTimeUtils, CastleScene, CastleSceneManager,
  CastleTriangles,
  GameSpawned;

type
  THeightAboveTerrainEvent = function (Pos: TVector3; out Y: Single): boolean of object;

  TEnemy = class(TSpawned)
  private
    EnemyIdleTemplate: TCastleScene;
    FIdle: boolean;
  protected
    procedure SpawnEnded; override;
  public
    property Idle: boolean read FIdle;
    { Enemy was hit. The Point should be
      in local EnemyIdleTemplate scene coordinates. }
    procedure Hit(const Point: TVector3; const Triangle: TTriangle);
  end;

  { Enemies list. It is a TCastleTransform descendant, and it should
    also be added to the SceneManager.Items.
    It automatically takes care of spawning the enemies. }
  TEnemies = class(TCastleTransform)
  private
    EnemyLastSpawn: TTimerResult;
    EnemySpawnTemplate: TCastleScene;
    EnemyIdleTemplate: TCastleScene;
    procedure TryEnemySpawn;
  public
    SceneManager: TCastleSceneManager;
    OnHeightAboveTerrain: THeightAboveTerrainEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses Math,
  CastleFilesUtils, CastleUtils, CastleShapes, CastleLog;

{ TEnemy --------------------------------------------------------------------- }

procedure TEnemy.SpawnEnded;
begin
  { when the "spawn" animation finished,
    remove the animation, and add static scene (shared by all enemies)
    EnemyIdleTemplate. }

  Clear;
  Add(EnemyIdleTemplate);

  FIdle := true;
end;

procedure TEnemy.Hit(const Point: TVector3; const Triangle: TTriangle);
begin
  // TODO
  WritelnLog('Hit: ' + Triangle.ITexCoord2D(Point).ToString);
  Free;
end;

{ TEnemies ------------------------------------------------------------------- }

constructor TEnemies.Create(AOwner: TComponent);
begin
  inherited;

  { We have two separate enemy templates -- one for spawning (animated),
    one for standing (static, idle).
    That is bacause a scene loaded from castle-anim-frames
    does not have detailed collision information (instead it's always
    approximated using a bounding box, even the TCastleScene.Spatial
    includes ssDynamicCollisions).
    And we need detailed collisions on "idle" scene to determine where
    did we hit, and how to break apart the enemy on hit.

    Also, it is a bit more optimal:
    since we know EnemyIdleTemplate doesn't animate, we can simply place
    this template in multiple positions on the world,
    no need to do EnemyIdleTemplate.Clone. }

  EnemySpawnTemplate := TCastleScene.Create(Self);
  EnemySpawnTemplate.Name := 'EnemySpawn'; // for nicer debugging
  EnemySpawnTemplate.Load(ApplicationData('evil_squirrel/evil-squirrel-board.castle-anim-frames'));

  EnemyIdleTemplate := TCastleScene.Create(Self);
  EnemyIdleTemplate.Name := 'EnemyIdle'; // for nicer debugging
  EnemyIdleTemplate.Spatial := [ssStaticCollisions];
  EnemyIdleTemplate.Load(ApplicationData('evil_squirrel/evil-squirrel-board_idle.x3d'));

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
  Enemy: TEnemy;
  Pos, Dir: TVector3;
begin
  if FindSpawnPosition(Pos) then
  begin
    Enemy := TEnemy.Create(Self);
    Enemy.EnemyIdleTemplate := EnemyIdleTemplate;
    Enemy.Spawn(EnemySpawnTemplate);
    Enemy.Translation := Pos;

    { make the enemy face player }
    Dir := Pos - SceneManager.WalkCamera.Position;
    if not VectorsParallel(Dir, SceneManager.GravityUp) then
    begin
      MakeVectorsOrthoOnTheirPlane(Dir, SceneManager.GravityUp);
      Enemy.Direction := Dir;
    end;

    Add(Enemy);
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
