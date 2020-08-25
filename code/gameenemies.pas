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

  { Enemy object, instantiated only by the TEnemies.Update. }
  TEnemy = class(TSpawned)
  private
    const
      { These values reflect the position/size of the shooting target on
        ../data/evil_squirrel/evil-squirrel.png . }
      TargetCenter: TVector2 = (Data: (417 / 1024, 1 - 717 / 1024));
      TargetRadius = 194 / 1024;
      TargetBullseyeRadius = 10 / 1024;
    var
      EnemyIdleTemplate, EnemyDestroyedPartTemplate: TCastleScene;
      FIdle: boolean;
    { Place 3 splitted parts of the original enemy in the World. }
    procedure SplitIntoParts(const HitCoord: TVector2; const HitDirection: TVector3);
  protected
    procedure SpawnEnded; override;
  public
    const
      MaxScore = 10;
    property Idle: boolean read FIdle;
    { Enemy was hit. The Point should be
      in local EnemyIdleTemplate scene coordinates. }
    procedure Hit(const Point: TVector3; const Triangle: TTriangle; const HitDirection: TVector3);
    function HitScore(const Point: TVector3; const Triangle: TTriangle): Cardinal;
    function HitScore(const HitCoord: TVector2): Cardinal;
  end;

  { Enemies list. It is a TCastleTransform descendant, and it should
    also be added to the SceneManager.Items.
    It automatically takes care of spawning the enemies. }
  TEnemies = class(TCastleTransform)
  private
    EnemyLastSpawn: TTimerResult;
    EnemySpawnTemplate: TCastleScene;
    EnemyIdleTemplate: TCastleScene;
    EnemyDestroyedPartTemplate: TCastleScene;
    procedure TryEnemySpawn;
  public
    SceneManager: TCastleSceneManager;
    OnHeightAboveTerrain: THeightAboveTerrainEvent;
    constructor Create(AOwner: TComponent); override;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
    { Prepare resources using SceneManager, to render faster when the game starts. }
    procedure Prepare;
  end;

implementation

uses Math, SysUtils,
  CastleFilesUtils, CastleUtils, CastleShapes, CastleLog, X3DNodes, X3DFields,
  CastleSceneCore, CastleBoxes,
  GameText3D;

{ TEnemyDestroyedPart -------------------------------------------------------- }

type
  { Destroyed enemy part, instantiated only by TEnemy.Hit. }
  TEnemyDestroyedPart = class(TCastleTransform)
  strict private
    class var
      NameId: Int64;
    var
      LifeTime: TFloatTime;
  public
    { Create one (out of 3) destroyed enemy parts.
      The meaning of ClipPlaneAngles and PartIndex is the same
      as appropriate shader effect parameters, see
      ../data/evil_squirrel/evil-squirrel-destroyed-part.x3dv. }
    constructor Create(const Enemy: TEnemy; const HitCoord: TVector2;
      const ClipPlaneAngles: TVector3; const PartIndex: Integer;
      const HitDirection: TVector3); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

constructor TEnemyDestroyedPart.Create(const Enemy: TEnemy;
  const HitCoord: TVector2;
  const ClipPlaneAngles: TVector3; const PartIndex: Integer;
  const HitDirection: TVector3);
var
  Side, DirectionToBreakApart: TVector3;
  Scene: TCastleScene;
  ClipEffect: TEffectNode;
  Body: TRigidBody;
  Collider: TBoxCollider;
begin
  inherited Create(Enemy.World);

  Scene := Enemy.EnemyDestroyedPartTemplate.Clone(Self);
  // assigning Scene.Name is completely optional, it's only for debugging
  Scene.Name := 'EnemyDestroyedPart' + IntToStr(NameId);
  Inc(NameId);
  Scene.ProcessEvents := true;

  // set visual look of the scene (clip by shader effect)
  ClipEffect := Scene.Node('ClipEffect') as TEffectNode;
  (ClipEffect.Field('hitPoint') as TSFVec2f).Send(HitCoord);
  (ClipEffect.Field('clipPlaneAngles') as TSFVec3f).Send(ClipPlaneAngles);
  (ClipEffect.Field('part') as TSFInt32).Send(PartIndex);

  Side := TVector3.CrossProduct(Enemy.Direction, Enemy.Up);
  case PartIndex of
    0: DirectionToBreakApart := Side;
    1: DirectionToBreakApart := RotatePointAroundAxisDeg( 120, Side, Enemy.Direction);
    2: DirectionToBreakApart := RotatePointAroundAxisDeg(-120, Side, Enemy.Direction);
    else raise EInternalError.Create('TEnemyDestroyedPart.Create:PartIndex invalid');
  end;

  // set physics
  Body := TRigidBody.Create(Self);
  Body.Dynamic := true;
  Body.InitialLinearVelocity := DirectionToBreakApart * 5 + HitDirection * 7;

  Collider := TBoxCollider.Create(Body);
  Collider.Size := Enemy.LocalBoundingBox.Size;
  Collider.Restitution := 0.3;
  Collider.Density := 100.0;

  // necessary to make Collider centered around (0,0,0) work
  Scene.Translation := -Enemy.LocalBoundingBox.Center;

  Translation := Enemy.Translation + Enemy.LocalBoundingBox.Center;
  Rotation := Enemy.Rotation;
  RigidBody := Body;

  Add(Scene);
end;

procedure TEnemyDestroyedPart.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  FadeTime = 10;
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  if LifeTime > FadeTime then
    RemoveMe := rtRemoveAndFree;
end;

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

procedure TEnemy.Hit(const Point: TVector3; const Triangle: TTriangle; const HitDirection: TVector3);
var
  Text: TText3D;
  S: string;
  Score: Cardinal;
  HitCoord: TVector2;
begin
  HitCoord := Triangle.ITexCoord2D(Point);

  Score := HitScore(HitCoord);

  if Score = 0 then
    S := 'Hit, but no points.'
  else
  if Score = MaxScore then
    S := 'BULLSEYE! +10 POINTS!'
  else
    S := '+' + IntToStr(Score) + ' points!';

  Text := TText3D.Create(World, S);
  Text.Translation := BoundingBox.Center;
  World.Add(Text);

  SplitIntoParts(HitCoord, HitDirection);

  Free;
end;

function TEnemy.HitScore(const Point: TVector3; const Triangle: TTriangle): Cardinal;
begin
  Result := HitScore(Triangle.ITexCoord2D(Point));
end;

function TEnemy.HitScore(const HitCoord: TVector2): Cardinal;
var
  D: Single;
begin
  D := PointsDistance(HitCoord, TargetCenter);

  if D > TargetRadius then
    Result := 0
  else
  if D <= TargetBullseyeRadius then
    Result := MaxScore
  else
    Result := Round(MapRange(D, TargetBullseyeRadius, TargetRadius, MaxScore - 1, 1));
end;

procedure TEnemy.SplitIntoParts(const HitCoord: TVector2; const HitDirection: TVector3);

  (*
  procedure SortVector(var V: TVector3);
  var
    Min12: Integer;
  begin
    Min12 := Iff(V.Data[1] < V.Data[2], 1, 2);
    if V.Data[0] > V.Data[Min12] then
      SwapValues(V.Data[0], V.Data[Min12]);

    // now V.Data[0] is done, it is the smallest one
    Assert(V.Data[0] <= V.Data[1]);
    Assert(V.Data[0] <= V.Data[2]);

    OrderUp(V.Data[1], V.Data[2]);
  end;
  *)

var
  ClipPlaneAngles: TVector3;
  I: Integer;
  Part: TEnemyDestroyedPart;
begin
  // ClipPlaneAngles[0] := RandomFloatRange(-Pi, Pi);
  // ClipPlaneAngles[1] := RandomFloatRange(-Pi, Pi);
  // ClipPlaneAngles[2] := RandomFloatRange(-Pi, Pi);
  // SortVector(ClipPlaneAngles);

  { initially I experimented with more random ClipPlaneAngles,
    but the below approach looks better.
    Also, it doesn't require SortVector call. }
  ClipPlaneAngles[0] := RandomFloatRange(-Pi          , -Pi / 3 - 0.2);
  ClipPlaneAngles[1] := RandomFloatRange(-Pi / 3 + 0.2,  Pi / 3 - 0.2);
  ClipPlaneAngles[2] := RandomFloatRange( Pi / 3 + 0.2,  Pi);

  for I := 0 to 2 do
  begin
    Part := TEnemyDestroyedPart.Create(Self,
      HitCoord, ClipPlaneAngles, I, HitDirection);
    World.Add(Part);
  end;
end;

{ TEnemies ------------------------------------------------------------------- }

constructor TEnemies.Create(AOwner: TComponent);
begin
  inherited;

  { We have two separate enemy templates -- one for spawning (animated),
    one for standing (static, idle).
    That is bacause a scene loaded from castle-anim-frames
    does not have detailed collision information (instead it's always
    approximated using a bounding box, even when the TCastleScene.Spatial
    includes ssDynamicCollisions).
    And we need detailed collisions on "idle" scene to determine where
    did we hit, and how to break apart the enemy on hit.

    Also, it is a bit more optimal:
    since we know EnemyIdleTemplate doesn't animate, we can simply place
    this template in multiple positions on the world,
    no need to do EnemyIdleTemplate.Clone. }

  EnemySpawnTemplate := TCastleScene.Create(Self);
  EnemySpawnTemplate.Name := 'EnemySpawn'; // for nicer debugging
  EnemySpawnTemplate.Load('castle-data:/evil_squirrel/evil-squirrel-board.castle-anim-frames');

  EnemyIdleTemplate := TCastleScene.Create(Self);
  EnemyIdleTemplate.Name := 'EnemyIdle'; // for nicer debugging
  EnemyIdleTemplate.Spatial := [ssStaticCollisions];
  EnemyIdleTemplate.Load('castle-data:/evil_squirrel/evil-squirrel-board_idle.x3d');

  EnemyDestroyedPartTemplate := TCastleScene.Create(Self);
  EnemyDestroyedPartTemplate.Name := 'EnemyDestroyedPart'; // for nicer debugging
  EnemyDestroyedPartTemplate.Load('castle-data:/evil_squirrel/evil-squirrel-destroyed-part.x3dv');

  EnemyLastSpawn := Timer;
end;

procedure TEnemies.Prepare;
begin
  SceneManager.PrepareResources(EnemySpawnTemplate);
  SceneManager.PrepareResources(EnemyIdleTemplate);
  SceneManager.PrepareResources(EnemyDestroyedPartTemplate);
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
      Pos := SceneManager.WalkNavigation.Position;
      Dir := SceneManager.WalkNavigation.DirectionInGravityPlane;
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
    Enemy.EnemyDestroyedPartTemplate := EnemyDestroyedPartTemplate;
    Enemy.Spawn(EnemySpawnTemplate);
    Enemy.Translation := Pos;

    { make the enemy face player }
    Dir := SceneManager.WalkNavigation.Position - Pos;
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
  EnemySpawnDelay = 4.0;
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
