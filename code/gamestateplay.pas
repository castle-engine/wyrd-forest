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
  CastleTransform, CastleVectors, CastleTriangles, CastleTimeUtils,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications,
  GameTerrain, GameEnemies;

type
  TTutorialState = (tsShootEnemy, tsPlantTree, tsFinished);

  { Play the game, instantiating terrain, trees, shooting targets and so on. }
  TStatePlay = class(TUIState)
  strict private
    Status: TCastleLabel;
    OnScreenMenu: TCastleOnScreenMenu;
    Terrain: TTerrain;
    Notifications: TCastleNotifications;
    SceneManager: TCastleSceneManager;
    TreeTemplate: TCastleScene;
    Trees: TCastleTransform;
    Enemies: TEnemies;
    Crosshair: TCastleCrosshair;
    TutorialLabel: TCastleLabel;
    TutorialState: TTutorialState;

    { Height (Y) at the given Position of the terrain.
      Only Pos.X, Pos.Z matter, input Pos.Y is ignored.
      Returns @false is this is not over terrain (maybe outside terrain,
      maybe over another tree or enemy). }
    function HeightAboveTerrain(Pos: TVector3; out Y: Single): boolean;
    { Fix SceneManager camera position to stand on ground }
    procedure FixCamera;
    { Determine Enemy and exact hit point and triangle,
      looking at SceneManager.MouseRayHit. }
    function EnemyUnderMouse(
      out Enemy: TEnemy; out Point: TVector3; out Triangle: PTriangle): boolean;
  public
    InitialGridCount: Cardinal;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StatePlay: TStatePlay;

implementation

uses GameStateMainMenu, GameSpawned;

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
  SceneManager.WalkCamera.MoveSpeed := 10;

  EnvironmentScene := TCastleScene.Create(FreeAtStop);
  EnvironmentScene.Load(ApplicationData('environment/environment.x3dv'));
  EnvironmentScene.ProcessEvents := true;
  SceneManager.Items.Add(EnvironmentScene);
  SceneManager.MainScene := EnvironmentScene;

  Enemies := TEnemies.Create(FreeAtStop);
  Enemies.SceneManager := SceneManager;
  Enemies.OnHeightAboveTerrain := @HeightAboveTerrain;
  SceneManager.Items.Add(Enemies);

  TreeTemplate := TCastleScene.Create(FreeAtStop);
  TreeTemplate.Name := 'Tree'; // for nicer debugging
  TreeTemplate.Load(ApplicationData('tree/oaktree_with_good_collisions.x3dv'));

  Trees := TCastleTransform.Create(FreeAtStop);
  SceneManager.Items.Add(Trees);

  OnScreenMenu := TCastleOnScreenMenu.Create(FreeAtStop);
  OnScreenMenu.Exists := false;
  OnScreenMenu.Anchor(hpRight, -10);
  OnScreenMenu.Anchor(vpBottom, 10);
  OnScreenMenu.RegularSpaceBetweenItems := 4;
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

  TutorialLabel := TCastleLabel.Create(FreeAtStop);
  TutorialLabel.Anchor(hpMiddle);
  TutorialLabel.Anchor(vpMiddle);
  TutorialLabel.FontSize := 30;
  TutorialLabel.Color := White;
  TutorialLabel.Frame := true;
  TutorialLabel.Padding := 20;
  TutorialLabel.Caption :=
    'Move with [AWSD] keys.' + NL +
    'Toggle mouse look with [F4].' + NL +
    'Press [left mouse button] to shoot the evil squirrel.';
  InsertFront(TutorialLabel);

  TutorialState := tsShootEnemy;

  Crosshair := TCastleCrosshair.Create(Owner);
  Crosshair.Exists := false; // synchronized with Camera.MouseLook
  InsertFront(Crosshair);

  Terrain := TTerrain.Create(FreeAtStop, InitialGridCount);
  Terrain.SceneManager := SceneManager;
  Terrain.OnFixCamera := @FixCamera;
  Terrain.CreateScene;
  Terrain.AddSlidersToMenu(OnScreenMenu);

  SceneManager.WalkCamera.SetView(
    { Experimentally chosen sensible default position: }
    Vector3(20.51, 0, 12.68),

    { Alternative idea: Initially, stand in the middle.
      Note that the CreateScene would call FixCamera,
      and fix camera to stand in the middle anyway
      (with message "Camera stands outside of terrain, fixing" in log).
      But doing it explicitly feels cleaner. }
    //SceneManager.Items.BoundingBox.Center,

    // Look in the direction that shrinks / grows when you change GridCount.
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  FixCamera; // fix SceneManager.WalkCamera.Position.Y

  // disable dragging, sometimes collides with OnScreenMenu usage
  SceneManager.WalkCamera.Input := SceneManager.WalkCamera.Input - [ciMouseDragging];
end;

function TStatePlay.HeightAboveTerrain(Pos: TVector3; out Y: Single): boolean;
var
  RayCollision: TRayCollision;
begin
  Pos.Y := 1000 * 1000;
  RayCollision := SceneManager.Items.WorldRay(Pos, Vector3(0, -1, 0));
  try
    Result := (RayCollision <> nil) and (RayCollision[0].Item = Terrain.Scene);
    if Result then
      Y := RayCollision[0].Point.Y;
  finally FreeAndNil(RayCollision) end;
end;

procedure TStatePlay.FixCamera;
var
  P: TVector3;
  Y: Single;
begin
  P := SceneManager.WalkCamera.Position;
  if HeightAboveTerrain(P, Y) then
  begin
    P.Y := Y + 2;
    SceneManager.WalkCamera.Position := P;
  end else
  begin
    WritelnLog('Camera stands outside of terrain, fixing');
    SceneManager.WalkCamera.Position := SceneManager.Items.BoundingBox.Center;
    FixCamera;
  end;
end;

function TStatePlay.EnemyUnderMouse(
  out Enemy: TEnemy; out Point: TVector3; out Triangle: PTriangle): boolean;
var
  EnemyIndex: Integer;
begin
  Result := false;

  if SceneManager.MouseRayHit <> nil then
  begin
    EnemyIndex := SceneManager.MouseRayHit.IndexOfItem(TEnemy);
    if EnemyIndex <> -1 then
    begin
      Enemy := SceneManager.MouseRayHit[EnemyIndex].Item as TEnemy;
      Point := SceneManager.MouseRayHit[0].Point;
      Triangle := SceneManager.MouseRayHit[0].Triangle;
      if Enemy.Idle and (Triangle <> nil) then
        Result := true;
    end;
  end;
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: boolean);

  function HitUnderMouse: Cardinal;
  var
    Enemy: TEnemy;
    Point: TVector3;
    Triangle: PTriangle;
  begin
    if EnemyUnderMouse(Enemy, Point, Triangle) then
      Result := Enemy.HitScore(Point, Triangle^)
    else
      Result := 0;
  end;

begin
  Status.Caption := Format(
    'FPS: %f' +NL+
    'Move speed (change by [-] [+]): %f' + NL +
    'Hit points under mouse: %d' + NL +
    '[AWSD] or arrow keys to move' + NL +
    '[left click] shoot evil squirrel' + NL +
    '[right click] plant tree' + NL +
    '[ctrl + right click] shoot test physics box' + NL +
    '[F4] Toggle mouse look' +NL+
    '[F5] Screenshot' +NL+
    '[F6] Save terrain to X3D file' +NL+
    '[F10] Toggle controls to tweak display' +NL+
    '[Escape] Back to main menu',
    [Container.Fps.RealTime,
     SceneManager.WalkCamera.MoveSpeed,
     HitUnderMouse]);
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;

  procedure HitNotification;
  var
    ItemHit: TCastleTransform;
  begin
    if SceneManager.MouseRayHit = nil then
      Notifications.Show('Nothing hit')
    else
    begin
      ItemHit := SceneManager.MouseRayHit[0].Item;
      Notifications.Show('Hit ' + ItemHit.Name + ' ' + ItemHit.ClassName);
    end;
  end;

  procedure SpawnPhysicsBox;
  var
    Box: TBoxNode;
    Shape: TShapeNode;
    Root: TX3DRootNode;
    Scene: TCastleScene;
    Body: TRigidBody;
    Collider: TBoxCollider;
    Pos, Dir, Up: TVector3;
  begin
    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(0.5, 0.5, 0.5);

    Shape.Material := TMaterialNode.Create;
    Shape.Material.DiffuseColor := Vector3(0.5, 0.5, 1.0);

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    Scene := TCastleScene.Create(SceneManager);
    Scene.Load(Root, true);

    SceneManager.Camera.GetView(Pos, Dir, Up);
    Scene.Translation := Pos + Dir * 2;
    Scene.Direction := Dir;

    Body := TRigidBody.Create(Self);
    Body.InitialLinearVelocity := Dir * 4;
    Body.Dynamic := true;

    Collider := TBoxCollider.Create(Body);
    Collider.Size := Box.Size;
    Collider.Restitution := 0.3;
    Collider.Density := 100.0;
    Scene.RigidBody := Body;

    SceneManager.Items.Add(Scene);
  end;

  function TrySpawnTree: boolean;
  var
    Tree: TSpawned;
    Pos: TVector3;
  begin
    Result := false;

    if (SceneManager.MouseRayHit <> nil) and
       (SceneManager.MouseRayHit[0].Item = Terrain.Scene) then
    begin
      Result := true;

      Pos := SceneManager.MouseRayHit[0].Point;
      Tree := TSpawned.Create(Self);
      Tree.Spawn(TreeTemplate);
      Tree.Translation := Pos;
      Trees.Add(Tree);

      { advance tutorial }
      if TutorialState = tsPlantTree then
      begin
        TutorialLabel.Exists := false;
        Inc(TutorialState);
      end;
    end;
  end;

  function TryShootEnemy: boolean;
  var
    Enemy: TEnemy;
    Point: TVector3;
    Triangle: PTriangle;
  begin
    Result := EnemyUnderMouse(Enemy, Point, Triangle);
    if Result then
    begin
      Enemy.Hit(Point, Triangle^, SceneManager.WalkCamera.Direction);

      { advance tutorial }
      if TutorialState = tsShootEnemy then
      begin
        TutorialLabel.Caption := 'Press [right mouse button] to plant a tree.';
        Inc(TutorialState);
      end;
    end;
  end;

var
  S: string;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsMouseButton(mbLeft) then
  begin
    if not TryShootEnemy then HitNotification;
    Result := true;
  end;

  if Event.IsMouseButton(mbRight) then
  begin
    if Container.Pressed.Keys[K_Ctrl] then
    begin
      SpawnPhysicsBox;
    end else
    begin
      if not TrySpawnTree then HitNotification;
    end;
    Result := true;
  end;

  if Event.IsKey(K_F4) then
  begin
    SceneManager.WalkCamera.MouseLook := not SceneManager.WalkCamera.MouseLook;
    Crosshair.Exists := SceneManager.WalkCamera.MouseLook;
    Result := true;
  end;

  if Event.IsKey(K_F5) then
  begin
    S := FileNameAutoInc('wyrd_forest_screen_%d.png');
    Container.SaveScreen(S);
    Notifications.Show('Saved screen to ' + S);
    Result := true;
  end;

  if Event.IsKey(K_F6) then
  begin
    S := FileNameAutoInc('terrain_%d.x3dv');
    Terrain.Scene.Save(S);
    Notifications.Show('Saved terrain 3D model to ' + S);
    Result := true;
  end;

  if Event.IsKey(K_F10) then
  begin
    OnScreenMenu.Exists := not OnScreenMenu.Exists;
    Result := true;
  end;

  if Event.IsKey(K_Escape) then
  begin
    TUIState.Current := StateMainMenu;
    Result := true;
  end;
end;

end.
