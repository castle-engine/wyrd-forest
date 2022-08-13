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

{ TStatePlay, responsible for playing the game. }
unit GameStatePlay;

interface

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleUIState, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, CastleRenderOptions,
  CastleTransform, CastleVectors, CastleTriangles, CastleTimeUtils,
  CastleUtils, CastleBoxes, CastleNotifications,
  GameTerrain, GameEnemies;

type
  TTutorialState = (tsShootEnemy, tsPlantTree, tsFinished);

  { Play the game, instantiating terrain, trees, shooting targets and so on. }
  TStatePlay = class(TUIState)
  strict private
    Status: TCastleLabel;
    MyTerrain: TMyTerrain;
    Notifications: TCastleNotifications;
    Viewport: TCastleViewport;
    Navigation: TCastleWalkNavigation;
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
    { Fix Viewport camera position to stand on ground }
    procedure FixCamera;
    { Determine Enemy and exact hit point and triangle,
      looking at Viewport.MouseRayHit. }
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

  Viewport := TCastleViewport.Create(FreeAtStop);
  Viewport.FullSize := true;
  InsertBack(Viewport);

  Navigation := TCastleWalkNavigation.Create(FreeAtStop);
  Navigation.PreferredHeight := 2;
  Navigation.MoveSpeed := 10;
  Viewport.InsertFront(Navigation);

  EnvironmentScene := TCastleScene.Create(FreeAtStop);
  EnvironmentScene.Load('castle-data:/environment/environment.x3dv');
  EnvironmentScene.ProcessEvents := true;
  Viewport.Items.Add(EnvironmentScene);
  Viewport.Items.MainScene := EnvironmentScene;

  Enemies := TEnemies.Create(FreeAtStop);
  Enemies.Viewport := Viewport;
  Enemies.Navigation := Navigation;
  Enemies.OnHeightAboveTerrain := @HeightAboveTerrain;
  Enemies.Prepare;
  Viewport.Items.Add(Enemies);

  TreeTemplate := TCastleScene.Create(FreeAtStop);
  TreeTemplate.Name := 'Tree'; // for nicer debugging
  TreeTemplate.Load('castle-data:/tree/oaktree_with_good_collisions.x3dv');
  { Prepare resources, to render faster when the game starts. }
  Viewport.PrepareResources(TreeTemplate);

  Trees := TCastleTransform.Create(FreeAtStop);
  Viewport.Items.Add(Trees);

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

  MyTerrain := TMyTerrain.Create(FreeAtStop);
  MyTerrain.Viewport := Viewport;
  MyTerrain.OnFixCamera := @FixCamera;
  MyTerrain.InitialGridCount := InitialGridCount;
  MyTerrain.Initialize;

  Viewport.Camera.SetView(
    { Initially, stand in the middle. }
    Viewport.Items.BoundingBox.Center,
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  FixCamera; // fix Viewport.WalkCamera.Position.Y
end;

function TStatePlay.HeightAboveTerrain(Pos: TVector3; out Y: Single): boolean;
var
  RayCollision: TRayCollision;
begin
  Pos.Y := 1000 * 1000;
  RayCollision := Viewport.Items.WorldRay(Pos, Vector3(0, -1, 0));
  try
    Result :=
      (RayCollision <> nil) and
      (RayCollision.Count >= 2) and
      (RayCollision[1].Item = MyTerrain.Terrain);
    if Result then
      Y := RayCollision[0].Point.Y;
  finally FreeAndNil(RayCollision) end;
end;

procedure TStatePlay.FixCamera;
var
  P: TVector3;
  Y: Single;
begin
  P := Viewport.Camera.Translation;
  if HeightAboveTerrain(P, Y) then
  begin
    P.Y := Y + 2;
    Viewport.Camera.Translation := P;
  end else
  begin
    WritelnLog('Camera stands outside of terrain, fixing');
    Viewport.Camera.Translation := Viewport.Items.BoundingBox.Center;
    FixCamera;
  end;
end;

function TStatePlay.EnemyUnderMouse(
  out Enemy: TEnemy; out Point: TVector3; out Triangle: PTriangle): boolean;
var
  EnemyIndex: Integer;
begin
  Result := false;

  if Viewport.MouseRayHit <> nil then
  begin
    EnemyIndex := Viewport.MouseRayHit.IndexOfItem(TEnemy);
    if EnemyIndex <> -1 then
    begin
      Enemy := Viewport.MouseRayHit[EnemyIndex].Item as TEnemy;
      Point := Viewport.MouseRayHit[0].Point;
      Triangle := Viewport.MouseRayHit[0].Triangle;
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
    'FPS: %s' +NL+
    'Move speed (change by [-] [+]): %f' + NL +
    'Hit points under mouse: %d' + NL +
    '[AWSD] or arrow keys to move' + NL +
    '[left click] shoot evil squirrel' + NL +
    '[right click] plant tree' + NL +
    '[ctrl + right click] shoot test physics box' + NL +
    '[F4] Toggle mouse look' +NL+
    '[F5] Screenshot' +NL+
    '[Escape] Back to main menu',
    [Container.Fps.ToString,
     Navigation.MoveSpeed,
     HitUnderMouse]);
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;

  procedure HitNotification;
  var
    ItemHit: TCastleTransform;
  begin
    if Viewport.MouseRayHit = nil then
      Notifications.Show('Nothing hit')
    else
    begin
      ItemHit := Viewport.MouseRayHit[0].Item;
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
    Material: TMaterialNode;
  begin
    Box := TBoxNode.CreateWithShape(Shape);
    Box.Size := Vector3(0.5, 0.5, 0.5);

    Material := TMaterialNode.Create;
    Material.DiffuseColor := Vector3(0.5, 0.5, 1.0);
    Shape.Material := Material;

    Root := TX3DRootNode.Create;
    Root.AddChildren(Shape);

    Scene := TCastleScene.Create(Viewport);
    Scene.Load(Root, true);

    Viewport.Camera.GetView(Pos, Dir, Up);
    Scene.Translation := Pos + Dir * 2;
    Scene.Direction := Dir;

    Body := TRigidBody.Create(Self);
    Body.LinearVelocity := Dir * 4;
    Body.Dynamic := true;

    Collider := TBoxCollider.Create(Body);
    Collider.Size := Box.Size;
    Collider.Restitution := 0.3;
    Collider.Density := 100.0;
    Scene.RigidBody := Body;

    Viewport.Items.Add(Scene);
  end;

  function TrySpawnTree: boolean;
  var
    Tree: TSpawned;
    Pos: TVector3;
  begin
    Result := false;

    if (Viewport.MouseRayHit <> nil) and
       (Viewport.MouseRayHit.Count >= 2) and
       (Viewport.MouseRayHit[1].Item = MyTerrain.Terrain) then
    begin
      Result := true;

      Pos := Viewport.MouseRayHit[0].Point;
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
      Enemy.Hit(Point, Triangle^, Viewport.Camera.Direction);

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

  if Event.IsMouseButton(buttonLeft) then
  begin
    if not TryShootEnemy then HitNotification;
    Result := true;
  end;

  if Event.IsMouseButton(buttonRight) then
  begin
    if Container.Pressed.Keys[keyCtrl] then
    begin
      SpawnPhysicsBox;
    end else
    begin
      if not TrySpawnTree then HitNotification;
    end;
    Result := true;
  end;

  if Event.IsKey(keyF4) then
  begin
    Navigation.MouseLook := not Navigation.MouseLook;
    Crosshair.Exists := Navigation.MouseLook;
    Result := true;
  end;

  if Event.IsKey(keyF5) then
  begin
    S := Container.SaveScreenToDefaultFile;
    Notifications.Show('Saved screen to ' + S);
    Result := true;
  end;

  if Event.IsKey(keyEscape) then
  begin
    TUIState.Current := StateMainMenu;
    Result := true;
  end;
end;

end.
