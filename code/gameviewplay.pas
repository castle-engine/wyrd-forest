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

{ TViewPlay, responsible for playing the game. }
unit GameViewPlay;

interface

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleViewport,
  CastleCameras, X3DNodes, X3DFields, CastleRenderOptions,
  CastleTransform, CastleVectors, CastleTriangles, CastleTimeUtils,
  CastleUtils, CastleBoxes, CastleNotifications,
  GameTerrain, GameEnemies;

type
  TTutorialState = (tsShootEnemy, tsPlantTree, tsFinished);

  { Play the game, instantiating terrain, trees, shooting targets and so on. }
  TViewPlay = class(TCastleView)
  strict private
    { Components designed using CGE editor, loaded from gameviewmain.castle-user-interface. }
    MainViewport: TCastleViewport;
    MainNavigation: TCastleWalkNavigation;

    Status: TCastleLabel;
    MyTerrain: TMyTerrain;
    Notifications: TCastleNotifications;
    TreeTemplate: TCastleScene;
    Trees: TCastleTransform;
    Enemies: TEnemies;
    Crosshair: TCastleCrosshair;
    TutorialLabel: TCastleLabel;
    RectTutorial: TCastleRectangleControl;
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
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewPlay: TViewPlay;

implementation

uses GameViewMainMenu, GameSpawned;

{ TViewPlay ----------------------------------------------------------------- }

constructor TViewPlay.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewplay.castle-user-interface';
end;

procedure TViewPlay.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  MainViewport := DesignedComponent('MainViewport') as TCastleViewport;
  MainNavigation := DesignedComponent('MainNavigation') as TCastleWalkNavigation;

  Enemies := TEnemies.Create(FreeAtStop);
  Enemies.Viewport := MainViewport;
  Enemies.Navigation := MainNavigation;
  Enemies.OnHeightAboveTerrain := @HeightAboveTerrain;
  Enemies.Prepare;
  MainViewport.Items.Add(Enemies);

  TreeTemplate := TCastleScene.Create(FreeAtStop);
  TreeTemplate.Name := 'Tree'; // for nicer debugging
  TreeTemplate.Load('castle-data:/tree/oaktree_with_good_collisions.x3dv');
  { Prepare resources, to render faster when the game starts. }
  MainViewport.PrepareResources(TreeTemplate);

  Trees := TCastleTransform.Create(FreeAtStop);
  MainViewport.Items.Add(Trees);

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

  RectTutorial := TCastleRectangleControl.Create(FreeAtStop);
  RectTutorial.AutoSizeToChildren := true;
  RectTutorial.Color := Vector4(0, 0, 0, 0.5);
  RectTutorial.Anchor(hpMiddle);
  RectTutorial.Anchor(vpMiddle);
  InsertFront(RectTutorial);

  TutorialLabel := TCastleLabel.Create(FreeAtStop);
  TutorialLabel.Anchor(hpMiddle);
  TutorialLabel.Anchor(vpMiddle);
  TutorialLabel.FontSize := 30;
  TutorialLabel.Color := White;
  TutorialLabel.Padding := 20;
  TutorialLabel.Caption :=
    'Move with [AWSD] keys.' + NL +
    'Toggle mouse look with [F4].' + NL +
    'Press [left mouse button] to shoot the evil squirrel.';
  RectTutorial.InsertFront(TutorialLabel);

  TutorialState := tsShootEnemy;

  Crosshair := TCastleCrosshair.Create(Owner);
  Crosshair.Exists := false; // synchronized with Camera.MouseLook
  InsertFront(Crosshair);

  MyTerrain := TMyTerrain.Create(FreeAtStop);
  MyTerrain.Viewport := MainViewport;
  MyTerrain.OnFixCamera := @FixCamera;
  MyTerrain.InitialGridCount := InitialGridCount;
  MyTerrain.Initialize;

  MainViewport.Camera.SetView(
    { Initially, stand in the middle. }
    MainViewport.Items.BoundingBox.Center,
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  FixCamera; // fix MainViewport.WalkCamera.Position.Y
end;

function TViewPlay.HeightAboveTerrain(Pos: TVector3; out Y: Single): boolean;
var
  RayCollision: TRayCollision;
begin
  Pos.Y := 1000 * 1000;
  RayCollision := MainViewport.Items.WorldRay(Pos, Vector3(0, -1, 0));
  try
    Result :=
      (RayCollision <> nil) and
      (RayCollision.Count >= 2) and
      (RayCollision[1].Item = MyTerrain.Terrain);
    if Result then
      Y := RayCollision[0].Point.Y;
  finally FreeAndNil(RayCollision) end;
end;

procedure TViewPlay.FixCamera;
var
  P: TVector3;
  Y: Single;
begin
  P := MainViewport.Camera.Translation;
  if HeightAboveTerrain(P, Y) then
  begin
    P.Y := Y + 2;
    MainViewport.Camera.Translation := P;
  end else
  begin
    WritelnLog('Camera stands outside of terrain, fixing');
    MainViewport.Camera.Translation := MainViewport.Items.BoundingBox.Center;
    FixCamera;
  end;
end;

function TViewPlay.EnemyUnderMouse(
  out Enemy: TEnemy; out Point: TVector3; out Triangle: PTriangle): boolean;
var
  EnemyIndex: Integer;
begin
  Result := false;

  if MainViewport.MouseRayHit <> nil then
  begin
    EnemyIndex := MainViewport.MouseRayHit.IndexOfItem(TEnemy);
    if EnemyIndex <> -1 then
    begin
      Enemy := MainViewport.MouseRayHit[EnemyIndex].Item as TEnemy;
      Point := MainViewport.MouseRayHit[0].Point;
      Triangle := MainViewport.MouseRayHit[0].Triangle;
      if Enemy.Idle and (Triangle <> nil) then
        Result := true;
    end;
  end;
end;

procedure TViewPlay.Update(const SecondsPassed: Single; var HandleInput: boolean);

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
     MainNavigation.MoveSpeed,
     HitUnderMouse]);
end;

function TViewPlay.Press(const Event: TInputPressRelease): boolean;

  procedure HitNotification;
  var
    ItemHit: TCastleTransform;
  begin
    if MainViewport.MouseRayHit = nil then
      Notifications.Show('Nothing hit')
    else
    begin
      ItemHit := MainViewport.MouseRayHit[0].Item;
      Notifications.Show('Hit ' + ItemHit.Name + ' ' + ItemHit.ClassName);
    end;
  end;

  procedure SpawnPhysicsBox;
  var
    Box: TCastleBox;
    Body: TCastleRigidBody;
    Collider: TCastleBoxCollider;
    Pos, Dir, Up: TVector3;
  begin
    Box := TCastleBox.Create(MainViewport);
    Box.Size := Vector3(0.5, 0.5, 0.5);
    Box.Material := pmPhong;
    Box.Color := Vector4(0.5, 0.5, 1.0, 1.0);

    MainViewport.Camera.GetView(Pos, Dir, Up);
    Box.Translation := Pos + Dir * 2;
    Box.Direction := Dir;

    Body := TCastleRigidBody.Create(Self);
    Body.LinearVelocity := Dir * 4;
    Body.Dynamic := true;
    Box.AddBehavior(Body);

    Collider := TCastleBoxCollider.Create(Self);
    Collider.Restitution := 0.3;
    Collider.Density := 100.0;
    Box.AddBehavior(Collider);

    MainViewport.Items.Add(Box);
  end;

  function TrySpawnTree: boolean;
  var
    Tree: TSpawned;
    Pos: TVector3;
  begin
    Result := false;

    if (MainViewport.MouseRayHit <> nil) and
       (MainViewport.MouseRayHit.Count >= 2) and
       (MainViewport.MouseRayHit[1].Item = MyTerrain.Terrain) then
    begin
      Result := true;

      Pos := MainViewport.MouseRayHit[0].Point;
      Tree := TSpawned.Create(Self);
      Tree.Spawn(TreeTemplate);
      Tree.Translation := Pos;
      Trees.Add(Tree);

      { advance tutorial }
      if TutorialState = tsPlantTree then
      begin
        RectTutorial.Exists := false;
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
      Enemy.Hit(Point, Triangle^, MainViewport.Camera.Direction);

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
    MainNavigation.MouseLook := not MainNavigation.MouseLook;
    Crosshair.Exists := MainNavigation.MouseLook;
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
    Container.View := ViewMainMenu;
    Result := true;
  end;
end;

end.
