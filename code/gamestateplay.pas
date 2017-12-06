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
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications,
  GameTerrain;

type
  TStatePlay = class(TUIState)
  strict private
    Status: TCastleLabel;
    OnScreenMenu: TCastleOnScreenMenu;
    Terrain: TTerrain;
    Notifications: TCastleNotifications;
    SceneManager: TCastleSceneManager;
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

uses GameStateMainMenu;

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

  Terrain := TTerrain.Create(FreeAtStop, InitialGridCount, SceneManager);
  Terrain.CreateScene;
  Terrain.AddSlidersToMenu(OnScreenMenu);

  SceneManager.WalkCamera.SetView(
    { Initially, stand in the middle.
      This is not really necessary -- the CreateScene already called UpdateScene
      that already called FixCamera, and moved it to the middle (with message
      "Camera stands outside of terrain, fixing" in log).
      But doing it explicitly feels cleaner. }
    SceneManager.Items.BoundingBox.Center,
    // look in the direction that shrinks / grows when you change GridCount
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  Terrain.FixCamera; // fix SceneManager.WalkCamera.Position.Y
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
