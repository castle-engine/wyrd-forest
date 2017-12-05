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

{ Game initialization and logic. }
unit Game;

interface

implementation

uses SysUtils, Classes,
  CastleWindowTouch, CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleTerrain, CastleUIState, CastleSceneManager,
  CastleCameras, X3DNodes, CastleTransform, CastleVectors, CastleTriangles,
  CastleOnScreenMenu, CastleUtils, CastleBoxes;

var
  SceneManager: TCastleSceneManager;

{ TTerrain ------------------------------------------------------------------- }

type
  TTerrain = class(TComponent)
  strict private
    Scene: TCastleScene;

    { Configure terrain noise. }
    Divisions: Cardinal;
    GridSize: Single;
    Octaves: Single;
    Smoothness: Single;
    Amplitude: Single;
    Frequency: Single;
    Heterogeneous: Single;
    procedure UpdateScene(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure AddSlidersToMenu(OnScreenMenu: TCastleOnScreenMenu);
    procedure CreateScene;
  end;

constructor TTerrain.Create(AOwner: TComponent);
begin
  inherited;

  Divisions := 150;
  GridSize := 0.57;
  Octaves := 6.94;
  Smoothness := 1.9;
  Amplitude := 7.85;
  Frequency := 0.04;
  Heterogeneous := 0.64;
end;

procedure TTerrain.CreateScene;
begin
  Scene := TCastleScene.Create(Self);
  { no need for ssDynamicCollisions -- scene is static }
  Scene.Spatial := [ssRendering, ssStaticCollisions];
  { no need for ProcessEvents, as this scene is static }
  // Scene.ProcessEvents := true;
  SceneManager.Items.Add(Scene);
  SceneManager.MainScene := Scene;

  UpdateScene(nil);
end;

procedure TTerrain.AddSlidersToMenu(OnScreenMenu: TCastleOnScreenMenu);

  procedure IntSlider(const Name: string; const ValuePointer: PInteger; const Min, Max: Integer);
  begin
    OnScreenMenu.Add(Name, TCastleIntegerSlider.Create(Self,
      ValuePointer, Min, Max, @UpdateScene));
  end;

  procedure FloatSlider(const Name: string; const ValuePointer: PSingle; const Min, Max: Single);
  begin
    OnScreenMenu.Add(Name, TCastleFloatSlider.Create(Self,
      ValuePointer, Min, Max, @UpdateScene));
  end;

begin
  IntSlider('Divisions', @Divisions, 10, 200);
  FloatSlider('Grid Size', @GridSize, 0.1, 2);
  FloatSlider('Octaves', @Octaves, 0, 20);
  FloatSlider('Smoothness', @Smoothness, 1, 10);
  FloatSlider('Amplitude', @Amplitude, 0.1, 10);
  FloatSlider('Frequency', @Frequency, 0.001, 0.1);
  FloatSlider('Heterogeneous', @Heterogeneous, 0, 2);
end;

procedure TTerrain.UpdateScene(Sender: TObject);

  { fix SceneManager camera position to stand on ground }
  procedure FixCamera;
  var
    RayCollision: TRayCollision;
    P: TVector3;
  begin
    P := SceneManager.WalkCamera.Position;
    RayCollision := SceneManager.Items.WorldRay(
      Vector3(P.X, 1000 * 1000, P.Z), Vector3(0, -1, 0));
    try
      if RayCollision <> nil then
        P.Y := RayCollision[0].Point.Y + 2;
    finally FreeAndNil(RayCollision) end;
    SceneManager.WalkCamera.Position := P;
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

type
  TStatePlay = class(TUIState)
  strict private
    Status: TCastleLabel;
    OnScreenMenu: TCastleOnScreenMenu;
    Terrain: TTerrain;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

procedure TStatePlay.Start;
begin
  inherited;

  { Show a label with FPS }
  Status := TCastleLabel.Create(FreeAtStop);
  Status.Anchor(vpTop, -10);
  Status.Anchor(hpRight, -10);
  Status.Color := Yellow; // you could use "Vector4(1, 1, 0, 1)" instead of Yellow
  InsertFront(Status);

  SceneManager.NavigationType := ntWalk;
  SceneManager.WalkCamera.PreferredHeight := 2;
  SceneManager.WalkCamera.SetView(
    Vector3(1, 20, 1), // Vector3(50, 0, 50);
    // look in the direction that shrinks / grows when you change Divisions
    Vector3(1, 0, 1),
    Vector3(0, 1, 0)
  );
  SceneManager.WalkCamera.MoveSpeed := 10;

  OnScreenMenu := TCastleOnScreenMenu.Create(FreeAtStop);
  OnScreenMenu.Anchor(hpRight, -10);
  OnScreenMenu.Anchor(vpBottom, 10);
  InsertFront(OnScreenMenu);

  Terrain := TTerrain.Create(FreeAtStop);
  Terrain.CreateScene;
  Terrain.AddSlidersToMenu(OnScreenMenu);
end;

procedure TStatePlay.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  Status.Caption := Format(
    'FPS: %f' +NL+
    'Move speed (change by [-] [+]): %f' + NL +
    'Toggle mouse look with [F4]',
    [Container.Fps.RealTime,
     SceneManager.WalkCamera.MoveSpeed]);
end;

function TStatePlay.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_F4) then
    SceneManager.WalkCamera.MouseLook := not SceneManager.WalkCamera.MouseLook;
end;

{ application routines ------------------------------------------------------- }

var
  Window: TCastleWindowTouch;
  StatePlay: TStatePlay;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { use the default full-screen Window.SceneManager for this simple game }
  SceneManager := Window.SceneManager;

  StatePlay := TStatePlay.Create(Application);
  TUIState.Current := StatePlay;
end;

function MyGetApplicationName: string;
begin
  Result := 'wyrd-forest';
end;

initialization
  { This sets SysUtils.ApplicationName.
    It is useful to make sure it is correct (as early as possible)
    as our log routines use it. }
  OnGetApplicationName := @MyGetApplicationName;

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowTouch.Create(Application);
  Application.MainWindow := Window;
end.
