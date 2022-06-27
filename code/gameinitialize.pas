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

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleUIState, CastleViewport,
  CastleTransform, CastleVectors, CastleImages, CastleApplicationProperties,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameStatePlay
  , GameStateLoading
  , GameStateMainMenu
  {$endregion 'Castle Initialization Uses'};

{ application routines ------------------------------------------------------- }

var
  Window: TCastleWindow;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Adjust container settings for a scalable UI (adjusts to any window size in a smart way). }
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  { customize TCastleLabel with TCastleLabel.Frame=true look }
  Theme.ImagesPersistent[tiLabel].Url := 'castle-data:/gui/transparent_pixel.png';
  Theme.ImagesPersistent[tiLabel].ProtectedSides.AllSides := 0;

  { Create game states and set initial state }
  {$region 'Castle State Creation'}
  // The content here may be automatically updated by CGE editor.
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateLoading := TStateLoading.Create(Application);
  {$endregion 'Castle State Creation'}

  { set current state }
  TUIState.Current := StateMainMenu;
end;

initialization
  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;
end.
