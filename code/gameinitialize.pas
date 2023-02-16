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

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleViewport,
  CastleTransform, CastleVectors, CastleImages, CastleApplicationProperties,
  CastleUtils, CastleBoxes, CastleNotifications
  {$region 'Castle Initialization Uses'}
  // The content here may be automatically updated by CGE editor.
  , GameViewPlay
  , GameViewLoading
  , GameViewMainMenu
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

  { Create game views and set initial view }
  {$region 'Castle View Creation'}
  // The content here may be automatically updated by CGE editor.
  ViewMainMenu := TViewMainMenu.Create(Application);
  ViewPlay := TViewPlay.Create(Application);
  ViewLoading := TViewLoading.Create(Application);
  {$endregion 'Castle View Creation'}

  { set current view }
  Window.Container.View := ViewMainMenu;
end;

initialization
  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindow.Create(Application);
  Window.ParseParameters; // allows to control window size / fullscreen on the command-line
  Application.MainWindow := Window;
end.
