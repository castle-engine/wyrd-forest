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
unit GameInitialize;

interface

implementation

uses SysUtils, Classes,
  CastleWindow, CastleScene, CastleControls, CastleLog,
  CastleFilesUtils, CastleSceneCore, CastleKeysMouse, CastleColors,
  CastleUIControls, CastleUIState, CastleSceneManager,
  CastleTransform, CastleVectors, CastleImages, CastleApplicationProperties,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications,
  GameStatePlay, GameStateLoading, GameStateMainMenu;

{ application routines ------------------------------------------------------- }

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { customize TCastleLabel with TCastleLabel.Frame=true look }
  Theme.Images[tiLabel] := LoadImage('castle-data:/gui/transparent_pixel.png');
  Theme.OwnsImages[tiLabel] := true;
  Theme.Corners[tiLabel] := Vector4(0, 0, 0, 0);

  { create TUIState instances }
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateLoading := TStateLoading.Create(Application);

  { set current state }
  TUIState.Current := StateMainMenu;
end;

initialization
  { Set ApplicationName early, as our log uses it. }
  ApplicationProperties.ApplicationName := 'wyrd-forest';

  InitializeLog;

  { initialize Application callbacks }
  Application.OnInitialize := @ApplicationInitialize;

  { create Window and initialize Window callbacks }
  Window := TCastleWindowBase.Create(Application);
  Application.MainWindow := Window;
end.
