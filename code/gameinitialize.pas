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
  CastleTransform, CastleVectors, CastleImages,
  CastleOnScreenMenu, CastleUtils, CastleBoxes, CastleNotifications,
  GameStatePlay, GameStateLoading, GameStateMainMenu;

{ application routines ------------------------------------------------------- }

var
  Window: TCastleWindowCustom;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  Window.Container.UIReferenceWidth := 1024;
  Window.Container.UIReferenceHeight := 768;
  Window.Container.UIScaling := usEncloseReferenceSize;

  { customize TCastleLabel with TCastleLabel.Frame=true look }
  Theme.Images[tiLabel] := LoadImage(ApplicationData('gui/transparent_pixel.png'));
  Theme.OwnsImages[tiLabel] := true;
  Theme.Corners[tiLabel] := Vector4Integer(0, 0, 0, 0);

  { create TUIState instances }
  StateMainMenu := TStateMainMenu.Create(Application);
  StatePlay := TStatePlay.Create(Application);
  StateLoading := TStateLoading.Create(Application);

  { set current state }
  TUIState.Current := StateMainMenu;
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
  Window := TCastleWindowCustom.Create(Application);
  Application.MainWindow := Window;
end.
