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

{ Game state showing loading screen. }
unit GameStateLoading;

interface

uses Classes, CastleControls, CastleKeysMouse, CastleUIState,
  CastleStringUtils, CastleGLImages;

type
  TStateLoading = class(TUIState)
  private
    ImageBackground: TCastleImageControl;
    LabelLoading: TCastleLabel;
    RenderDone: boolean;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
  end;

var
  StateLoading: TStateLoading;

implementation

uses SysUtils,
  CastleColors, CastleUIControls, CastleTimeUtils, CastleLog, CastleFilesUtils,
  GameStatePlay;

{ TStateLoading ----------------------------------------------------------------- }

procedure TStateLoading.Start;
begin
  inherited;

  // not needed, ImageBackground covers whole screen
  // Background := TCastleSimpleBackground.Create(FreeAtStop);
  // InsertFront(Background);

  ImageBackground := TCastleImageControl.Create(FreeAtStop);
  ImageBackground.Stretch := true;
  ImageBackground.ProportionalScaling := psEnclose;
  ImageBackground.URL := ApplicationData('loading.png');
  ImageBackground.FullSize := true;
  ImageBackground.Anchor(hpMiddle);
  ImageBackground.Anchor(vpMiddle);
  InsertFront(ImageBackground);

  LabelLoading := TCastleLabel.Create(FreeAtStop);
  LabelLoading.Anchor(hpMiddle);
  LabelLoading.Anchor(vpMiddle);
  LabelLoading.Color := White;
  LabelLoading.Frame := true;
  LabelLoading.Padding := 20;
  LabelLoading.Caption := 'Loading';
  InsertFront(LabelLoading);

  RenderDone := false;
end;

procedure TStateLoading.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if RenderDone then
    { show loading screen, then go to StatePlay.
      The actual loading will happen in StatePlay.Start, but with our background. }
    TUIState.Current := StatePlay;
end;

procedure TStateLoading.Render;
begin
  inherited;
  RenderDone := true;
end;

end.
