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

{ Game view showing loading screen. }
unit GameViewLoading;

interface

uses Classes, CastleControls, CastleKeysMouse, CastleUIControls,
  CastleStringUtils, CastleGLImages;

type
  { Show loading screen and switch to ViewPlay. }
  TViewLoading = class(TCastleView)
  private
    ImageBackground: TCastleImageControl;
    LabelLoading: TCastleLabel;
    RenderDone: boolean;
    RectLoading: TCastleRectangleControl;
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    procedure Render; override;
  end;

var
  ViewLoading: TViewLoading;

implementation

uses SysUtils,
  CastleColors, CastleTimeUtils, CastleLog, CastleFilesUtils, CastleVectors,
  GameViewPlay;

{ TViewLoading ----------------------------------------------------------------- }

procedure TViewLoading.Start;
begin
  inherited;

  // not needed, ImageBackground covers whole screen
  // Background := TCastleSimpleBackground.Create(FreeAtStop);
  // InsertFront(Background);

  ImageBackground := TCastleImageControl.Create(FreeAtStop);
  ImageBackground.Stretch := true;
  ImageBackground.ProportionalScaling := psEnclose;
  ImageBackground.URL := ApplicationData('gui/loading.png');
  ImageBackground.FullSize := true;
  ImageBackground.Anchor(hpMiddle);
  ImageBackground.Anchor(vpMiddle);
  InsertFront(ImageBackground);

  RectLoading := TCastleRectangleControl.Create(FreeAtStop);
  RectLoading.AutoSizeToChildren := true;
  RectLoading.Color := Vector4(0, 0, 0, 0.5);
  RectLoading.Anchor(hpMiddle);
  RectLoading.Anchor(vpMiddle);
  InsertFront(RectLoading);

  LabelLoading := TCastleLabel.Create(FreeAtStop);
  LabelLoading.Anchor(hpMiddle);
  LabelLoading.Anchor(vpMiddle);
  LabelLoading.Color := White;
  LabelLoading.Padding := 20;
  LabelLoading.Caption := 'Loading';
  RectLoading.InsertFront(LabelLoading);

  RenderDone := false;
end;

procedure TViewLoading.Update(const SecondsPassed: Single; var HandleInput: boolean);
begin
  inherited;
  if RenderDone then
    { show loading screen, then go to ViewPlay.
      The actual loading will happen in ViewPlay.Start, but with our background. }
    Container.View := ViewPlay;
end;

procedure TViewLoading.Render;
begin
  inherited;
  RenderDone := true;
end;

end.
