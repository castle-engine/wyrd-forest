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

{ TViewMainMenu, that asks player what terrain to load. }
unit GameViewMainMenu;

interface

uses Classes,
  CastleControls, CastleUIControls, CastleKeysMouse;

type
  { Ask player what terrain to load. }
  TViewMainMenu = class(TCastleView)
  strict private
    ImageBackground: TCastleImageControl;
    EditGridCount: TCastleEdit;
    LabelGridCount: TCastleLabel;
    ButtonPlay: TCastleButton;
    procedure PlayClick(Sender: TObject);
  public
    procedure Start; override;
    procedure Stop; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  ViewMainMenu: TViewMainMenu;

implementation

uses SysUtils,
  CastleColors, CastleFilesUtils, CastleUtils,
  GameViewPlay, GameViewLoading;

{ TViewMainMenu ------------------------------------------------------------- }

procedure TViewMainMenu.Start;
begin
  inherited;

  // not needed, ImageBackground covers whole screen
  // SimpleBackground := TCastleSimpleBackground.Create(FreeAtStop);
  // InsertFront(SimpleBackground);

  ImageBackground := TCastleImageControl.Create(FreeAtStop);
  ImageBackground.Stretch := true;
  ImageBackground.ProportionalScaling := psEnclose;
  ImageBackground.URL := 'castle-data:/gui/loading.png';
  ImageBackground.FullSize := true;
  ImageBackground.Anchor(hpMiddle);
  ImageBackground.Anchor(vpMiddle);
  InsertFront(ImageBackground);

  EditGridCount := TCastleEdit.Create(FreeAtStop);
  EditGridCount.AllowedChars := ['0'..'9'];
  EditGridCount.Width := 300;
  EditGridCount.MaxLength := 10;
  EditGridCount.Text := '100';
  EditGridCount.Anchor(hpLeft, 10);
  EditGridCount.Anchor(vpBottom, 10);
  InsertFront(EditGridCount);

  LabelGridCount := TCastleLabel.Create(FreeAtStop);
  LabelGridCount.MaxWidth := Container.UnscaledWidth - 20;
  LabelGridCount.Anchor(hpLeft, 10);
  LabelGridCount.Anchor(vpBottom, 10 + EditGridCount.EffectiveHeight + 10);
  LabelGridCount.Color := White;
  LabelGridCount.Frame := true;
  LabelGridCount.Padding := 10;
  LabelGridCount.Caption := 'Initial Grid Count.' + NL +
    'The terrain mesh size is (Grid Count) * (Grid Count).' + NL +
    '- 50-100 is good for experimenting with terrain noise parameters.' + NL +
    '- 500 is good for actualy playing.' + NL +
    '- 1000 is great for playing, it is large (and still plays very fast), but loading takes a while now (~50 secs).';
  InsertFront(LabelGridCount);

  ButtonPlay := TCastleButton.Create(FreeAtStop);
  ButtonPlay.Caption := 'PLAY';
  ButtonPlay.FontSize := 40;
  ButtonPlay.Anchor(hpMiddle);
  ButtonPlay.Anchor(vpMiddle);
  ButtonPlay.OnClick := @PlayClick;
  ButtonPlay.PaddingHorizontal := 40;
  ButtonPlay.PaddingVertical := ButtonPlay.PaddingHorizontal;
  InsertFront(ButtonPlay);

  Container.ForceCaptureInput := EditGridCount;
end;

procedure TViewMainMenu.Stop;
begin
  Container.ForceCaptureInput := nil;
  inherited;
end;

procedure TViewMainMenu.PlayClick(Sender: TObject);
begin
  ViewPlay.InitialGridCount := StrToInt(EditGridCount.Text);
  Container.View := ViewLoading; // ViewLoading will switch to ViewPlay
end;

function TViewMainMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(keyEnter) then
    PlayClick(nil);
end;

end.
