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

{ TStateMainMenu, that asks player what terrain to load. }
unit GameStateMainMenu;

interface

uses Classes,
  CastleControls, CastleUIState, CastleOnScreenMenu, CastleKeysMouse;

type
  TStateMainMenu = class(TUIState)
  strict private
    SimpleBackground: TCastleSimpleBackground;
    ImageBackground: TCastleImageControl;
    EditDivisions: TCastleEdit;
    LabelDivisions: TCastleLabel;
    ButtonPlay: TCastleButton;
    procedure PlayClick(Sender: TObject);
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): boolean; override;
  end;

var
  StateMainMenu: TStateMainMenu;

implementation

uses SysUtils,
  CastleColors, CastleUIControls, CastleFilesUtils, CastleUtils,
  GameStatePlay;

{ TStateMainMenu ------------------------------------------------------------- }

procedure TStateMainMenu.Start;
begin
  inherited;

  SimpleBackground := TCastleSimpleBackground.Create(FreeAtStop);
  InsertFront(SimpleBackground);

  ImageBackground := TCastleImageControl.Create(FreeAtStop);
  ImageBackground.Stretch := true;
  ImageBackground.ProportionalScaling := psEnclose;
  ImageBackground.URL := ApplicationData('textures/island_sand2_d.jpg');
  ImageBackground.FullSize := true;
  ImageBackground.Anchor(hpMiddle);
  ImageBackground.Anchor(vpMiddle);
  InsertFront(ImageBackground);

  EditDivisions := TCastleEdit.Create(FreeAtStop);
  EditDivisions.CaptureAllInput := true;
  EditDivisions.AllowedChars := ['0'..'9'];
  EditDivisions.Width := 300;
  EditDivisions.MaxLength := 10;
  EditDivisions.Text := '100';
  EditDivisions.Anchor(hpLeft, 10);
  EditDivisions.Anchor(vpBottom, 10);
  InsertFront(EditDivisions);

  LabelDivisions := TCastleLabel.Create(FreeAtStop);
  LabelDivisions.MaxWidth := StateContainer.UnscaledWidth - 20;
  LabelDivisions.Anchor(hpLeft, 10);
  LabelDivisions.Anchor(vpBottom, 10 + EditDivisions.CalculatedHeight + 10);
  LabelDivisions.Color := Black;
  LabelDivisions.Caption := 'Initial Terrain Divisions.' + NL +
    '50 - 100 is good for experimenting with terrain noise parameters.' + NL +
    '500 is good for actualy playing.' + NL +
    '1000 is great for playing, it is large (and still plays very fast), but loading takes a while now (~50 secs).';
  InsertFront(LabelDivisions);

  ButtonPlay := TCastleButton.Create(FreeAtStop);
  ButtonPlay.Caption := 'PLAY';
  ButtonPlay.FontSize := 40;
  ButtonPlay.Anchor(hpMiddle);
  ButtonPlay.Anchor(vpMiddle);
  ButtonPlay.OnClick := @PlayClick;
  ButtonPlay.PaddingHorizontal := 40;
  ButtonPlay.PaddingVertical := ButtonPlay.PaddingHorizontal;
  InsertFront(ButtonPlay);
end;

procedure TStateMainMenu.PlayClick(Sender: TObject);
begin
  StatePlay.InitialDivisions := StrToInt(EditDivisions.Text);
  TUIState.Current := StatePlay;
end;

function TStateMainMenu.Press(const Event: TInputPressRelease): boolean;
begin
  Result := inherited;
  if Result then Exit;

  if Event.IsKey(K_Enter) then
    PlayClick(nil);
end;

end.
