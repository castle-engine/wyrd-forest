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

{ Text that appears and fades out in 3D (TText3D). }
unit GameText3D;

interface

uses Classes,
  CastleTransform, CastleScene, X3DNodes, CastleTimeUtils;

type
  { Text that appears and fades out in 3D.
    Used to show messages like "+10 points!" or "Bullseye!" during the game. }
  TText3D = class(TCastleScene)
  private
    Material: TUnlitMaterialNode;
    LifeTime: TFloatTime;
  public
    constructor Create(const AOwner: TComponent; const Text: string); reintroduce;
    procedure Update(const SecondsPassed: Single; var RemoveMe: TRemoveType); override;
  end;

implementation

uses CastleColors, CastleUtils;

constructor TText3D.Create(const AOwner: TComponent; const Text: string);
var
  Billboard: TBillboardNode;
  TextNode: TTextNode;
  FontStyle: TFontStyleNode;
  Shape: TShapeNode;
  Appearance: TAppearanceNode;
  Root: TX3DRootNode;
begin
  inherited Create(AOwner);

  TextNode := TTextNode.Create;
  TextNode.SetText([Text]);

  FontStyle := TFontStyleNode.Create;
  FontStyle.Justify := fjMiddle;
  FontStyle.Family := ffSans;
  FontStyle.Size := 0.4;
  FontStyle.Bold := true;
  TextNode.FontStyle := FontStyle;

  Material := TUnlitMaterialNode.Create;
  Material.EmissiveColor := YellowRGB;

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := TextNode;
  Shape.Appearance := Appearance;

  Billboard := TBillboardNode.Create;
  Billboard.AddChildren(Shape);

  Root := TX3DRootNode.Create;
  Root.AddChildren(Billboard);

  Load(Root, true);

  Collides := false;
  Pickable := false;
  ProcessEvents := true; // needed for Billboard to work
end;

procedure TText3D.Update(const SecondsPassed: Single; var RemoveMe: TRemoveType);
const
  MoveSpeed = 0.5;
  FadeTime = 1.0;
begin
  inherited;
  LifeTime := LifeTime + SecondsPassed;
  if LifeTime > FadeTime then
    RemoveMe := rtRemoveAndFree;
  Translation := Translation + World.GravityUp * (MoveSpeed * SecondsPassed);
  Material.Transparency := MapRange(LifeTime, 0, FadeTime, 0, 1);
end;

end.
