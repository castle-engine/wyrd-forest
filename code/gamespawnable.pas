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

{ Common logic of something that can be spawned (TSpawnable). }
unit GameSpawnable;

interface

uses CastleTransform, CastleScene, CastleVectors, X3DFields, X3DTime;

type
  { Common logic of something that can be spawned,
    like a tree or shooting target. }
  TSpawnable = class(TCastleTransform)
  private
    Scene: TCastleScene;
    procedure SpawnIsActiveChanged(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure Spawn(const SceneTemplate: TCastleScene; const APosition: TVector3);
  end;

implementation

uses X3DNodes, CastleLog, CastleSceneCore;

procedure TSpawnable.Spawn(const SceneTemplate: TCastleScene;
  const APosition: TVector3);
var
  TimeSensor: TTimeSensorNode;
begin
  Translation := APosition;

  Scene := SceneTemplate.Clone(Self);
  Scene.ProcessEvents := true;
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  Add(Scene);

  Scene.PlayAnimation('spawn', paForceNotLooping);
  TimeSensor := Scene.AnimationTimeSensor('spawn');
  if TimeSensor = nil then
    WritelnWarning('Missing TimeSensor for animation "spawn"')
  else
    TimeSensor.EventIsActive.AddNotification(@SpawnIsActiveChanged);
end;

procedure TSpawnable.SpawnIsActiveChanged(
  Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
var
  Val: boolean;
begin
  Val := (Value as TSFBool).Value;
  if not Val then
  begin
    { This is not actually necessary now, we could as well just remain
      in the last frame of the "spawn" animation,
      it works as well for current tree and evil-squirrel models.
      But in the future them may have more interesting "idle" animations. }
    Scene.PlayAnimation('idle', paForceLooping);
  end;
end;

end.
