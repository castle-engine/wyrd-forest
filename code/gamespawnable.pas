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
    class var
      NextId: Integer;
    var
      Scene: TCastleScene;
    procedure SpawnIsActiveChanged(
      Event: TX3DEvent; Value: TX3DField; const Time: TX3DTime);
  public
    procedure Spawn(const SceneTemplate: TCastleScene);
  end;

implementation

uses SysUtils,
  X3DNodes, CastleLog, CastleSceneCore;

procedure TSpawnable.Spawn(const SceneTemplate: TCastleScene);
var
  TimeSensor: TTimeSensorNode;
  SceneName: string;
begin
  Scene := SceneTemplate.Clone(Self);
  Scene.ProcessEvents := true;
  Scene.Spatial := [ssRendering, ssDynamicCollisions];
  { Otherwise, the shape of the Scene before the animation plays may blink
    for a single frame. And our enemy and tree do not have the initial
    (before animation) frame equal to the 1st spawn frame. }
  Scene.ForceAnimationPose('spawn', 0, paForceNotLooping);
  Scene.Collides := SceneTemplate.Collides;
  Scene.Pickable := SceneTemplate.Pickable;
  Add(Scene);

  SceneName := SceneTemplate.Name;
  if SceneName = '' then
    SceneName := 'Spawnable';
  SceneName := SceneName + IntToStr(NextId);
  Inc(NextId);
  Scene.Name := SceneName; // assing unique name, for nicer debugging

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
