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

{ Common logic of something that can be spawned (TSpawned). }
unit GameSpawned;

interface

uses CastleTransform, CastleSceneCore, CastleScene, CastleVectors, X3DNodes;

type
  { Common logic of something that can be spawned,
    like a tree or shooting target. }
  TSpawned = class(TCastleTransform)
  private
    class var
      NextId: Integer;
    var
      Scene: TCastleScene;
    procedure SpawnStopped(const AScene: TCastleSceneCore; const Animation: TTimeSensorNode);
  protected
    procedure SpawnEnded; virtual;
  public
    procedure Spawn(const SceneTemplate: TCastleScene);
  end;

implementation

uses SysUtils,
  CastleLog;

procedure TSpawned.Spawn(const SceneTemplate: TCastleScene);
var
  SceneName: String;
  PlayParams: TPlayAnimationParameters;
begin
  Scene := SceneTemplate.Clone(Self);
  Scene.ProcessEvents := true;
  Scene.PreciseCollisions := true;
  { Otherwise, the shape of the Scene before the animation plays may blink
    for a single frame. And our enemy and tree do not have the initial
    (before animation) frame equal to the 1st spawn frame. }
  Scene.ForceAnimationPose('spawn', 0, false);
  Scene.Collides := SceneTemplate.Collides;
  Scene.Pickable := SceneTemplate.Pickable;
  Add(Scene);

  SceneName := SceneTemplate.Name;
  if SceneName = '' then
    SceneName := 'Spawned';
  SceneName := SceneName + IntToStr(NextId);
  Inc(NextId);
  Scene.Name := SceneName; // assign unique name, for nicer debugging

  PlayParams := TPlayAnimationParameters.Create;
  try
    PlayParams.Name := 'spawn';
    PlayParams.StopNotification := @SpawnStopped;
    Scene.PlayAnimation(PlayParams);
  finally FreeAndNil(PlayParams) end;
end;

procedure TSpawned.SpawnStopped(const AScene: TCastleSceneCore; const Animation: TTimeSensorNode);
begin
  SpawnEnded;
end;

procedure TSpawned.SpawnEnded;
begin
  { This is not actually necessary now, we could as well just remain
    in the last frame of the "spawn" animation,
    it works as well for current tree and evil-squirrel models.
    But in the future them may have more interesting "idle" animations. }
  Scene.PlayAnimation('idle', true);
end;

end.
