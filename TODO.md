- Add water, like the one in our demo-models/water/ examples, and in little-things game.

- Add small grass moved by the wind.

- Add simple bloom screen effect, to make the forest look like under a bright daylight. I'm not sure whether it will look OK without changing the screen effects to use float textures, we'll see.

- Non-instant planting of trees and shooting:

    - When using _right mouse button_, you drop some "seed", and the tree grows where it falls. The "seed" is dropped just like boxes in [Castle Game Engine 3D physics demo](https://github.com/castle-engine/castle-engine/tree/master/examples/physics/physics_3d_demo).

    - When using _left mouse button_, you shoot an arrow that actually flies, with some gravity, through the level.

- Sounds:

    - Tree spawns with "whoosh" sound.
    - "Wooden plank standing up" sound when enemy spawns.
    - "hit" sound when arrow hits anything.
    - "crash" sound when arrow hits enemy, and it crashes.

- Shadow maps, to make dynamic objects (trees, enemies) seem more connected
  to the ground.

- Optimize TTerrainNoise.CreateNode in Castle Game Engine (run under profiler, possibly there are some easy spots to optimize --- it was never optimized yet; also, it's possible to remake it, to generate multiple vertexes at once, instead of calling `TTerrainNoise.Height` for each vertex).

- The shader has a small thin artifact visible at "hhalf" line, under

    Renderer: GeForce GTS 450/PCIe/SSE2
    Version: 4.5.0 NVIDIA 375.82
    on Linux/x86_64.
