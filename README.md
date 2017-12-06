# Wyrd Forest

Game about walking in a weird forest, planting healing trees, and shooting evil targets.

Game sponsored by _Robert Daniel Murphy_ through Patreon ( https://www.patreon.com/castleengine ), showing off various [Castle Game Engine](https://castle-engine.sourceforge.io/) features. Thank you!

Features:

* Terrain editor (visually edit size and all generation parameters - octaves, smoothness, heterogeneous...)
* Terrain shader with 3 blended texture layers and darkened slopes (parameters are also editable during the game)
* Simple walking in FPS mode (AWSD keys, or drag with mouse, press F4 to toggle mouse look)
* Ability to save generated terrain as 3D model in X3D format

## Compilation

1. Get the [latest Castle Game Engine from GitHub](https://github.com/castle-engine/castle-engine/).
2. Compile:

    1. Using Lazarus:
        - Get [Lazarus](http://www.lazarus-ide.org/)
        - [Compile and install Castle Game Engine packages in Lazarus](https://castle-engine.sourceforge.io/documentation.php)
        - Open the `wyrd_forest.lpi` in Lazarus and press _Compile_ and _Run_.
    2. Using the build tool:
       - Get [Lazarus](http://www.lazarus-ide.org/) or just the [Free Pascal Compiler](https://www.freepascal.org/)
       - Set up the [Castle Game Engine build tool](https://github.com/castle-engine/castle-engine/wiki/Build-Tool) -- compile it and place on `$PATH`
       - Compile the game by simple `make` or `castle-engine compile` in the console.

## License

GPL 2.0 or any higher version. See the `LICENSE.txt` file.
