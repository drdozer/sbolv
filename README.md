# SBOLv Web Widgets

This is a library of web widgets for SBOLv. It provides reactive components that manage SVG fragments to display SBOLv
glyphs and to lay them out as design visualisations. In particular it provides:

* Javascript factories for pragmatically creating and modifying SBOLv widgets representing genetic parts and laying
 these out as genetic designs
* Shortcodes for displaying parts and designs in-line within the normal flow of text
* scala-js reactive API for building  your own complex user interfaces on top of Web Widgets

## Run the application
```shell
$ sbt
> ~sbolv-demo_jvm/re-start
$ open http://localhost:9200
```

## Project Structure

The SBOLv Web Widgets project is broken up into 3 modules:

1. sbolv-util: re-usable utility code.
2. sbolv-widgets: The SBOLv Web Widgets and related code. This can be used to build a .js file directly, or imported
into 

For more information, see the [project page](http://drdozer.github.io/sbolv/).