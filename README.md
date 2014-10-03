# SBOLv Web Widgets

This is a library of web widgets for SBOLv. It is structured as a demo Play! application. The widgets can all be used
from javascript by including the `sbolv_js-fastopt.js` javascript file, and calling the entry points you need. This
provides the following:

* Javascript factories for programatically creating and modifying SBOLv widgets representing genetic parts and laying
 these out as genetic designs
* Shortcodes for displaying parts and designs in-line within the normal flow of text.

## Run the application
```shell
$ sbt
> run
$ open http://localhost:9000
```

## Stand-alone build
```shell
$ sbt
> dist
```

This will build an archive named something like `./target/universal/sbolv_jvm-0.1.0-SNAPSHOT.zip`. Unzip this and run
the script in `./bin/` to launch the web server. It will print out the URL you need to put into your browser to try the
widgets out. This will typically be:

http://localhost:9000/index.html