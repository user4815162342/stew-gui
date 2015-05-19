
## Linux (Ubuntu and other Debian-based systems)

I am attempting to use debhelper utilities to build a .deb file. The source for this is all set up in the 'debian' folder.
For future reference, this was setup using the following setup, and then template files were modified as needed.

```
   $DEBFULLNAME = Neil M. Sheldon
   $DEBEMAIL = ...
   dh_make --native --copyright gpl --single -p stew-gui-linux_0.1
```

To do a clean rebuild on this package, call ```./build-linux```. Or, use ```dpkg-buildpackage``` with whatever parameters you need, just keep in mind that the created files will not be moved into 'distributables' that way.

Right now, there is a lintian error output at the end of this process that indicates that the binary does not have a man page. This
is a known issue that has to be dealt with.

