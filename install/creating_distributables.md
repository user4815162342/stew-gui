
## Linux (Ubuntu and other Debian-based systems)

I am attempting to use debhelper utilities to build a .deb file. The source for this is all set up in the 'debian' folder.
For future reference, this was setup using the following setup, and then template files were modified as needed.

```
   $DEBFULLNAME = Neil M. Sheldon
   $DEBEMAIL = ...
   dh_make --native --copyright gpl --single -p stew-gui-linux_0.1
```

To do a clean rebuild on this package, call ```./build-debian``` (with the current directory being the one that script is found in). Or, use ```dpkg-buildpackage``` with whatever parameters you need, just keep in mind that the created files will not be moved into 'distributables' that way.

Right now, there is a lintian error output at the end of this process that indicates that the binary does not have a man page. This
is a known issue that has to be dealt with.

The package built will always be the same package as the last one built. If you need to update the package versioning to reflect the
latest version number of the executable, then call ```./up-version-debian``` (again from the same directory). This will get the version and call 'dch' with it to enter a value in the changelog, then call build-debian.
