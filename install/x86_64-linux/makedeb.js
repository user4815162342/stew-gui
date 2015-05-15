#!/usr/bin/node
var cp = require('child_process');
var path = require('path');
var fs = require('fs');

// based on steps outlined here:
// http://ubuntuforums.org/showthread.php?t=910717

// TODO: There's possibly more I want to do, see:
// http://forum.lazarus.freepascal.org/index.php?topic=5032.0

// TODO: Turn this into a gulp file, to make the processing of dependencies
// easier.
// TODO: Need to check if dependencies are already met by one of the other dependencies.
// TODO: Don't do *too* much more, because in theory there are packaging tools
// out there that already do all of this stuff.

// globals...
const packageVersion = "1";
const packageBaseName = "stew-gui";
const maintainer = "Neil M. Sheldon <neilmsheldon@townsedgetechnology.com>";
const description = "A manuscript management and notekeeping tool.";
const section = "editors";
const priority = "optional";
const additionalDependencies = ["xdg-open", "xdg-user-dir"];

var baseDir = path.resolve(__dirname,"../..");
console.log("baseDir:",baseDir);
var platformName = path.basename(__dirname);
console.log("platformName:",platformName);
var exeDir = path.join(baseDir,"executable",platformName);
console.log("exeDir:",exeDir);
var exeFile = path.join(exeDir,packageBaseName);
console.log("exeFile:",exeFile);
var resourcesDir = path.join(__dirname,"resources");
console.log("resourcesDir:",resourcesDir);
var packageDir = path.join(baseDir,"distributable",platformName);
console.log("packageDir:",packageDir);
var packageExeFile = path.join(packageDir,"usr","local","bin",path.basename(exeFile));
console.log("packageExeFile:",packageExeFile);
var packageControlFile = path.join(packageDir,"DEBIAN","control");
console.log("packageControlFile:",packageControlFile);


// these are initialized later.
var productVersion = null;
var packageName = "";
var packageFile = "";
var dependencies = [];
// TODO: Also need xdg-user-dirs and xdg-open.


// this is called to build these contents.
var packageControlFileContents = function() {
    
    var architecture;
    switch (platformName) {
        case "x86_64-linux":
           architecture = "amd64";
           break;
        default:
           console.error("I don't know what architecture to put in the control file.");
           process.exit(1);
    }
   
    return "Package: " + packageBaseName + "\n" +
            "Version: " + productVersion + "-" + packageVersion + "\n" +
            "Section: " + section + "\n" +
            "Priority: " + priority + "\n" +
            "Architecture: " + architecture + "\n" +
            "Depends: " + dependencies.join(", ") + "\n" +
            "Maintainer: " + maintainer + "\n" +
            "Description: " + description + "\n";
}


// utility functions
var createDir = function(dir,cb) {
    fs.exists(dir,function(exists) {
        if (exists) {
            cb();
        } else {
            createDir(path.dirname(dir),function(err) {
                console.log("Creating directory " + dir);
                fs.mkdir(dir,function(err) {
                    cb(err);
                });
            });
        }
    });
}

var copyTree = function(source, target, cb) {
    if (source.join) {
        if (source.length) {
            // we have a list of items, so copy them. 
            copyTree(source.pop(),target.pop(),function(err) {
                if (err) {
                    cb(err);
                } else {
                    copyTree(source,target,cb);
                }
            });
        } else {
            cb();
        }
    } else {
        // we have a specific file name.
        fs.stat(source,function(err,stats) {
            if (err) {
                cb(err);
            } else if (stats.isDirectory()) {
                console.log("Copying directory " + source + " to " + target);
                createDir(target,function(err) {
                    if (err) {
                        cb(err);
                    } else {
                        fs.readdir(source,function(err,list) {
                            if (err) {
                                cb(err);
                            } else {
                                copyTree(list.map(function(file) {
                                    return path.join(source,file);
                                }),list.map(function(file) {
                                    return path.join(target,file);
                                }),cb);
                            }
                        });
                    }
                });
            } else {
                copyFile(source,target,{},cb);
            }
        });
    }
}

var copyFile = function(source, target, options, cb) {
  var cbCalled = false;
  console.log("copying " + source + " to " + target);

  var rd = fs.createReadStream(source);
  rd.on("error", done);

  var wr = fs.createWriteStream(target,options);
  wr.on("error", done);
  wr.on("close", function(ex) {
    done();
  });
  rd.pipe(wr);

  function done(err) {
    if (!cbCalled) {
      cb(err);
      cbCalled = true;
    }
  }
}

var deleteTree = function(source, cb) {
    if (source.join) {
        if (source.length) {
            // we have a list of items, so copy them. 
            deleteTree(source.pop(),function(err) {
                if (err) {
                    cb(err);
                } else {
                    deleteTree(source,cb);
                }
            });
        } else {
            cb();
        }
    } else {
        // we have a specific file name.
        fs.stat(source,function(err,stats) {
            if (err) {
                if (err.code == 'ENOENT') {
                    // already deleted.
                    cb();
                } else {
                    cb(err);
                }
            } else if (stats.isDirectory()) {
                fs.readdir(source,function(err,list) {
                    if (err) {
                        cb(err);
                    } else {
                        deleteTree(list.map(function(file) {
                            return path.join(source,file);
                        }),function(err) {
                            if (err) {
                                cb(err);
                            } else {
                                console.log("Deleting directory " + source);
                                fs.rmdir(source,cb);
                            }
                        });
                    }
                });
            } else {
                console.log("Deleting file " + source);
                fs.unlink(source,cb);
            }
        });
    }
}


var which = function(file,cb) {
    console.log("Finding location of " + file);
    cp.execFile("which",[file],function(err,stdout,stderr) {
        if (err) {
            cb(err);
        } else if (stderr) {
            cb(stderr);
        } else {
            cb(null,stdout.trim());
        }
    });
}

var batchWhich = function(files,cb) {
    if (files.length) {
        which(files.pop(),function(err,result) {
            if (err) {
                cb(err);
            } else {
                batchWhich(files,function(err,results) {
                    if (!err) {
                        if (results) {
                            results.push(result);
                        } else {
                            results = [result];
                        }
                    }
                    cb(err,results);
                });
            }
        });
    } else {
        cb();
    }
}

var findPackages = function(libraries,cb) {
    process.nextTick(function() {
        var results = [];
        if (libraries.length) {
            var library = libraries.shift();
            console.log("Finding package for " + library);
            cp.execFile("dpkg",["-S",library],function(err,stdout,stderr) {
                if (err) {
                    cb(err);
                } else if (stderr) {
                    cb(stderr);
                } else {
                    var result = stdout.trim().split(":");
                    if (result.length > 2) {
                        cb("Library " + library + " found in multiple packages.");
                        return;
                    }
                    findPackages(libraries,function(err,packages) {
                        if (!err) {
                            if (!packages) {
                                packages = {};
                            }
                            packages[result[0]] = true;
                        }
                        cb(err,packages);
                    });
                }
            });
        } else {
            cb();
        }
    });
}

var findDependencies = function(pkg,cb) {
// TODO: find the dependencies for a given package. Or, is it possible to figure
// out if a given package is dependent on another?
}

var reduceDependencies = function(list,cb) {
// TODO: Find if any other dependencies in the list are a dependency of the others,
// and remove them.
}


var listDependencies = function(cb) {
    console.log("Finding dependent libraries " + exeFile);
    cp.execFile("ldd",[exeFile],function(err,stdout,stderr) {
        if (err) {
            cb(err);
        } else if (stderr) {
            cb(stderr);
        } else {
            var lines = stdout.split("\n");
            var search1 = /\s*[^=]*=> ([^(]*)\([^)]*\)\s*/;
            var search2 = /\s*([^(]*)\([^)]*\)\s*/;
            lines = lines.map(function(line) {
                line = line.trim();
                if (line == '') {
                  return null;
                }
                var match = search1.exec(line);
                if (match) {
                    return match[1].trim()
                } else {
                    match = search2.exec(line);
                    if (match) {
                        return match[1].trim()
                    } else {
                        return "=========>" + line;
                    }
                }
            }).filter(function(line) {
                return (line !== null) && (line !== '');
            });
            batchWhich(additionalDependencies,function(err,results) {
                if (err) {
                    cb(err);
                } else {
                    findPackages(lines.concat(results),function(err,results) {
                        if (err) {
                            cb(err);
                        } else {
                            cb(null,Object.keys(results));
                        }
                    });
                }
            });
        }
    });
}



// async processing functions
var failed = function(err) {
    console.error(err);
    process.exit(1);
}

var buildPackage = function() {
// dpkg-deb --build helloworld_1.0-1
   console.log("Building package " + packageDir);
   cp.execFile("dpkg-deb",["--build",packageDir,packageFile],{ cwd: path.dirname(packageDir) },function(err,stdout,stderr) {
       if (err) {
           failed(err);
       } else if (stderr) {
           failed(stderr);
       } else {
           console.log(stdout);
       }
   });    
   
}

var fillPackage = function() {
    createDir(path.dirname(packageExeFile),function(err) {
        if (err) {
            failed(err);
        } else {
            console.log("Package Executable directory created.");
            copyFile(exeFile,packageExeFile,{ mode: 0755 },function(err) {
                if (err) {
                    failed(err);
                } else {
                    console.log("Executable copied over.");
                    createDir(path.dirname(packageControlFile),function(err) {
                        if (err) {
                            failed(err);
                        } else {
                            console.log("Package control file directory created.");
                            fs.writeFile(packageControlFile,packageControlFileContents(),function(err) {
                                if (err) {
                                    failed(err);
                                } else {
                                    console.log("Package control file written.");
                                    copyTree(resourcesDir,packageDir,function(err) {
                                        if (err) {
                                            failed(err);
                                        } else {
                                            buildPackage();
                                        }
                                    });
                                }
                            });
                        }
                    });
                }
            });
        }
    });
}

var createPackageDir = function() {
    createDir(packageDir,function(err) {
        if (err) {
            failed(err);
        } else {
            console.log("Package directory exists.");
            fillPackage();
        }
    });
}

var findDependencies = function() {
   listDependencies(function(err,results) {
       if (err) {
           failed(err);
       } else {
           dependencies = results;
           console.log("Dependencies:",dependencies.join(", "));
           createPackageDir();
       }
   });
}

var versionRan = function(err,stdout,stderr) {
    if (err) {
        failed(err);
    } else if (stderr) {
        failed(stderr);
    } else {
        productVersion = stdout.trim().split(".");
        productVersion = productVersion[0] + "." + productVersion[1];
        console.log("productVersion:",productVersion);
        packageName = packageBaseName + "_" + productVersion + "-" + packageVersion;
        console.log("packageName:",packageName);
        packageFile = packageName + "_" + platformName + ".deb";
        findDependencies();
    }
}

var loadVersion = function() {
    console.log("Getting version number from executable");
    cp.execFile(exeFile,["-version"],versionRan);
}

var clean = function() {
    deleteTree(packageDir,function(err) {
        if (err) {
            failed(err);
        }
    });
}


// kick it off
if (process.argv.length > 2) {
    if (process.argv[2] == "--list-deps") {
        listDependencies(function(err,results) {
            if (err) {
                failed(err);
            } else {
                // NOTE: I could further reduce the dependencies, but I'm not so sure...
                console.log("Dependencies suggested:");
                console.log("\t" + results.join("\n\t"));
            }
        });
    } else if (process.argv[2] == "--clean") {
        clean();
    } else {
        failed("Unknown option " + process.argv[2]);
    }
} else {
    loadVersion();
}

