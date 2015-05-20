#!/usr/bin/node
var path = require('path');
var cp = require('child_process');
var os = require('os');
var fs = require('fs');

if (os.platform() != "linux") {
    throw new Error("This script is intended for linux distributables.");
}

var exeFile = path.resolve(__dirname,"..","executable","stew-gui");
var distDir = path.resolve(__dirname,"..","distributable");
var archID;
switch (os.arch()) {
    case "x64":
      archID = "amd64";
      break;
    default:
      throw new Error("Code needs to be completed here for this architecture");
}

cp.execFile(exeFile,["--version"],function(err,stdout,stderr) {
    if (err) {
        console.error(err);
    } else if (stderr) {
        console.error(stderr);
    } else {
        var version = stdout.trim();
        
        var distFile = path.join(distDir,"stew-gui-linux_" + version + "_" + archID + ".deb");
        fs.exists(distFile,function(answer) {
            if (answer) {
                console.log("Distributable for current version already exists. Run ./build-linux to refresh it.");
            } else {
                console.log("Spawning dch");
                var p = cp.spawn("dch",["-v",version],{ stdio: "inherit" });
                p.on('exit',function(code,signal) {
                    if ((!code) && (!signal)) {
                        process.nextTick(function() {
                            cp.spawn(path.join(__dirname,"build-linux"),[],{stdio: "inherit" });
                        });
                    }
                });
            }
        });
         
    }
});
