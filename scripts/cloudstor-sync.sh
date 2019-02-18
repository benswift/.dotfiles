#!/bin/bash

# duck -u ben.swift@anu.edu.au --list davs://cloudstor.aarnet.edu.au/plus/remote.php/webdav/

_JVM_OPTS="-Xmx16G"

duck -u ben.swift@anu.edu.au\
	 --assumeyes\
	 --parallel 8\
	 --existing compare\
	 --upload davs://cloudstor.aarnet.edu.au/plus/remote.php/webdav/smithy/\
	 ~/Documents/
