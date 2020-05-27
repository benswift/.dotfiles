#!/bin/bash

# to list what's there:
# duck -u ben.swift@anu.edu.au --list davs://cloudstor.aarnet.edu.au/plus/remote.php/webdav/

declare -a FOLDERS=("Calibre Library" "Zotero" "business" "church" "misc" "personal" "research" "teaching")

# Read the array values with space
for folder in "${FOLDERS[@]}"; do
	duck -u ben.swift@anu.edu.au\
		 --assumeyes\
		 --quiet\
		 --parallel 4\
		 --existing compare\
		 --upload davs://cloudstor.aarnet.edu.au/plus/remote.php/webdav/smithy-duck/Documents/\
		 "~/Documents/$folder"
done
