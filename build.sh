#!/bin/bash
echo 'Building '$1' ...'
pasmo -1 --tapbas $1 $1.tap
retval=$?
if [ $retval -eq 0 ]; then
    echo 'Finished...'
    echo 'Running Fuse with '$2'.tap'
    /Applications/Fuse\ for\ Mac\ OS\ X/Fuse.app/Contents/MacOS/Fuse $1.tap
fi
