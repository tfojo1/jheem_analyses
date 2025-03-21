#!/usr/bin/bash

for j in `seq 22131495 22131500` ; do
    scancel $j
    echo $j
done