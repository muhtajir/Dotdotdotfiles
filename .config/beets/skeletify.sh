#!/bin/bash

sed -r 's/(\s*google.+?key.*:).+/#\1 restricted/gi' config.yaml > skel.tmp
mv skel.tmp config.yaml.skel
