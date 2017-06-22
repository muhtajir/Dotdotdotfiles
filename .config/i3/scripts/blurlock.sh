#!/bin/bash

import -window root /tmp/.blurlock.mpc
convert -blur 10x5 -quality 109 /tmp/.blurlock.mpc /tmp/.blurlock.png
i3lock -t -e -i /tmp/.blurlock.png
