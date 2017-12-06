#!/bin/bash

if systemctl --user is-active mpd.service > /dev/null; then
    here=$(dirname $0)
    BLOCK_INSTANCE=$BLOCK_INSTANCE BLOCK_BUTTON=$BLOCK_BUTTON python $here/mpdinfo.py
fi
