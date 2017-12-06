#!/bin/env python

from mpd import MPDClient
from os import getenv

host, port = [x.strip() for x in getenv('BLOCK_INSTANCE').split(';')]
try:
    mpd_client = MPDClient()
    mpd_client.connect(host, port)
    song_info = mpd_client.currentsong()
except Exception:
    quit()

clicked_button = getenv('BLOCK_BUTTON')

if clicked_button == "1":
    mpd_client.pause()

print('<b>{}</b> - {}'.format(song_info['artist'],
                              song_info['title']))
print(song_info['title'])
