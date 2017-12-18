#!/bin/env python

from mpd import MPDClient
from os import getenv


def escape_pango(msg):
    msg = msg.replace('&', '&amp;').replace('<', '&lt;').replace('>', '&gt;')
    return msg


def print_song_info(info_dict):
    if info_dict:
        print('<b>{}</b> - {}'.format(escape_pango(song_info['artist']),
                                      escape_pango(song_info['title'])))
        print(escape_pango(song_info['title']))
    else:
        print('silence…')
        print('…')


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

print_song_info(song_info)
