#!/usr/bin/python

from gi.repository.GLib import markup_escape_text
import subprocess
import os

instance = os.environ['BLOCK_INSTANCE']
artist = subprocess.check_output(['playerctl', 'metadata', '-p', instance, 'artist'])
title = subprocess.check_output(['playerctl', 'metadata', '-p', instance, 'title'])
artist = artist.decode()
title = title.decode()

# print(type(artist))
if artist == '' or artist == '(null)':
    print('silence…')
else:
    artist = ' <b>%s</b>' % markup_escape_text(artist)
    title = markup_escape_text(title)
    metadata = [artist, title]

    print(' - '.join(metadata))
