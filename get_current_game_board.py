#!/usr/bin/env python3.7
"""Recognizes current game board.
"""

import collections
import json
import subprocess
import sys

import cv2
import numpy


Info = collections.namedtuple(
    'Info',
    ['window_id', 'top_left', 'tiles_shape',
     'tile_size', 'tile_shrink'])

def screenshot(info):
    x, y = info.top_left
    cols, rows = info.tiles_shape
    tile_width, tile_height = info.tile_size
    width, height = cols * tile_width, rows * tile_height
    result = subprocess.run(['import',
                             '-window', str(info.window_id),
                             '-crop', f"{width}x{height}+{x}+{y}",
                             'png:-'],
                            stdout=subprocess.PIPE, check=True)
    nparr = numpy.frombuffer(result.stdout, numpy.uint8)
    return cv2.imdecode(nparr, cv2.IMREAD_COLOR)


if __name__ == '__main__':
    # This takes the output from find_window.py
    # info are valid as long as the browser tab
    # in question is still opened.
    [_, info_raw] = sys.argv
    info = json.loads(info_raw, object_hook=lambda d: Info(**d))
    screenshot(info)
