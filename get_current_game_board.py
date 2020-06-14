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

def get_screenshot(info):
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


def screenshot_to_tiles(img, info):
    cols, rows = info.tiles_shape
    tile_width, tile_height = info.tile_size
    width, height = cols * tile_width, rows * tile_height
    shrink2 = info.tile_shrink * 2
    def crop(r,c):
        left = c * tile_width + info.tile_shrink
        top = r * tile_height + info.tile_shrink
        return img[top:top+tile_height-shrink2, left:left+tile_width-shrink2]
    return [[crop(r,c) for c in range(cols)] for r in range(rows)]


if __name__ == '__main__':
    # This takes the output from find_window.py
    # info are valid as long as the browser tab
    # in question is still opened.
    [_, info_raw] = sys.argv
    info = json.loads(info_raw, object_hook=lambda d: Info(**d))
    tiles = screenshot_to_tiles(get_screenshot(info), info)
    cv2.imshow('', cv2.vconcat([ cv2.hconcat(row) for row in tiles ]))
    cv2.waitKey(0)
    cv2.waitKey(0)
