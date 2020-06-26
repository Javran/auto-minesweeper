#!/usr/bin/env python3.7
"""Recognizes current game board.
"""

import collections
import json
import os
import re
import subprocess
import sys
import uuid

import cv2
import numpy


Info = collections.namedtuple(
    'Info',
    ['window_id', 'top_left', 'tiles_shape',
     'tile_size', 'tile_shrink',
     # find_window.py won't have this 'tagging',
     # used for indicating that we actually want to store the sample.
     'tagging'
    ])


# Tagged patterns are small png files that has the same shape (width and height) as
# tiles with a filename indicating which tag it belongs to.
# Without being too pedentic, a png file "XXX_somestuff.png" is a pattern with tag "XXX"
# and "somestuff" is ignored. In addition "untagged" is meant to be manually tagged
# therefore it is ignored when importing patterns.
class TileMatcher:
    """Matches tiles with a set of known and tagged tile samples.
    """

    @staticmethod
    def load_tagged_samples(asset_path):
        samples = collections.defaultdict(list)
        re_tag = re.compile(r'^([0-9A-Za-z]+)_.*\.png$')
        for file_name in os.listdir(asset_path):
            match_result = re_tag.match(file_name)
            if match_result:
                tag = match_result.group(1)
                if tag != 'untagged':
                    img = cv2.imread(os.path.join(asset_path, file_name),
                                     cv2.IMREAD_COLOR)
                    samples[tag].append(img)
        return samples

    def __init__(self, asset_path):
        self.threshold = 0.999995
        self.asset_path = asset_path
        # key: tag, value: list of samples (images)
        self.tagged_samples = TileMatcher.load_tagged_samples(asset_path)

    def write_new_untagged(self, img):
        nonce = str(uuid.uuid4())
        file_path = os.path.join(self.asset_path, f"untagged_{nonce}.png")
        cv2.imwrite(file_path, img, [cv2.IMWRITE_PNG_COMPRESSION])

    def match(self, img):
        best = None # (tag, val)
        for tag, samples in self.tagged_samples.items():
            for s in samples:
                [[res]] = cv2.matchTemplate(img, s, cv2.TM_CCORR_NORMED)
                if res >= self.threshold and (best is None or best[1] < res):
                    best = (tag, res)
        if best is None:
            return None
        return best[0]


tile_matcher = TileMatcher('assets/tagged')


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
    def obj_hook(d):
        if not 'tagging' in d:
            d['tagging'] = False
        else:
            d['tagging'] = bool(d['tagging'])
        return Info(**d)
    info = json.loads(info_raw, object_hook=obj_hook)
    tiles = screenshot_to_tiles(get_screenshot(info), info)
    cols, rows = info.tiles_shape
    tile_count = cols * rows

    def tag_to_char(tag):
        if tag == 'unknown':
            return '?'
        elif tag == 'no':
            return ' '
        else:
            return tag

    unrecognized_tiles = []
    output_tmp = []

    for row in tiles:
        cur_row = []
        for tile in row:
            r = tile_matcher.match(tile)
            if r is None:
                unrecognized_tiles.append(tile)
                ch = 'E'
            else:
                ch = tag_to_char(r)
            cur_row.append(ch)
        output_tmp.append(''.join(cur_row))
    if unrecognized_tiles:
        # If there are too many unmatched files, chances are we are looking at
        # some screenshot totally unrelated. In those cases it's better not to store anything.
        assert len(unrecognized_tiles) != tile_count, 'Too many unrecognized tiles.'
        # Unrecognized tiles needs to be stored and manually tagged.
        if info.tagging:
            for t in unrecognized_tiles:
                tile_matcher.write_new_untagged(t)
            print(f"{len(unrecognized_tiles)} untagged tiles added to assets.")
        else:
            print(f"{len(unrecognized_tiles)} untagged tiles found but not stored.")

    print(f"{rows} {cols}")
    for o in output_tmp:
        print(o)

    if unrecognized_tiles:
        # This type of exception can be resolved by tagging manually.
        # Therefore we explicitly set exit code to 20 so that caller program can
        # return error messages correspondingly.
        sys.exit(20)
