#!/usr/bin/env python
# -*- coding: utf-8 -*-

# MIT License
# 
# Copyright (c) 2017 by Nathan Chong and John Wickerson
# 
# Permission is hereby granted, free of charge, to any person obtaining
# a copy of this software and associated documentation files (the
# "Software"), to deal in the Software without restriction, including
# without limitation the rights to use, copy, modify, merge, publish,
# distribute, sublicense, and/or sell copies of the Software, and to
# permit persons to whom the Software is furnished to do so, subject to
# the following conditions:
# 
# The above copyright notice and this permission notice shall be
# included in all copies or substantial portions of the Software.
# 
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
# EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
# MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
# NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
# LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
# OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
# WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

import argparse
import argparsing
import datetime
import os
import shutil
import sys
import tempfile

def main(args):
  timestamp = "{:%y%m%d-%H%M%S}".format(datetime.datetime.now())
  try:
    abspath = os.path.join(args.base_result_dir, timestamp)
    os.mkdir(abspath)
    latest_symlink = os.path.join(args.base_result_dir, "_latest")
    if os.path.exists(latest_symlink):
      os.remove(latest_symlink)
    os.symlink(abspath, latest_symlink)
    for d in ["xml", "dot", "png"]:
      path = os.path.join(abspath, d)
      os.mkdir(path)
    return abspath
  except OSError as e:
    print("ERROR: could not create dir structure")
    return None

if __name__ == '__main__':
  parser = argparse.ArgumentParser("Create dir structure for results of comparison")
  argparsing.add_setup_result_dir_args(parser)
  args = parser.parse_args(sys.argv[1:])
  abspath = main(args)
  if abspath:
    print abspath
    sys.exit(0)
  sys.exit(1)
