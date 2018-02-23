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

import os
import sys
import argparse
import argparsing
from argparsing import TOOL_PATH
import subprocess

def ext_of_file(f):
  _unused_basename, ext = os.path.splitext(f)
  return ext

def is_xml_file(f):
  return ext_of_file(f) == ".xml"

def main(argv=None):
  if argv is None:
    argv = sys.argv[1:]
  parser = argparse.ArgumentParser(description="Run gen over a hash file")
  parser.add_argument("-verbose", action="store_true")
  parser.add_argument("-push-on", action="store_true")
  parser.add_argument("-output", type=argparsing.is_existing_dir, 
                      default=os.getcwd(), help="Output directory")
  parser.add_argument("hash_file", type=file,
                      help="file containing xml hashes")
  parser.add_argument("-suffix", type=str, default="litmus")
  args, extra_args = parser.parse_known_args(argv)
  xml_result_dir = os.path.dirname(args.hash_file.name)
  for test_hash in args.hash_file:
    test_hash = test_hash.strip()
    out_file = os.path.join(args.output, "test_%s.%s" % (test_hash, args.suffix))
    xml_dir = os.path.join(xml_result_dir, "%s_unique" % test_hash)
    xml_files = [ x for x in os.listdir(xml_dir) if is_xml_file(x) ]
    cmd = [os.path.join(TOOL_PATH, "gen"), "-o", out_file, os.path.join(xml_dir, xml_files[0])]
    cmd.extend(extra_args)
    if args.verbose: print " ".join(cmd)
    code = subprocess.call(cmd)
    if code != 0:
      print "ERROR: litmus-test generation for %s was unsuccessful" % test_hash
      if args.push_on: continue
      return 1
  return 0

if __name__ == '__main__':
  sys.exit(main())
