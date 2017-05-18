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
import os
import sys
import subprocess
import platform

import argparsing
from argparsing import TOOL_PATH
import setup_result_dir
import run_alloy
import remove_dups

def ext_of_file(f):
  _unused_basename, ext = os.path.splitext(f)
  return ext

def is_cat_file(f):
  return ext_of_file(f) == ".cat"

def is_als_file(f):
  return ext_of_file(f) == ".als"

def is_xml_file(f):
  return ext_of_file(f) == ".xml"

def main(argv=None):
  if argv is None:
    argv = sys.argv[1:]
  parser = argparse.ArgumentParser(description="Top-level memalloy comparator", conflict_handler="resolve")

  parser.add_argument("-verbose", action="store_true",
                      help="Option: show subcommands and use verbose output")
  parser.add_argument("-expect", type=int, default=None,
                      help="Expect to find this many unique solutions (optional)")
  parser.add_argument("-desc", type=str, help="Textual description (optional)")
  parser.add_argument("-batch", action="store_true",
                        help="Option: suppress GUI")
  argparsing.add_setup_result_dir_args(parser)
  argparsing.add_gen_comparator_args(parser)
  argparsing.add_run_alloy_args(parser)
  argparsing.add_remove_dups_args(parser, is_standalone=False)
  args = parser.parse_args(argv)

  result_dir = setup_result_dir.main(args)
  xml_result_dir = os.path.join(result_dir, "xml")
  dot_result_dir = os.path.join(result_dir, "dot")
  png_result_dir = os.path.join(result_dir, "png")

  if args.desc:
    print "\"" + args.desc + "\""
  
  for model in args.satisfies + args.alsosatisfies + args.violates:
    if is_cat_file(model):
      if args.fencerels:
        code = subprocess.call([os.path.join(TOOL_PATH, "cat2als"), "-fencerels", model])
      else:
        code = subprocess.call([os.path.join(TOOL_PATH, "cat2als"), model])
      if code != 0:
        print "ERROR: Unable to convert cat file"
        return 1
    elif is_als_file(model):
      pass
    else:
      print "ERROR: Unrecognised model type [%s]" % model
      return 1

  comparator_script = os.path.join(result_dir, "comparator.als")
  cmd = [os.path.join(TOOL_PATH, "pp_comparator"), "-o", comparator_script]
  cmd.extend(argparsing.extract_gen_comparator_args(args))
  if args.verbose: print " ".join(cmd)
  code = subprocess.call(cmd)
  if code != 0:
    print "ERROR: Generation of comparator script was unsuccessful"
    return 1

  args.comparator_script = comparator_script
  args.alloystar = "alloystar"
  args.xml_result_dir = xml_result_dir
  try:
    code = run_alloy.main(args)
  except KeyboardInterrupt:
    code = 0
    print "WARNING: Alloy was interrupted"
  if code != 0:
    print "ERROR: Alloy was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if is_xml_file(x)])
  print "Alloy found %d solutions" % nsolutions
  
  if nsolutions == 0:
    if args.expect and args.expect != 0:
      print "ERROR: Expected %d unique solutions, found 0" % args.expect
      return 1
    else:
      return 0
  
  print "Remove duplicates"
  code = remove_dups.main(args)
  if code != 0:
    print "ERROR: Remove duplicates script was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if x.endswith("_unique")])
  print "Partitioned to %d unique solutions" % nsolutions

  # TODO: gen changed to operate over a directory not per-file
  hash_file = os.path.join(result_dir, "xml", "hashes.txt")
  with open(hash_file) as f:
    for test_hash in f:
      test_hash = test_hash.strip()
      if not test_hash: continue
      xml_dir = os.path.join(xml_result_dir, "%s_unique" % test_hash)
      xml_files = [ x for x in os.listdir(xml_dir) if is_xml_file(x) ]
      dot_file = os.path.join(result_dir, "dot", "test_%s.dot" % test_hash)
      assert 0 < len(xml_files)
      cmd = [os.path.join(TOOL_PATH, "gen"), "-Tdot", "-o", dot_file, os.path.join(xml_dir, xml_files[0])]
      if args.verbose: print " ".join(cmd)
      code = subprocess.call(cmd)
      if code != 0:
        print "ERROR: dot generation was unsuccessful"
        return 1
      png_file = os.path.join(result_dir, "png", "test_%s.png" % test_hash)
      cmd = ["dot", "-Tpng", "-o", png_file, dot_file]
      if args.verbose: print " ".join(cmd)
      code = subprocess.call(cmd)
      if code != 0:
        print "ERROR: png generation was unsuccessful"
        return 1

  if platform.system() == "Darwin" and args.batch == False:
    if nsolutions == 1:
      for f in os.listdir(png_result_dir):
        subprocess.Popen(["open", os.path.join(png_result_dir, f)])
    else:
      subprocess.Popen(["open", os.path.join(png_result_dir)])

  if args.expect:
    if args.expect != nsolutions:
      print "ERROR: Expected %d unique solutions, found %d" % (args.expect, nsolutions)
      return 1
      
  return 0

if __name__ == '__main__':
  sys.exit(main())
