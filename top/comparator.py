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
import shutil
import sys
import subprocess
import platform

import argparsing
from argparsing import TOOL_PATH
import setup_result_dir
import run_alloy

def ext_of_file(f):
  _unused_basename, ext = os.path.splitext(f)
  return ext

def is_cat_file(f):
  return ext_of_file(f) == ".cat"

def is_als_file(f):
  return ext_of_file(f) == ".als"

def is_xml_file(f):
  return ext_of_file(f) == ".xml"

def try_call(args, cmd, msg):
  if args.verbose: print " ".join(cmd)
  code = subprocess.call(cmd)
  if code != 0:
    print msg
    sys.exit(1)

def main(argv=None):
  if argv is None:
    argv = sys.argv[1:]
  parser = argparse.ArgumentParser(description="Top-level memalloy comparator", conflict_handler="resolve")

  parser.add_argument("-verbose", action="store_true",
                      help="Option: show subcommands and use verbose output")
  parser.add_argument("-expect", type=int, default=None,
                      help="Expect to find this many unique solutions (optional)")
  parser.add_argument("-minimise", action="store_true",
                      help="Minimise any solution(s) found")
  parser.add_argument("-desc", type=str, help="Textual description (optional)")
  parser.add_argument("-batch", action="store_true",
                        help="Option: suppress GUI")
  argparsing.add_setup_result_dir_args(parser)
  argparsing.add_gen_comparator_args(parser)
  argparsing.add_run_alloy_args(parser)
  args = parser.parse_args(argv)

  iterate = args.iter

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
    
  nsolutions = 0

  while True: # "solution-finding loop" 

    # Generate comparator to find any solution that is not a
    # super-execution of one that has been found already
    comparator_name = "comparator_%d.als" % nsolutions
    comparator_script = os.path.join(result_dir, comparator_name)
    cmd = [os.path.join(TOOL_PATH, "pp_comparator"), "-o", comparator_script]
    for i in range(0, nsolutions):
      hint_file = os.path.join(result_dir, "hint_%d.als" % i)
      cmd.extend(["-hint", hint_file])      
    cmd.extend(argparsing.extract_gen_comparator_args(args))
    try_call(args, cmd, "ERROR: Generation of comparator script was unsuccessful")

    # Run Alloy on the generated comparator script
    args.comparator_script = comparator_script
    args.alloystar = "alloystar"
    args.iter = False
    xml_result_dir_ = os.path.join(xml_result_dir, str(nsolutions))
    os.mkdir(xml_result_dir_)
    xml_result_dir_ = os.path.join(xml_result_dir_, str(0))
    os.mkdir(xml_result_dir_)
    args.xml_result_dir = xml_result_dir_
    code = run_alloy.main(args)
    if code != 0:
      print "ERROR: Alloy was unsuccessful"
      return 1

    # Check whether Alloy found a solution
    xml_soln = os.path.join(xml_result_dir_, "test_0.xml")
    if os.path.exists(xml_soln): # "found solution"

      print "Found a solution."

      num_equiv_solns_found = 1

      if not args.minimise and not iterate:
        
        # Copy the .xml file into its grandparent directory
        target_xml = os.path.join(xml_result_dir, "test_0.xml")
        shutil.copyfile(xml_soln, target_xml)
      
        nsolutions = 1
        break
      
      # Convert the solution into an Alloy predicate that will be
      # used as an upper bound when finding smaller solutions
      pred_name = "hint_%d_0" % nsolutions
      hint_file = "%s.als" % pred_name
      hint_path = os.path.join(result_dir, hint_file)
      cmd = [os.path.join(TOOL_PATH, "gen"), "-Tals", "-o", hint_path, "-name", pred_name, "-sub", xml_soln]
      try_call(args, cmd, "ERROR: hint generation was unsuccessful")

      while True: # "solution-minimising loop"

        # Generate comparator to find any solution that is a strict
        # sub-execution of the previously-generated solution
        comparator_name = "comparator_%d_%d.als" % (nsolutions, num_equiv_solns_found)
        comparator_script = os.path.join(result_dir, comparator_name)
        hint = "hint_%d_%d.als" % (nsolutions, num_equiv_solns_found-1)
        hint_path = os.path.join(result_dir, hint)
        cmd = [os.path.join(TOOL_PATH, "pp_comparator"), "-o", comparator_script, "-hint", hint_path]
        cmd.extend(argparsing.extract_gen_comparator_args(args))
        try_call(args, cmd, "ERROR: Generation of comparator script was unsuccessful")

        # Run Alloy on the generated comparator script
        args.comparator_script = comparator_script
        args.alloystar = "alloystar"
        args.iter = False
        xml_result_dir_ = os.path.join(xml_result_dir, str(nsolutions), str(num_equiv_solns_found))
        os.mkdir(xml_result_dir_)
        args.xml_result_dir = xml_result_dir_
        code = run_alloy.main(args)
        if code != 0:
          print "ERROR: Alloy was unsuccessful"
          return 1

        # Check whether Alloy found a solution
        xml_soln = os.path.join(xml_result_dir_, "test_0.xml")
        if os.path.exists(xml_soln): # "found smaller solution"

          print "Found a smaller solution."

          # Convert the solution into an Alloy predicate that will be
          # used as an upper bound when finding further solutions
          pred_name = "hint_%d_%d" % (nsolutions, num_equiv_solns_found)
          hint_file = "%s.als" % pred_name
          hint_path = os.path.join(result_dir, hint_file)
          cmd = [os.path.join(TOOL_PATH, "gen"), "-Tals", "-o", hint_path, "-name", pred_name, "-sub", xml_soln]
          try_call(args, cmd, "ERROR: hint generation was unsuccessful")

          num_equiv_solns_found += 1
          continue

        else:
          print "No more smaller solutions."
          os.rmdir(xml_result_dir_) # directory is empty
          break
        #end "found smaller solution"

      #end "solution-minimising loop"

      # Copy the minimal .xml file into its grandparent directory
      minimal_xml = os.path.join(xml_result_dir, str(nsolutions), str(num_equiv_solns_found-1), "test_0.xml")
      target_xml = os.path.join(xml_result_dir, "test_%d.xml" % nsolutions)
      shutil.copyfile(minimal_xml, target_xml)

      # Convert the solution into an Alloy predicate that stops
      # Alloy finding this execution again, or any of its
      # super-executions
      pred_name = "hint_%d" % nsolutions
      hint_file = "%s.als" % pred_name
      hint_path = os.path.join(result_dir, hint_file)
      cmd = [os.path.join(TOOL_PATH, "gen"), "-Tals", "-o", hint_path, "-name", pred_name, "-super", target_xml]
      try_call(args, cmd, "ERROR: hint generation was unsuccessful")

      nsolutions += 1

      if iterate:
        continue
      else:
        break

    else:
      print "No more solutions found."
      break
    #end "found solution"

  #end "solution-finding loop" 
  
  # Step through the minimal solutions and convert each to dot/png
  soln_ctr = 0
  for xml_file in os.listdir(xml_result_dir):
    if is_xml_file(xml_file):
      dot_file = os.path.join(result_dir, "dot", "test_%d.dot" % soln_ctr)
      cmd = [os.path.join(TOOL_PATH, "gen"), "-Tdot", "-o", dot_file, os.path.join(xml_result_dir, xml_file)]
      try_call(args, cmd, "ERROR: dot generation was unsuccessful")
      png_file = os.path.join(result_dir, "png", "test_%d.png" % soln_ctr)
      cmd = ["dot", "-Tpng", "-o", png_file, dot_file]
      try_call(args, cmd, "ERROR: png generation was unsuccessful")
      soln_ctr +=1

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
