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
from shutil import copyfile, rmtree
import sys
from subprocess32 import call, Popen, TimeoutExpired
import platform
from timeit import default_timer as timer
import collections

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
  parser.add_argument("-customscript", type=str, help="Custom comparator script (optional)")
  argparsing.add_setup_result_dir_args(parser)
  argparsing.add_gen_comparator_args(parser)
  argparsing.add_run_alloy_args(parser)
  argparsing.add_remove_dups_args(parser, is_standalone=False)
  args = parser.parse_args(argv)

  timing = collections.OrderedDict()

  # Stage 1: setup directory structure and generate comparator script
  start = timer()
  result_dir = setup_result_dir.main(args)
  xml_result_dir = os.path.join(result_dir, "xml")
  dot_result_dir = os.path.join(result_dir, "dot")
  png_result_dir = os.path.join(result_dir, "png")
  lit_result_dir = os.path.join(result_dir, "litmus")
  allow_result_dir = os.path.join(result_dir, "allow")
  allow_xml_result_dir = os.path.join(result_dir, "allow", "xml")
  allow_dot_result_dir = os.path.join(result_dir, "allow", "dot")
  allow_png_result_dir = os.path.join(result_dir, "allow", "png")
  allow_lit_result_dir = os.path.join(result_dir, "allow", "litmus")

  if args.desc:
    print "\"" + args.desc + "\""

  comparator_script = os.path.join(result_dir, "comparator.als")

  if args.customscript:

    if not os.path.exists(args.customscript):
      print "ERROR: Custom comparator script '%s' not found" % args.customscript
      return 1
    copyfile(args.customscript, comparator_script)

  else:

    # Deal with non-local models by copying them into models/_tmp_<n>.{cat,als}
    # and patching the command-line arguments
    nonlocal_model_map = {} # for temporaries
    all_models = args.satisfies + args.alsosatisfies + args.violates
    if args.filter: all_models = all_models + [args.filter]
    for model in all_models:
      model_path = os.path.abspath(model)
      if not os.path.dirname(model_path).startswith(argparsing.MEMALLOY_ROOT_DIR):
        tmp_cat = os.path.join("models", "_tmp_%d%s" % (len(nonlocal_model_map), ext_of_file(model)))
        nonlocal_model_map[model] = tmp_cat
        copyfile(model_path, os.path.join(argparsing.MEMALLOY_ROOT_DIR, tmp_cat))
        model = os.path.join(argparsing.MEMALLOY_ROOT_DIR, tmp_cat)
      if is_cat_file(model):
        if args.fencerels:
          cmd = [os.path.join(TOOL_PATH, "cat2als"), "-fencerels", model]
        else:
          cmd = [os.path.join(TOOL_PATH, "cat2als"), model]
        cmd.extend(["-u", str(args.unroll)])
        code = call(cmd)
        if code != 0:
          print "ERROR: Unable to convert cat file"
          return 1
      elif is_als_file(model):
        pass
      else:
        print "ERROR: Unrecognised model type [%s]" % model
        return 1

    if nonlocal_model_map:
      for k in ["satisfies", "alsosatisfies", "violates"]:
        for i,m in enumerate(args.__dict__[k]):
          if m in nonlocal_model_map:
            new_m = nonlocal_model_map[m]
            if args.verbose: print "Patching %s |-> %s" % (m, new_m)
            args.__dict__[k][i] = new_m

    cmd = [os.path.join(TOOL_PATH, "pp_comparator"), "-o", comparator_script]
    cmd.extend(argparsing.extract_gen_comparator_args(args))
    if args.verbose: print " ".join(cmd)
    code = call(cmd)
    if code != 0:
      print "ERROR: Generation of comparator script was unsuccessful"
      return 1
  end = timer()
  timing['setup'] = (end-start)
    
  #end if args.customscript

  # Stage 2: Alloy solving
  start = timer()
  args.comparator_script = comparator_script
  args.alloystar = "alloystar"
  args.xml_result_dir = xml_result_dir
  try:
    code = run_alloy.main(args)
  except KeyboardInterrupt:
    code = 0
    print "\nWARNING: Alloy was interrupted"
  except TimeoutExpired:
    code = 0
    print "\nWARNING: Alloy timeout (only partial solutions)"
  if code != 0:
    print "ERROR: Alloy was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if is_xml_file(x)])
  end = timer()
  print "Alloy found %d solutions in %.2f sec" % (nsolutions, end-start)
  timing['alloy'] = (end-start)
  
  if nsolutions == 0:
    if args.expect and args.expect != 0:
      print "ERROR: Expected %d unique solutions, found 0" % args.expect
      return 1
    else:
      return 0
 
  # Stage 3: Remove duplicates
  print "Remove duplicates"
  start = timer()
  code = remove_dups.main(args)
  if code != 0:
    print "ERROR: Remove duplicates script was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if x.endswith("_unique")])
  end = timer()
  print "Partitioned to %d unique solutions in %.2f sec" % (nsolutions, end-start)
  timing['rmdups'] = (end-start)

  # Stage 3a: Filter executions to remove those that are inconsistent according to the model given in the -filter parameter
  if args.filter:
    print "Filter executions"
    start = timer()
    hash_file = os.path.join(xml_result_dir, "hashes.txt")
    hash_file_keep = os.path.join(xml_result_dir, "hashes_keep.txt")
    hash_file_ignore = os.path.join(xml_result_dir, "hashes_ignore.txt")
    open(hash_file_keep, 'a').close()
    open(hash_file_ignore, 'a').close()
    nfiltered = 0    
    with open(hash_file) as f:
      for test_hash in f:
        test_hash = test_hash.strip()
        if not test_hash: continue
        xml_dir = os.path.join(xml_result_dir, "%s_unique" % test_hash)
        xml_files = [ x for x in os.listdir(xml_dir) if is_xml_file(x) ]
        assert 0 < len(xml_files)

        hint_file = os.path.join(result_dir, "test_%s.als" % test_hash)
        cmd = [os.path.join(TOOL_PATH, "gen"), "-Tals", \
                 "-name", "test_%s" % test_hash, \
                 "-o", hint_file, \
                 os.path.join(xml_dir, xml_files[0])]
        if args.verbose: print " ".join(cmd)
        code = call(cmd)
        if code != 0:
          print "ERROR: als generation was unsuccessful"
          return 1
        tmp_comparator_script = os.path.join(result_dir, "tmp_comparator.als")
        cmd = [os.path.join(TOOL_PATH, "pp_comparator"), \
                 "-arch", args.arch, \
                 "-satisfies", args.filter, \
                 "-events", str(args.events), \
                 "-hint", hint_file, \
                 "-o", tmp_comparator_script]
        if args.fencerels: cmd.extend(["-fencerels"])
        if args.verbose: print " ".join(cmd)
        code = call(cmd)
        if code != 0:
          print "ERROR: generating a comparator to check whether a generated execution should be filtered"
          return 1
        os.remove(hint_file)
        tmp_xml_result_dir = os.path.join(result_dir, "tmp_xml")
        if os.path.isdir(tmp_xml_result_dir):
          rmtree(tmp_xml_result_dir)
        os.mkdir(tmp_xml_result_dir)
        args.comparator_script = tmp_comparator_script
        args.xml_result_dir = tmp_xml_result_dir
        args.iter = False
        code = run_alloy.main(args)
        if code != 0:
          print "ERROR: checking whether a generated execution should be filtered was unsuccessful"
          return 1
        num_solutions = len([x for x in os.listdir(tmp_xml_result_dir) if is_xml_file(x)])
        if num_solutions == 1:
          print "Execution %s is consistent under baseline." % test_hash
          with open(hash_file_keep, "a") as f_keep:
            print >>f_keep, test_hash
        else:
          print "Execution %s is also inconsistent under baseline -- remove!" % test_hash
          with open(hash_file_ignore, "a") as f_ignore:
            print >>f_ignore, test_hash
          nfiltered = nfiltered + 1
    end = timer()
    print "Filtered down to %d solutions in %.2f sec" % (nsolutions - nfiltered, end-start)
    timing['filtering'] = (end-start)
  
  # Stage 3b: Generate allow-set
  if args.allowset:
    print "Generate allow-set"
    start = timer()
    cmd = [os.path.join(TOOL_PATH, "weaken"), \
             "-o", allow_xml_result_dir, \
             xml_result_dir]
    if args.verbose: print " ".join(cmd)
    code = call(cmd)
    if code != 0:
      print "ERROR: generating allowed executions was unsuccessful"
      return 1
    nsolutions = len([x for x in os.listdir(allow_xml_result_dir) if is_xml_file(x)])
    print "Constructed %d allowed variants" % nsolutions 
    print "Remove duplicates from allow-set"
    args.xml_result_dir = allow_xml_result_dir
    remove_dups.UNIQUE = {}
    code = remove_dups.main(args)
    if code != 0:
      print "ERROR: removing duplicates from allow-set was unsuccessful"
      return 1
    nsolutions = len([x for x in os.listdir(allow_xml_result_dir) if x.endswith("_unique")])
    end = timer()
    print "Partitioned to %d unique solutions in %.2f sec" % (nsolutions, end-start)
    timing['allowset'] = (end-start)
  
  # Stage 4: Generate litmus test output
  start = timer()
  result_dir_list = [result_dir]
  if args.allowset:
    result_dir_list = [result_dir, allow_result_dir]
  for my_result_dir in result_dir_list:
    print "Converting in %s" % my_result_dir
    hash_file = os.path.join(my_result_dir, "xml", "hashes.txt")
    #litmus_filenames = []
    with open(hash_file) as f:
      for test_hash in f:
        test_hash = test_hash.strip()
        if not test_hash: continue
        xml_dir = os.path.join(my_result_dir, "xml", "%s_unique" % test_hash)
        xml_files = [ x for x in os.listdir(xml_dir) if is_xml_file(x) ]
        assert 0 < len(xml_files)

        als_file = os.path.join(my_result_dir, "als", "test_%s.als" % test_hash)
        cmd = [os.path.join(TOOL_PATH, "gen"), "-Tals", \
                 "-name", "test_%s" % test_hash, \
                 "-o", als_file, \
                 os.path.join(xml_dir, xml_files[0])]
        if args.verbose: print " ".join(cmd)
        code = call(cmd)
        if code != 0:
          print "ERROR: als generation was unsuccessful"
          return 1
        
        dot_file = os.path.join(my_result_dir, "dot", "test_%s.dot" % test_hash)
        cmd = [os.path.join(TOOL_PATH, "gen"), "-Tdot", "-o", dot_file, os.path.join(xml_dir, xml_files[0])]
        if args.verbose: print " ".join(cmd)
        code = call(cmd)
        if code != 0:
          print "ERROR: dot generation was unsuccessful"
          return 1
      
        png_file = os.path.join(my_result_dir, "png", "test_%s.png" % test_hash)
        cmd = ["dot", "-Tpng", "-o", png_file, dot_file]
        if args.verbose: print " ".join(cmd)
        code = call(cmd)
        if code != 0:
          print "ERROR: png generation was unsuccessful"
          return 1

        litmus = "test_%s.litmus" % test_hash
        #litmus_filenames.append(litmus)

        archs = [args.arch]
        if args.arch == "HW":
          archs = ["ARM8", "PPC", "X86"]
        for arch in archs:
          lit_file = os.path.join(my_result_dir, "litmus", arch, litmus)
          cmd = [os.path.join(TOOL_PATH, "gen"), "-Tlit", \
                "-arch", arch, \
                "-o", lit_file, \
                os.path.join(xml_dir, xml_files[0])]
          if args.verbose: print " ".join(cmd)
          code = call(cmd)
          if code != 0:
            print "ERROR: litmus-test generation was unsuccessful"
            return 1

          
  # litmus7 @all
  archs = [args.arch]
  if args.arch == "HW":
    archs = ["ARM8", "PPC", "X86"]
  for arch in archs:
    
    litmus_filenames = []
    hashes_txt = "hashes_keep.txt" if args.filter else "hashes.txt"
    hash_file = os.path.join(result_dir, "xml", hashes_txt)
    with open(hash_file) as f:
      for test_hash in f:
        test_hash = test_hash.strip()
        litmus = "test_%s.litmus" % test_hash
        litmus_filenames.append(litmus)
    with open(os.path.join(result_dir, "litmus", arch, "@all"), "w+") as f:
      for test in litmus_filenames:
        print >>f, test
    if args.allowset:
      litmus_filenames = []
      hash_file = os.path.join(allow_result_dir, "xml", "hashes.txt")
      with open(hash_file) as f:
        for test_hash in f:
          test_hash = test_hash.strip()
          litmus = "test_%s.litmus" % test_hash
          litmus_filenames.append(litmus)
      with open(os.path.join(allow_result_dir, "litmus", arch, "@all"), "w+") as f:
        for test in litmus_filenames:
          print >>f, test

  # Stage 5: Check the allow-set
  if args.allowset and len(args.violates) == 1:
      print "Checking that the allow-set executions are allowed"
      allow_comparator_script = os.path.join(result_dir, "allow_comparator.als")
      hash_file = os.path.join(allow_result_dir, "xml", "hashes.txt")
      with open(hash_file) as f:
        for test_hash in f:
          sys.stdout.flush()
          test_hash = test_hash.strip()
          if not test_hash: continue
          hint_file = os.path.join(allow_result_dir, "als", "test_%s.als" % test_hash)
          cmd = [os.path.join(TOOL_PATH, "pp_comparator"), \
                "-arch", args.arch, \
                "-satisfies", (args.violates)[0], \
                "-events", str(args.events), \
                "-hint", hint_file, \
                "-o", allow_comparator_script]
          if args.fencerels: cmd.extend(["-fencerels"])
          if args.verbose: print " ".join(cmd)
          code = call(cmd)
          if code != 0:
            print "ERROR: generating a comparator to check that the allow-set executions are allowed was unsuccessful"
            return 1
          tmp_xml_result_dir = os.path.join(allow_result_dir, "tmp_xml")
          if os.path.isdir(tmp_xml_result_dir):
            rmtree(tmp_xml_result_dir)
          os.mkdir(tmp_xml_result_dir)
          args.comparator_script = allow_comparator_script
          args.xml_result_dir = tmp_xml_result_dir
          args.iter = False
          code = run_alloy.main(args)
          if code != 0:
            print "ERROR: checking that the allow-set executions are allowed was unsuccessful"
            return 1
          num_solutions = len([x for x in os.listdir(tmp_xml_result_dir) if is_xml_file(x)])
          if num_solutions == 1:
            print "Execution %s in allow-set is indeed consistent." % test_hash
          else:
            print "ERROR: execution %s in allow-set is inconsistent!" % test_hash
            return
          
  end = timer()
  timing['dump'] = (end-start)

  if platform.system() == "Darwin" and args.batch == False:
    if nsolutions == 1:
      f = os.path.join(lit_result_dir, args.arch, litmus_filenames[0])
      if os.path.isfile(f):
        Popen(["cat", os.path.join(lit_result_dir, f)])
      for f in os.listdir(png_result_dir):
        Popen(["open", os.path.join(png_result_dir, f)])
    else:
      Popen(["open", os.path.join(png_result_dir)])

  if args.expect:
    if args.expect != nsolutions:
      print "ERROR: Expected %d unique solutions, found %d" % (args.expect, nsolutions)
      return 1

  for (k,d) in timing.items():
    print "%s\t%.2f sec" % (k,d)
      
  return 0

if __name__ == '__main__':
  sys.exit(main())
