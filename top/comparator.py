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
import datetime
import distutils.spawn
import os
import platform
import shutil
import subprocess
import sys
import tempfile

import remove_dups
import reduce_tests

MEMALLOY_ROOT_DIR = os.environ.get('MEMALLOY_ROOT_DIR')
def check_dependencies():
  if not MEMALLOY_ROOT_DIR:
    raise Exception("Need to have MEMALLOY_ROOT_DIR env variable; please source configure.sh")
  if not distutils.spawn.find_executable("dot"):
    raise Exception("Need to have dot for converting executions into images")
check_dependencies()
#TOOL_PATH=os.path.join(MEMALLOY_ROOT_DIR, "top")
TOOL_PATH=MEMALLOY_ROOT_DIR

SOLVERS = ["sat4j", "cryptominisat", "glucose",
           "plingeling", "lingeling", "minisatprover", "minisat"]

def is_existing_dir(arg):
  if os.path.isdir(arg):
    return os.path.abspath(arg)
  raise argparse.ArgumentParser(
      "Base result directory does not exist [%s]" % arg)

def add_common_args(parser):
  parser.add_argument("-verbose", action="store_true")
  parser.add_argument("-fencerels", action="store_true")

def add_setup_result_dir_args(parser):
  add_common_args(parser)
  default_result_dir = os.path.join(MEMALLOY_ROOT_DIR, "results")
  if not os.path.exists(default_result_dir):
    os.makedirs(default_result_dir)
  parser.add_argument("-base_result_dir", type=is_existing_dir,
                      default=default_result_dir,
                      help="Results will be placed in base_result_dir/<stamp>")
  parser.add_argument("-arch_dir", type=is_existing_dir,
                      default=os.path.join(MEMALLOY_ROOT_DIR, "archs"),
                      help="Arch als directory")

def add_gen_comparator_args(parser):
  add_common_args(parser)
  parser.add_argument('-satisfies', action='append', metavar='<M>', default=[],
                      help='Execution should satisfy this model (repeatable)')
  parser.add_argument('-violates', action='append', metavar='<N>', default=[],
                      help='Execution should satisfy this model (repeatable)')
  parser.add_argument("-arch", type=str, required=True,
                      help="Type of executions being compared (required)")
  parser.add_argument("-events", metavar='<n>', type=int, required=True,
                      help="Max number of events")
  parser.add_argument("-mapping", type=str, default=None,
                      help="An .als file representing a mapping between executions")
  parser.add_argument("-arch2", type=str,
                      help="Type of target execution (required iff -mapping is given)")
  parser.add_argument("-events2", metavar='<n>', type=int, default=0,
                      help="Max number of target events (required iff -mapping is given)")
  parser.add_argument("-hint", type=str, default=None,
                      help="An .als file containing a 'hint[X]' predicate (optional)")
  parser.add_argument("-minthreads", type=int,
                      help="Find executions with at least N threads (default 0)")
  parser.add_argument("-maxthreads", type=int,
                      help="Find executions with at most N threads")
  parser.add_argument("-threads", type=int,
                      help="Find executions with exactly N threads")
  parser.add_argument("-minlocations", type=int, default=0,
                      help="Find executions with at least N locations (default 0)")
  parser.add_argument("-maxlocations", type=int, default=None,
                      help="Find executions with at most N locations")
  parser.add_argument("-locations", type=int,
                      help="Find executions with exactly N locations")
  parser.add_argument("-minimal", action='store_true',
                      help="Option: find minimal executions")
  parser.add_argument("-withinit", action='store_true',
                      help="Option: explicit initial writes")

def ignore_opt(option_value):
  return option_value == None or (type(option_value) == bool and option_value == False)

def extract_gen_comparator_args(args):
  d = vars(args)
  cmd_options = []
  for model in d["satisfies"]:
    cmd_options.extend(["-satisfies", model])
  for model in d["violates"]:
    cmd_options.extend(["-violates", model])
  for opt in ["arch", "events", "mapping", "arch2",
                "events2", "hint", "minthreads", "maxthreads",
                "threads", "minlocations", "maxlocations",
                "locations", "minimal", "withinit"]:
    if not ignore_opt(d[opt]):
      cmd_options.extend(["-" + opt, str(d[opt])])
  if args.fencerels:
    cmd_options.extend(["-fencerels"])
  return cmd_options

def add_run_alloy_args(parser):
  add_common_args(parser)
  parser.add_argument("-solver", choices=SOLVERS, default="glucose",
                      help="Which SAT solver to use (optional). One of: %s" % ", ".join(SOLVERS))
  parser.add_argument("-iter", action='store_true',
                      help="Option: find all solutions")
  parser.add_argument("-alloystar_dir", type=is_existing_dir,
                      default=os.path.join(MEMALLOY_ROOT_DIR, "alloystar"),
                      help="Option: location of root alloystar dir")

def add_remove_dups_args(parser, is_standalone):
  add_common_args(parser)
  if is_standalone:
    parser.add_argument("-events", metavar='<n>', type=int, required=True,
                        help="Max number of events")
    parser.add_argument("xml_result_dir", type=is_existing_dir,
                        help="Input directory of xml files")

def ext_of_file(f):
  _unused_basename, ext = os.path.splitext(f)
  return ext

def setup_result_dir(args):
  timestamp = "{:%y%m%d-%H%M%S}".format(datetime.datetime.now())
  try:
    #abspath = tempfile.mkdtemp(prefix=timestamp, dir=args.base_result_dir)
    abspath = os.path.join(args.base_result_dir, timestamp)
    os.mkdir(abspath)
    latest_symlink = os.path.join(args.base_result_dir, "_latest")
    if os.path.exists(latest_symlink):
      os.remove(latest_symlink)
    os.symlink(abspath, latest_symlink)
    for d in ["xml", "dot", "png"]:
      path = os.path.join(abspath, d)
      os.mkdir(path)
    #shutil.copytree(args.arch_dir, os.path.join(abspath, "als", "archs"))
    return abspath
  except OSError as e:
    print("ERROR: could not create dir structure")
    return None
  
def run_alloy(args):
  if not os.path.exists(args.comparator_script):
    parser.error("ERROR: could not find [%s]" % args.comparator_script)
  comparator_script = os.path.abspath(args.comparator_script)
  if not os.path.isdir(args.xml_result_dir):
    parser.error("ERROR: output dir [%s] not found" % args.xml_result_dir)
  xml_result_dir = os.path.abspath(args.xml_result_dir)
  prevdir = os.getcwd()
  os.chdir(args.alloystar_dir)
  os.environ['SOLVER'] = args.solver
  alloy_cmd = "./runalloy_%s.sh" % ("iter" if args.iter else "once")
  cmd = [alloy_cmd, comparator_script, "0", xml_result_dir]
  if args.verbose:
    print " ". join(cmd)
  code = subprocess.call(cmd)
  os.chdir(prevdir)
  return code

def main(argv=None):
  if argv is None:
    argv = sys.argv[1:]
  parser = argparse.ArgumentParser(description="Top-level memalloy comparator", conflict_handler="resolve")

  parser.add_argument("-verbose", action="store_true",
                      help="Option: show subcommands and use verbose output")
  parser.add_argument("-expect", type=int, default=None,
                      help="Expect to find this many unique solutions (optional)")
  parser.add_argument("-desc", type=str, help="Textual description (optional)")
  add_setup_result_dir_args(parser)
  add_gen_comparator_args(parser)
  add_run_alloy_args(parser)
  add_remove_dups_args(parser, is_standalone=False)
  args = parser.parse_args(argv)

  result_dir = setup_result_dir(args)
  #als_result_dir = os.path.join(result_dir, "als")
  xml_result_dir = os.path.join(result_dir, "xml")
  dot_result_dir = os.path.join(result_dir, "dot")
  png_result_dir = os.path.join(result_dir, "png")

  for model in args.satisfies + args.violates:
    if ext_of_file(model) == ".cat":
      #code = subprocess.call([os.path.join(TOOL_PATH, "cat2als"), "-o", als_result_dir, model])
      if args.fencerels:
        code = subprocess.call([os.path.join(TOOL_PATH, "cat2als"), "-fencerels", model])
      else:
        code = subprocess.call([os.path.join(TOOL_PATH, "cat2als"), model])
      if code != 0:
        print "ERROR: Unable to convert cat file"
        return 1
    # TODO: will not understand imports made by als
    elif ext_of_file(model) == ".als":
      pass
      #shutil.copy(model, als_result_dir)
    else:
      print "ERROR: Unrecognised model type [%s]" % model
      return 1

  #comparator_script = os.path.join(als_result_dir, "comparator.als")
  comparator_script = os.path.join(result_dir, "comparator.als")
  cmd = [os.path.join(TOOL_PATH, "pp_comparator"), "-o", comparator_script]
  cmd.extend(extract_gen_comparator_args(args))
  if args.verbose: print " ".join(cmd)
  code = subprocess.call(cmd)
  if code != 0:
    print "ERROR: Generation of comparator script was unsuccessful"
    return 1

  args.comparator_script = comparator_script
  args.alloystar = "alloystar"
  args.xml_result_dir = xml_result_dir
  code = run_alloy(args)
  if code != 0:
    print "ERROR: Alloy was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if ext_of_file(x) == ".xml"])
  print "Alloy found %d solutions" % nsolutions

  print "Remove duplicates"
  code = remove_dups.main(args)
  if code != 0:
    print "ERROR: Remove duplicates script was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if x.endswith("_unique")])
  print "Partitioned to %d unique solutions" % nsolutions

  print "Reducing tests"
  code = reduce_tests.main(args)
  if code != 0:
    print "ERROR: Reduce tests script was unsuccessful"
    return 1
  nsolutions = len([x for x in os.listdir(xml_result_dir) if x.endswith("_unique")])
  print "Reduced to %d weakest solutions" % nsolutions

  # TODO: gen changed to operate over a directory not per-file
  hash_file = os.path.join(result_dir, "xml", "hashes.txt")
  with open(hash_file) as f:
    for test_hash in f:
      test_hash = test_hash.strip()
      if not test_hash: continue
      xml_dir = os.path.join(xml_result_dir, "%s_unique" % test_hash)
      xml_files = [ x for x in os.listdir(xml_dir) if ext_of_file(x) == ".xml" ]
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

  if platform.system() == "Darwin":
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
