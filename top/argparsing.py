import argparse
import os
import distutils.spawn

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

MEMALLOY_ROOT_DIR = os.path.abspath(os.path.join(os.path.realpath(__file__), os.pardir, os.pardir))

def check_dependencies():
  if not distutils.spawn.find_executable("dot"):
    raise Exception("Need to have dot for converting executions into images")
check_dependencies()
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
  parser.add_argument("-unroll", type=int, default=3,
                        help="Unroll fixpoints")

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
  parser.add_argument('-alsosatisfies', action='append', metavar='<M>', default=[],
                      help='Execution should also satisfy this model (repeatable)')
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
  parser.add_argument("-minlocations", type=int,
                      help="Find executions with at least N locations (default 0)")
  parser.add_argument("-maxlocations", type=int,
                      help="Find executions with at most N locations")
  parser.add_argument("-locations", type=int,
                      help="Find executions with exactly N locations")
  parser.add_argument("-minimal", action='store_true',
                      help="Option: find minimal executions")
  parser.add_argument("-withinit", action='store_true',
                      help="Option: explicit initial writes")
  parser.add_argument("-exact", action='store_true',
                        help="Option: solution(s) must use exactly the given number of events")

def ignore_opt(option_value):
  return option_value == None or (type(option_value) == bool and option_value == False)

def extract_gen_comparator_args(args):
  d = vars(args)
  cmd_options = []
  for model in d["satisfies"]:
    cmd_options.extend(["-satisfies", model])
  for model in d["alsosatisfies"]:
    cmd_options.extend(["-alsosatisfies", model])
  for model in d["violates"]:
    cmd_options.extend(["-violates", model])
  for opt in ["arch", "events", "mapping", "arch2",
                "events2", "hint", "minthreads", "maxthreads",
                "threads", "minlocations", "maxlocations",
                "locations"]:
    if not ignore_opt(d[opt]):
      cmd_options.extend(["-" + opt, str(d[opt])])
  if args.withinit:
    cmd_options.extend(["-withinit"])
  if args.fencerels:
    cmd_options.extend(["-fencerels"])
  if args.iter or args.minimal:
    cmd_options.extend(["-minimal"])
  if args.exact:
    cmd_options.extend(["-exact"])
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
