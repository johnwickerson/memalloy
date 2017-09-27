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

import argparsing
import argparse
import os
import sys
from subprocess32 import Popen, TimeoutExpired
import signal

def main(args):
  if not os.path.exists(args.comparator_script):
    parser.error("ERROR: could not find [%s]" % args.comparator_script)
  comparator_script = os.path.abspath(args.comparator_script)
  if not os.path.isdir(args.xml_result_dir):
    parser.error("ERROR: output dir [%s] not found" % args.xml_result_dir)
  xml_result_dir = os.path.abspath(args.xml_result_dir)
  os.environ['SOLVER'] = args.solver
  alloy_cmd = "./runalloy_%s.sh" % ("iter" if args.iter else "once")
  cmd = [alloy_cmd, comparator_script, "0", xml_result_dir]
  if args.verbose:
    print " ". join(cmd)
  with Popen(cmd, cwd=args.alloystar_dir, preexec_fn=os.setsid) as process:
    try:
      process.communicate(timeout=args.timeout)
      return process.returncode
    except KeyboardInterrupt:
      os.killpg(process.pid, signal.SIGINT)
      raise
    except TimeoutExpired:
      if args.verbose: print "Timeout of %d seconds" % args.timeout
      os.killpg(process.pid, signal.SIGINT)
      raise
  assert False

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description="Invoke Alloy on comparator script")
  argparsing.add_run_alloy_args(parser)
  parser.add_argument("comparator_script")
  parser.add_argument("xml_result_dir")
  args = parser.parse_args(sys.argv[1:])
  sys.exit(main(args))
