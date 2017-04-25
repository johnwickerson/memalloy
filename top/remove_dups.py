#!/usr/bin/env python
# -*- coding: utf-8 -*-

# MIT License
# 
# Copyright (c) 2017 by Tyler Sorensen and John Wickerson
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
import xml.etree.ElementTree as ET
import os
import shutil
import itertools
import pdb
import sys
import hashlib
import progressbar

# To store one file name per unique solution
UNIQUE = {}

# Number of events is passed in as an argument
EVENTS = None

# Directory of .xml files to be partitioned is passed in as an argument
PARTITION_DIR = None

# Say a lot
VERBOSE = False

# The events in the .xml file
BASE_ARR = None

# Different event types to check for

# Doesn't need to contain "A" because we have screl and scacq. 
# Doesn't need to contain "F" because we have "*mb*"
# Doesn't need to contain "ev" because we only partition executions of the same size (i.e. number of events)
# The order of these makes a difference. Put things that are likely to fail fast first
E_TYPES = ["W", "R", "F", "isb", "dmb", "dmbst", "dmbld", "scacq", "screl", "T", "naL", "IW", "A", "acq", "rel", "sc", "locked", "wg", "dv", "sy", "fga", "G", "L", "entry_fence", "exit_fence", "isync", "sync", "lwsync", "eieio", "membar_sys", "membar_gl", "membar_cta"]

# We don't need sthread because we have sb
# We don't need sloc because have rf and co (I think)
# The order of these makes a difference. Put things that are likely to fail fast first
E_RELS = ["rf", "co", "sb", "atom", "stxn", "sloc", "sthd", "ad", "dd", "cd", "swg", "sdv", "sbar", "scta", "sgl"]

def print_to_file(s,fname):
    f = open(fname,'w')
    f.write(s)
    f.close()

def get_results_prefix():
    #return os.path.join(PARTITION_DIR,"partitioned_executions")
    return PARTITION_DIR
    #return "../test_output"

def get_instance(fname):
    base_tree = ET.parse(fname)
    base_root = base_tree.getroot()
    base_instance = base_root[0]
    return base_instance
    
def file_to_str(fname):
    f = open(fname)
    data = f.read();
    f.close()
    return data

def get_types_xml(ins, name):
    ret = []
    for child in ins.iter('field'):
        if child.attrib['label'] == name:
            for reads in child:
                if reads.tag == 'tuple':
                    label = reads[1].attrib['label']
                    ret.append(label)
    return ret

def get_rels_xml(ins, name):
    ret = []
    for child in ins.iter('field'):
        if child.attrib['label'] == name:
            for reads in child:
                if reads.tag == 'tuple':
                    label1 = reads[1].attrib['label']
                    label2 = reads[2].attrib['label']
                    ret.append((label1,label2))
    return ret

# Assuming sb is total here, not the case for C!!
def sort_by_sb(events, my_inst):
    sb_rel = get_rels_xml(my_inst, "sb")
    ordered = set()
    ret = []
    index = 0
    while len(ret) != len(events):
        for i in range(len(events)):
            for e in events:
                before = [evt for evt in events if (evt,e) in sb_rel]
                if set(before) == set(ordered):
                    ret.append(e)
                    ordered.add(e)
                    index = index + 1

    return ret
    
def threads_and_sb(my_inst):
    threads = []
    sthd_rel = get_rels_xml(my_inst, "sthd")
    for b in BASE_ARR:
        d = [x[0] for x in sthd_rel]
        if b in d:
            same_thread = list(set([x[0] for x in sthd_rel if b == x[0] or b == x[1]]))
            same_thread = sort_by_sb(same_thread, my_inst)
            sthd_rel = [x for x in sthd_rel if x[0] not in same_thread and x[1] not in same_thread]
            threads.append(same_thread)
            
    return threads

def check_and_get_perm(s_threads, p):
    new_arr = []
    for i in p:
        new_arr.append(s_threads[i])


    for i in range(len(new_arr) - 1):
        if len(new_arr[i]) > len(new_arr[i + 1]):
            return []
    return new_arr

def enumerate_orderings(threads):
    ret = []
    thread_ids = [x for x in range(len(threads))]
    s_threads = sorted(threads,key=len)
    
    # Check all permutations of threads and throw away ones that don't work
    # There's probably a better way to do this, but this works for now.
    perms = itertools.permutations(thread_ids)
    for p in perms:
        # Returns an empty list if it isn't a valid
        # permutation
        p_threads = check_and_get_perm(s_threads, p)
        if len(p_threads) != 0:
            ret.append(p_threads)

    return ret

def get_maps(orderings):
    ret = []
    for l in orderings:
        flattened = [item for sublist in l for item in sublist]
        mapped = {x:"E$" + str(i) for i, x in enumerate(flattened)}
        ret.append(mapped)
    return ret
    
def get_exec_string_from_map(m, my_inst):
    ret = ""
    for E in E_TYPES:
        ret = ret + E + "\n"
        events_of_type = get_types_xml(my_inst, E)
        ret = ret + " ".join(sorted([m[e] for e in events_of_type])) + "\n"

    for E in E_RELS:
        ret = ret + E + "\n"
        events_of_rel = get_rels_xml(my_inst, E)
        ret = ret + " ".join(sorted([m[e[0]] + "->" + m[e[1]]for e in events_of_rel])) + "\n"

    return ret
            
def get_exec_strings_from_maps(maps, my_inst):
    ret = []
    for m in maps:
        ret.append(get_exec_string_from_map(m, my_inst))

    return ret;        

def get_hash_from_instance(my_inst):
    threads = threads_and_sb(my_inst)
    different_orderings = enumerate_orderings(threads)
    different_maps = get_maps(different_orderings)
    exec_strings = get_exec_strings_from_maps(different_maps, my_inst)
    #pdb.set_trace()
    hashed = sorted([str((hashlib.md5(x)).hexdigest()) for x in exec_strings])
    
    # Second hash to account for multiple possible hashes
    new_str = "+".join(hashed)
    return str(hashlib.md5(new_str).hexdigest())
    
def get_hash_from_file(fname):
    return get_hash_from_instance(get_instance(fname))
    
def found_unique(fname, hash_str):
    if VERBOSE: print "Found unique! " + str(len(UNIQUE)) + " with hash " + hash_str
    dir_name = os.path.join(get_results_prefix(), hash_str + "_unique")
    os.makedirs(dir_name)
    new_name = os.path.join(dir_name, os.path.basename(fname))
    shutil.move(fname, dir_name)
    inst = get_instance(new_name)
    UNIQUE[new_name] = hash_str
    return

# determine if a file is unique or not and move it to 
# the appropriate place if unique
def partition_file(fname):

    # Get the execution hash
    hash_str = get_hash_from_file(fname)

    # The first one is unique
    if len(UNIQUE) == 0:        
        found_unique(fname, hash_str)
        return

    # Else compare to the other unique solutions
    found = False

    for f in UNIQUE:
        hash_str_unique = UNIQUE[f]
        if hash_str == hash_str_unique:
            shutil.move(fname, os.path.join(os.path.dirname(f)))
            return
            
    found_unique(fname, hash_str)
    return

def partition_dir():
    dir_name = PARTITION_DIR
    processed = 0
    xmls = os.listdir(PARTITION_DIR)
    total = float(len(xmls))

    bar = progressbar.ProgressBar(maxval=100, widgets=[progressbar.Bar('=', '[', ']'), ' ', progressbar.Percentage()])
    bar.start()
    for i in xmls:
        # Process each .xml file
        if i.endswith(".xml"):
            processed = processed + 1
            percent_f = (processed / total) * 100
            if VERBOSE:
                percent_str = " [processed %.2f%%]" % percent_f
                print "partitioning file: " + i  + "  " + percent_str
            else:
                bar.update(int(percent_f))
            partition_file(os.path.join(PARTITION_DIR,i))
    bar.finish()

def main(args):
  global EVENTS
  global PARTITION_DIR
  global BASE_ARR
  global VERBOSE
  EVENTS = args.events + args.events2
  PARTITION_DIR = args.xml_result_dir
  BASE_ARR = ["E$%d" % x for x in range(EVENTS)]
  VERBOSE = args.verbose
  try:
    partition_dir()
    hashes = "\n".join(sorted([UNIQUE[f] for f in UNIQUE]))
    print_to_file(hashes, os.path.join(get_results_prefix(), "hashes.txt"))
    return 0
  except:
    return 1

if __name__ == '__main__':
  parser = argparse.ArgumentParser(description="Remove duplicates from xml results dir")
  argparsing.add_remove_dups_args(parser, is_standalone=True)
  args = parser.parse_args(sys.argv[1:])
  args.events2 = 0
  sys.exit(main(args))
