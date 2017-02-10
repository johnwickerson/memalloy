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

import xml.etree.ElementTree as ET
import os
import shutil
import itertools
import pdb
import sys

# To store one file name per unique solution
UNIQUE = {}

# Number of events is passed in as an argument
EVENTS = int(sys.argv[1])

# Directory of .xml files to be partitioned is passed in as an argument
PARTITION_DIR = sys.argv[2]

# The events in the .xml file
BASE_ARR = ["E$%d" % x for x in range(EVENTS)]

# Different event types to check for

# Doesn't need to contain "A" because we have screl and scacq. 
# Doesn't need to contain "F" because we have "*mb*"
# Doesn't need to contain "ev" because we only partition executions of the same size (i.e. number of events)
# The order of these makes a difference. Put things that are likely to fail fast first
E_TYPES = ["W", "R", "F", "isb", "dmb", "dmbst", "dmbld", "scacq", "screl", "naL", "IW", "A", "acq", "rel", "sc", "locked", "wg", "dv", "sy", "fga", "G", "L", "entry_fence", "exit_fence", "isync", "sync", "lwsync", "eieio", "membar_sys", "membar_gl", "membar_cta"]

# We don't need sthread because we have sb
# We don't need sloc because have rf and co (I think)
# The order of these makes a difference. Put things that are likely to fail fast first
E_RELS = ["rf", "co", "sb", "atom", "sloc", "sthd", "ad", "dd", "cd", "swg", "sdv", "sbar", "scta", "sgl"]

def get_results_prefix():
    #return os.path.join(PARTITION_DIR,"partitioned_executions")
    return PARTITION_DIR

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

def print_to_file(s,fname):
    f = open(fname,'w')
    f.write(s)
    f.close()

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

def check_types(base_ins, new_ins, event_map):
    for t in E_TYPES:
        base_types = get_types_xml(base_ins, t)
        new_types = [event_map[x] for x in get_types_xml(new_ins, t)]
        if set(base_types) != set(new_types):
            return False
    return True

def check_rels(base_ins, new_ins, event_map):
    for t in E_RELS:
        base_rels = get_rels_xml(base_ins, t)
        new_rels = get_rels_xml(new_ins, t)        
        new_rels = [(event_map[x[0]],event_map[x[1]]) for x in new_rels]
        if set(base_rels) != set(new_rels):
            return False
    return True

def cmp_xml_map(base_ins, new_ins, event_map):
    if not check_types(base_ins, new_ins, event_map):
        return False
    if not check_rels(base_ins, new_ins, event_map):
        return False
    return True

def check_types_map(base_types, new_types, event_map):
    for t in E_TYPES:
        base_types_inner = base_types[t]
        new_types_inner = new_types[t]
        new_types_inner = [event_map[x] for x in new_types_inner]
        if set(base_types_inner) != set(new_types_inner):
            return False
    return True

def check_rels_map(base_rels, new_rels, event_map):
    for t in E_RELS:
        base_rels_inner = base_rels[t]
        new_rels_inner = new_rels[t]
        new_rels_inner = [(event_map[x[0]],event_map[x[1]]) for x in new_rels_inner]
        if set(base_rels_inner) != set(new_rels_inner):
            return False
    return True


def cmp_map(base_types, base_rels, new_types, new_rels, event_map):
    if not check_types_map(base_types, new_types, event_map):
        return False
    if not check_rels_map(base_rels, new_rels, event_map):
        return False
    return True

def get_types(cur_ins):
    ret = {}
    for t in E_TYPES:
        ret[t] = set(get_types_xml(cur_ins, t))
    return ret

def get_rels(cur_ins):
    ret = {}
    for t in E_RELS:
        ret[t] = set(get_rels_xml(cur_ins, t))
    return ret

def cmp_xml(new_ins, base_ins, new_types, new_rels, base_types, base_rels):

    #Pull these up out of the loop.
    #base_types = get_types(base_ins)
    #base_rels = get_rels(base_ins)
    
    perms = itertools.permutations(BASE_ARR)
    for p in perms:
        zipped = zip(p, BASE_ARR)
        e_map = {x[0] : x[1] for x in zipped}
        found1 = cmp_map(base_types, base_rels, new_types, new_rels, e_map)

        # for debugging!
        #found2 = cmp_xml_map(base_ins, new_ins, e_map)
        #if found1 != found2:
        #    pdb.set_trace()

        if found1:
            return True

    return False
    
def found_unique(fname):
    print "Found unique! " + str(len(UNIQUE))
    dir_name = os.path.join(get_results_prefix(), str(len(UNIQUE)) + "_unique")
    os.makedirs(dir_name)
    new_name = os.path.join(dir_name, os.path.basename(fname))
    shutil.move(fname, dir_name)
    inst = get_instance(new_name)
    UNIQUE[new_name] = (inst, get_types(inst), get_rels(inst))
    return

# determine if a file is unique or not and move it to 
# the appropriate place if unique
def partition_file(fname):

    # The first one is unique
    if len(UNIQUE) == 0:
        found_unique(fname)
        return

    # Else compare to the other unique solutions
    found = False

    new_ins = get_instance(fname)
    new_types = get_types(new_ins)
    new_rels = get_rels(new_ins)

    for f in UNIQUE:
        base_ins = UNIQUE[f][0]
        base_types = UNIQUE[f][1]
        base_rels = UNIQUE[f][2]
        found = cmp_xml(new_ins, base_ins, new_types, new_rels, base_types, base_rels)
        if found:
            shutil.move(fname, os.path.join(os.path.dirname(f)))
            return
            
    found_unique(fname)
    return

def partition_dir():
    dir_name = PARTITION_DIR
    processed = 0
    xmls = os.listdir(PARTITION_DIR)
    total = float(len(xmls))
    for i in xmls:
        # Process each .xml file
        if i.endswith(".xml"):
            processed = processed + 1
            percent_f = (processed / total) * 100
            percent_str = " [processed %.2f%%]" % percent_f
            print "partitioning file: " + i  + "  " + percent_str
            partition_file(os.path.join(PARTITION_DIR,i))

partition_dir()
