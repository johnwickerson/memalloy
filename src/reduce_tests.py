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

# TODO:
# 1. Currently just handles binary reductions, that is, if
# one relation/annotation is in one test and missing from another.
# It should probably handle "strengths" as well. That is, one annoation
# can be reduced to another

import xml.etree.ElementTree as ET
import os
import shutil
import itertools
import pdb
import sys
import hashlib
import copy

# Number of events is passed in as an argument
EVENTS = int(sys.argv[1])

# Directory of .xml files to be partitioned is passed in as an argument
REDUCE_DIR = sys.argv[2]

# The events in the .xml file
BASE_ARR = ["E$%d" % x for x in range(EVENTS)]

REDUCED_HASHES = {}

HASH_METRIC = {}

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

REDUCE_RELS = ["ad", "dd", "cd", "stxn", "atom"]

REDUCE_TYPES = ["T", "scacq", "screl", "A", "acq", "rel", "sc", "locked"]

def print_to_file(s,fname):
    f = open(fname,'w')
    f.write(s)
    f.close()

def get_results_prefix():
    return REDUCE_DIR

INSTANCE_HASH = {}
def get_instance(fname):
    global INSTANCE_HASH
    if fname in INSTANCE_HASH:
        return INSTANCE_HASH[fname]
    base_tree = ET.parse(fname)
    base_root = base_tree.getroot()
    base_instance = base_root[0]
    INSTANCE_HASH[fname] = base_instance
    return base_instance
    
def file_to_str(fname):
    f = open(fname)
    data = f.read();
    f.close()
    return data

TYPES_HASH = {}
def get_types_xml(ins, name):
    global TYPES_HASH
    if (ins, name) in TYPES_HASH:
        return TYPES_HASH[(ins, name)]
    ret = []
    for child in ins.iter('field'):
        if child.attrib['label'] == name:
            for reads in child:
                if reads.tag == 'tuple':
                    label = reads[1].attrib['label']
                    ret.append(label)
    TYPES_HASH[(ins, name)] = ret
    return ret

RELS_HASH = {}
def get_rels_xml(ins, name):
    global RELS_HASH
    if (ins,name) in RELS_HASH:
        return RELS_HASH[(ins,name)]
    ret = []
    for child in ins.iter('field'):
        if child.attrib['label'] == name:
            for reads in child:
                if reads.tag == 'tuple':
                    label1 = reads[1].attrib['label']
                    label2 = reads[2].attrib['label']
                    ret.append((label1,label2))
    RELS_HASH[(ins,name)] = ret
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
    
def get_exec_string_from_map(m, my_inst, black_list_types, black_list_rels):
    ret = ""
    for E in [x for x in E_TYPES if x not in black_list_types]:
        ret = ret + E + "\n"
        events_of_type = get_types_xml(my_inst, E)
        ret = ret + " ".join(sorted([m[e] for e in events_of_type])) + "\n"

    for E in [x for x in E_RELS if x not in black_list_rels]:
        ret = ret + E + "\n"
        events_of_rel = get_rels_xml(my_inst, E)
        ret = ret + " ".join(sorted([m[e[0]] + "->" + m[e[1]]for e in events_of_rel])) + "\n"

    return ret
            
def get_exec_strings_from_maps(maps, my_inst, black_list_types, black_list_rels):
    ret = []
    for m in maps:
        ret.append(get_exec_string_from_map(m, my_inst, black_list_types, black_list_rels))

    return ret;        

def get_hash_from_instance(my_inst):
    threads = threads_and_sb(my_inst)
    different_orderings = enumerate_orderings(threads)
    different_maps = get_maps(different_orderings)
    exec_strings = get_exec_strings_from_maps(different_maps, my_inst, REDUCE_TYPES, REDUCE_RELS)
    hashed = sorted([str((hashlib.md5(x)).hexdigest()) for x in exec_strings])
    
    # Second hash to account for multiple possible hashes
    new_str = "+".join(hashed)
    return str(hashlib.md5(new_str).hexdigest())
    
def get_hash_from_file(fname):
    return get_hash_from_instance(get_instance(fname))
    
def found_unique(fname, hash_str):
    print "Found unique! " + str(len(UNIQUE)) + " with hash " + hash_str
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

def get_rep_file(h):
    f = os.listdir(os.path.join(REDUCE_DIR, h + "_unique"))[0]
    assert(".xml" in f)
    return os.path.join(REDUCE_DIR, h + "_unique", f)

def can_compare(hash1, hash2):
    global REDUCED_HASHES
    return REDUCED_HASHES[hash1] == REDUCED_HASHES[hash2]

def get_reduced_hash(h):
    global REDUCED_HASHES
    global HASH_METRIC
    xml_file1 = get_rep_file(h)
    new_hash1 = get_hash_from_file(xml_file1)
    REDUCED_HASHES[h] = new_hash1
    inst = get_instance(xml_file1)
    metric = 0
    for e in REDUCE_RELS:
        metric = metric + len(get_rels_xml(inst, e))

    for e in REDUCE_TYPES:
        metric = metric + len(get_types_xml(inst, e))
    
    HASH_METRIC[h] = metric


def get_hashes(dir_name):    
    return [x for x in file_to_str(os.path.join(dir_name,"hashes.txt")).split("\n") if x != ""]

def get_mappings(inst):
    threads = threads_and_sb(inst)
    different_orderings = enumerate_orderings(threads)
    different_maps = get_maps(different_orderings)
    return different_maps

def find_eq_map(inst1, inst2, mappings1, mappings2):
    exec_strings1 = get_exec_strings_from_maps(mappings1, inst1, REDUCE_TYPES, REDUCE_RELS)
    exec_strings2 = get_exec_strings_from_maps(mappings2, inst2, REDUCE_TYPES, REDUCE_RELS)
    str1 = exec_strings1[0]
    str2 = ""
    index = 0
    for e in exec_strings2:
        if e == str1:
            str2 = e
            break
        index = index + 1

    assert(str2 == str1 and get_exec_string_from_map(mappings1[0], inst1, REDUCE_TYPES, REDUCE_RELS) == get_exec_string_from_map(mappings2[index], inst2, REDUCE_TYPES, REDUCE_RELS))
    return mappings1[0], mappings2[index]

def is_subset_or_eq(l1, l2):
    return set(l1) <= set(l2)

def is_exec_less_than_inst_map(inst1, inst2, map1, map2):

    for rel in REDUCE_RELS:
        rels1 = [map1[e[0]] + "->" + map1[e[0]] for e in get_rels_xml(inst1, rel)]
        rels2 = [map2[e[0]] + "->" + map2[e[0]] for e in get_rels_xml(inst2, rel)]
        if not is_subset_or_eq(rels1, rels2):
            return False

    for tp in REDUCE_TYPES:
        types1 = [map1[e] for e in get_types_xml(inst1, tp)]
        types2 = [map2[e] for e in get_types_xml(inst2, tp)]
        if not is_subset_or_eq(types1, types2):
            return False

    return True

# returns 1 if inst1 < inst2
#        -1 if inst1 > inst2
#         0 if neither                                
def compare_exec_inst(inst1, inst2):
    #first thing to do is find the mapping between executions
    mappings1 = get_mappings(inst1)
    mappings2 = get_mappings(inst2)
    map1,map2 = find_eq_map(inst1, inst2, mappings1, mappings2)

    if is_exec_less_than_inst_map(inst1, inst2, map1, map2):
        return 1
    elif is_exec_less_than_inst_map(inst2, inst1, map2, map1):
        return -1
    return 0        

# returns 1 if xml1 < xml2
#        -1 if xml1 > xml2
#         0 if neither
def compare_exec_xml(xml1, xml2):
    inst1 = get_instance(xml1)
    inst2 = get_instance(xml2)
    return compare_exec_inst(inst1, inst2)

# returns 1 if h1 < h2
#        -1 if h1 > h2
#         0 if neither
def compare_exec_hash(h1, h2):
    xml_file1 = get_rep_file(h1)
    xml_file2 = get_rep_file(h2)
    return compare_exec_xml(xml_file1, xml_file2)

def get_percent_str(i1,i2):
    p = (float(i1)/float(i2)) * 100.0
    return "%.2f" % p

def partition_hashes(hashes):
    ret = {}
    for h in hashes:
        if REDUCED_HASHES[h] not in ret:
            ret[REDUCED_HASHES[h]] = [h]
        else:
            ret[REDUCED_HASHES[h]].append(h)

    return ret

def cmp_hashes(h1, h2):
    return HASH_METRIC[h1] > HASH_METRIC[h2]

def write_new_hash(new_hashes):
    hashes = "\n".join(sorted(new_hashes))
    print_to_file(hashes, os.path.join(get_results_prefix(), "hashes.txt"))
    
def rm_test_hash(h):
    shutil.rmtree(os.path.join(get_results_prefix(), h + "_unique"))
    return 0

def reduce_dir():
    dir_name = REDUCE_DIR
    hashes = get_hashes(dir_name)
    reduced_tests = 0
    compared_tests = 0
    total_tests = len(hashes)
    final_hashes = copy.deepcopy(hashes)
    #print hashes

    print "collecting reduced hashes"
    
    for h in hashes:
        if h != "":
            get_reduced_hash(h)

    print "comparing hashes"

    #hashes.sort(cmp_hashes)

    i = 0
    while i < len(hashes):
        h = hashes[i]
        print "checking test " + h + "[" + get_percent_str(compared_tests,len(hashes)) + "%]"
        j = i + 1
        while j < len(hashes):
            c = hashes[j]
            if not can_compare(h,c):
                j = j + 1
                continue
            comp = compare_exec_hash(c,h)
            if comp == 1:
                print "  test " + h + " can be reduced to test " + c
                print "  removing test " + h
                reduced_tests = reduced_tests + 1
                final_hashes.remove(h)
                rm_test_hash(h)
                break
            if comp == -1:
                print "  test " + c + " can be reduced to test " + h
                print "  removing test " + c
                reduced_tests = reduced_tests + 1
                hashes.remove(c)
                rm_test_hash(c)
                final_hashes.remove(c)
                continue
            j = j + 1
                    
        compared_tests = compared_tests + 1
        i = i + 1;
                       

    print "reduced " + str(reduced_tests) + " out of " + str(total_tests)
    write_new_hash(final_hashes)
            

reduce_dir()

#hashes = "\n".join(sorted([UNIQUE[f] for f in UNIQUE]))
#print_to_file(hashes, os.path.join(get_results_prefix(), "hashes.txt"))
