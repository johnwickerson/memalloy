import string
import sys

def replace_line(file_name, line_num):
    lines = open(file_name, 'r').readlines()
    before = '../archs/'
    after = '../../archs/fences_as_relations/'
    lines[line_num] = string.replace(lines[line_num], before, after)
    out = open(file_name, 'w')
    out.writelines(lines)
    out.close()

file_name = sys.argv[1]
replace_line(file_name, 0)
replace_line(file_name, 1)
