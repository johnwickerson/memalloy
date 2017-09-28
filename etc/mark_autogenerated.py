import sys

def add_watermark(file_name):
    lines = open(file_name, 'r').readlines()
    newlines = []
    for line in lines:
        newlines.append("/* Automatically generated. Do not edit! */\n")
        newlines.append(line)
    out = open(file_name, 'w')
    out.writelines(newlines)
    out.close()

file_name = sys.argv[1]
add_watermark(file_name)
