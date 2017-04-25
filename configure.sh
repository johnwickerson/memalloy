# Uncomment one of the following lines:

#export OS=x86-freebsd
#export OS=x86-linux
export OS=x86-mac
#export OS=x86-windows
#export OS=amd64-linux

# Set the Java maximum heap size (default is 3 gigabytes)
export JAVA_HEAP_SIZE=3g

# MEMALLOY_ROOT_DIR is the directory that contains this script
export MEMALLOY_ROOT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
