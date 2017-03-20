./comparator \
    -desc "Finding a bug in an OpenCL/PTX mapping (old PTX model)" \
    -arch OpenCL \
    -arch2 PTX \
    -violates models/opencl_scoped.cat \
    -satisfies models/ptx_orig.cat \
    -mapping mappings/opencl_ptx.als \
    -events 5 \
    -events2 5 \
    -expect 1
