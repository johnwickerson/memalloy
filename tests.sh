./comparator -desc "c11_simp_not_sra_5ev" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 5 -expect 0
./comparator -desc "c11_simp_not_sra_6ev" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -expect 1
./comparator -desc "c11_simp_not_swrf_7ev" -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -events 7 -expect 0 #slow 
./comparator -desc "c11_simp_not_swrf_12ev" -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -events 12 -expect 1 -solver plingeling #slow
./comparator -desc "c11_orig_not_simp_4ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 4 -expect 0
./comparator -desc "c11_orig_not_simp_5ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 5 -expect 1
./comparator -desc "x86_not_mca" -arch X86 -fencerels -violates models/mca_x86.cat -satisfies models/x86tso.cat -events 7 -expect 0 -solver plingeling
./comparator -desc "ppc_not_mca_5ev" -arch PPC -fencerels -violates models/mca_ppc.cat -satisfies models/ppc.cat -events 5 -expect 0
./comparator -desc "ppc_not_mca_6ev" -arch PPC -fencerels -violates models/mca_ppc.cat -satisfies models/ppc.cat -events 6 -expect 1
./comparator -arch C -desc "c11_scdrf_bug_2ev" -violates models/sc.cat -satisfies models/c11_nodrf.cat -satisfies models/c11_normws.cat -satisfies models/c11_onlysc.cat -events 2 -expect 1
./comparator -desc "ptx_orig_not_cumul_4ev" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 4 -expect 0
./comparator -desc "ptx_orig_not_cumul_5ev" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 5 -expect 1
./comparator -desc "ptx_orig_not_cumul_5ev_iter" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -iter -events 5 -expect 14 #slow
./comparator -desc "ptx_orig_not_cumul_6ev_iter" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -iter -events 6 -exact -expect 189 #slow
./comparator -desc "compile_c11_arm7_4ev" -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/nofences.cat -events 4 -events2 7 -expect 0
./comparator -desc "compile_c11_arm7_5ev" -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/nofences.cat -events 5 -events2 5 -expect 1
./comparator -desc "compile_c11_ppc_4ev" -arch C -arch2 PPC -fencerels -violates models/c11_simp.cat -satisfies models/ppc.cat -mapping mappings/fences_as_relations/c11_ppc.als -events 4 -events2 6 -expect 0
./comparator -desc "compile_c11_ppc_5ev" -arch C -arch2 PPC -fencerels -violates models/c11_simp.cat -satisfies models/ppc.cat -mapping mappings/fences_as_relations/c11_ppc.als -events 5 -events2 5 -expect 1
./comparator -desc "c11_partial_not_lidbury" -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -expect 1
./comparator -desc "c11_partial_not_lidbury_iter" -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -iter -expect 5
./comparator -desc "compile_opencl_ptx_orig" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_orig.cat -mapping mappings/fences_as_relations/opencl_ptx.als -events 5 -events2 5 -expect 1
./comparator -desc "compile_opencl_ptx_cumul" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_cumul.cat -mapping mappings/fences_as_relations/opencl_ptx_buggy.als -events 5 -events2 5 -expect 1
./comparator -desc "c11_simp_not_sra_iter" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -iter -expect 1
