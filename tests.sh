./comparator -desc "c11_simp_not_sra_5ev" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 5 -expect 0
./comparator -desc "c11_simp_not_sra_6ev" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -expect 1
./comparator -desc "c11_simp_not_sra_iter" -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -iter
./comparator -desc "c11_simp_not_swrf_7ev" -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -events 7 -expect 0 #slow 
./comparator -desc "c11_simp_not_swrf_12ev_hint" -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -events 12 -solver plingeling -hint execution_hints/nienhuis_example.als -expect 1
./comparator -desc "c11_simp_not_swrf_12ev" -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -events 12 -solver plingeling -expect 1 #slow
./comparator -desc "c11_orig_not_simp_4ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 4 -expect 0
./comparator -desc "c11_orig_not_simp_5ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 5 -expect 1
./comparator -desc "c11_partial_not_lidbury_3ev" -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 3 -expect 0
./comparator -desc "c11_partial_not_lidbury_4ev" -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -expect 1
./comparator -desc "c11_partial_not_lidbury_iter" -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -iter
./comparator -desc "ppc_not_mca_5ev" -arch PPC -fencerels -violates models/mca_ppc.cat -satisfies models/ppc.cat -events 5 -expect 0
./comparator -desc "ppc_not_mca_6ev" -arch PPC -fencerels -violates models/mca_ppc.cat -satisfies models/ppc.cat -events 6 -expect 1
./comparator -arch C -desc "c11_scdrf_bug_4ev" -violates models/sc.cat -satisfies models/c11_nodrf.cat -satisfies models/c11_normws.cat -satisfies models/c11_onlysc.cat -events 4 -expect 1
./comparator -desc "ptx_orig_not_cumul_4ev" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 4 -expect 0
./comparator -desc "ptx_orig_not_cumul_5ev" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 5 -expect 1
./comparator -desc "ptx_orig_not_cumul_5ev_iter" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -iter -events 5
./comparator -desc "ptx_orig_not_cumul_6ev_iter" -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -iter -events 6 -exact -expect 185 #slow
./comparator -desc "c11_strengthening_seq_4ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_simp.cat -mapping mappings/c11_strengthening_seq.als -events 4 -expect 0
./comparator -desc "c11_strengthening_seq_6ev_hint" -arch C -violates models/c11_simp.cat -mapping mappings/c11_strengthening_seq.als -satisfies models/c11_simp.cat -events 6 -solver plingeling -hint execution_hints/c11_strengthening_seq.als -expect 1
./comparator -desc "c11_strengthening_seq_6ev" -arch C -violates models/c11_simp.cat -mapping mappings/c11_strengthening_seq.als -satisfies models/c11_simp.cat -events 6 -solver plingeling -expect 1 #slow
./comparator -desc "c11_strengthening_mo_4ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_simp.cat -mapping mappings/c11_strengthening_mo.als -events 4 -expect 0
./comparator -desc "c11_strengthening_mo_5ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_simp.cat -mapping mappings/c11_strengthening_seq.als -events 5 -expect 0 #slow (about 5 min)
./comparator -desc "c11_strengthening_mo_7ev_hint" -arch C -violates models/c11_simp.cat -satisfies models/c11_simp.cat -mapping mappings/c11_strengthening_mo.als -events 7 -hint execution_hints/c11_strengthening_mo.als -expect 1
./comparator -desc "c11_strengthening_mo_7ev" -arch C -violates models/c11_simp.cat -satisfies models/c11_simp.cat -mapping mappings/c11_strengthening_mo.als -events 7 -expect 1 #slow
./comparator -desc "compile_c11_arm7_4ev" -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/nofences.cat -events 4 -events2 7 -expect 0
./comparator -desc "compile_c11_arm7_5ev" -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/nofences.cat -events 5 -events2 5 -expect 1
./comparator -desc "compile_c11_ppc_4ev" -arch C -arch2 PPC -fencerels -violates models/c11_simp.cat -satisfies models/ppc.cat -mapping mappings/fences_as_relations/c11_ppc.als -events 4 -events2 6 -expect 0
./comparator -desc "compile_c11_ppc_5ev" -arch C -arch2 PPC -fencerels -violates models/c11_simp.cat -satisfies models/ppc.cat -mapping mappings/fences_as_relations/c11_ppc.als -events 5 -events2 5 -expect 1
./comparator -desc "compile_opencl_ptx_orig_4ev" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_orig.cat -mapping mappings/fences_as_relations/opencl_ptx.als -events 4 -events2 6 -expect 0
./comparator -desc "compile_opencl_ptx_orig_5ev" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_orig.cat -mapping mappings/fences_as_relations/opencl_ptx.als -events 5 -events2 5 -expect 1
./comparator -desc "compile_opencl_ptx_cumul_4ev" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_cumul.cat -mapping mappings/fences_as_relations/opencl_ptx_buggy.als -events 4 -events2 6 -expect 0
./comparator -desc "compile_opencl_ptx_cumul_5ev" -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_cumul.cat -mapping mappings/fences_as_relations/opencl_ptx_buggy.als -events 5 -events2 5 -expect 1
./comparator -desc "c11_not_lahav" -arch C -violates models/c11_lahav.cat -satisfies models/c11_normws.cat -events 4 -iter
./comparator -desc "TSO_not_PTSO" -arch X86 -satisfies models/x86tso.cat -violates models/x86ptso.cat -satisfies models/normws.cat -events 4 -iter -expect 15
./comparator -desc "C_not_PC" -arch C -satisfies models/c11_lahav.cat -violates models/c11_lahav_persistent.cat -satisfies models/c11_normws.cat -events 2 -iter -expect 8
./comparator -desc "PC11_sequentialisation" -arch C -violates models/c11_lahav_persistent.cat -satisfies models/c11_lahav_persistent.cat -alsosatisfies models/c11_normws.cat -mapping mappings/c11_strengthening_seq.als -events 3 -expect 1
