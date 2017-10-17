ALL_TESTS=x86_not_sc \
	c11_partial_not_lidbury \
	c11_simp_not_sra \
	c11_orig_not_simp_4ev \
	c11_orig_not_simp_5ev \
	c11_scdrf_bug \
	ppc_not_mca \
	x86_not_mca \
	compile_opencl_ptx_orig \
	compile_opencl_ptx_cumul \
	c11_simp_not_sra_iter \
	compile_ocaml_ppc_v1 \
	compile_ocaml_ppc_v2 \
	c11_partial_not_lidbury_iter \
	ptx_orig_not_cumul_5ev

SLOW_TESTS=c11_simp_not_swrf_hint \
	ptx_orig_not_cumul_6ev \
	c11_simp_not_swrf \
	arm8_strengthening_6ev \
	c11_repairing0 \
	c11_repairing1 \
	c11_repairing2 \
	c11_repairing3 \
	c11_repairing4 \
	c11_repairing5 \
	compile_c11_ppc \
	compile_c11_arm7_v1 \
	compile_c11_arm7_v2 

c11_partial_not_lidbury:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -expect 1 $(ARGS)

c11_partial_not_lidbury_iter:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_lidbury.cat -satisfies models/c11_partial.cat -satisfies models/c11_normws.cat -events 4 -iter -expect 5 $(ARGS)

x86_not_sc:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch X86 -violates models/sc.cat -satisfies models/x86tso.cat -events 4 -iter -expect 2 $(ARGS)

c11_simp_not_sra:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -expect 1 $(ARGS)

c11_simp_not_sra_iter:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_sra.cat -satisfies models/c11_simp.cat -satisfies models/c11_relacq.cat -satisfies models/c11_normws.cat -satisfies models/c11_simplepost.cat -events 6 -iter -expect 1 $(ARGS)

c11_simp_not_swrf_hint:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -solver plingeling -events 12 -hint execution_hints/nienhuis_example.als -expect 1 $(ARGS)

c11_simp_not_swrf:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_swrf.cat -satisfies models/c11_simp.cat -solver plingeling -events 12 -expect 1 $(ARGS)

c11_orig_not_simp_4ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 4 -expect 0 $(ARGS)

c11_orig_not_simp_5ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_simp.cat -satisfies models/c11_orig.cat -events 5 -expect 1 $(ARGS)

c11_scdrf_bug:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/sc.cat -satisfies models/c11_nodrf.cat -satisfies models/c11_normws.cat -satisfies models/c11_onlysc.cat -events 4 -expect 1 $(ARGS)

ppc_not_mca:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch PPC -fencerels -violates models/mca_ppc.cat -satisfies models/ppc.cat -events 6 -expect 1 -minimal $(ARGS)

x86_not_mca:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch X86 -fencerels -violates models/mca_x86.cat -satisfies models/x86tso.cat -events 7 -expect 0 $(ARGS)

c11_repairing0:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing0.cat -satisfies models/c11_repairing1.cat -satisfies models/c11_noscfences.cat -satisfies models/c11_normws.cat -events 5 -expect 1 $(ARGS)

c11_repairing1:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing2.cat -satisfies models/c11_repairing1.cat -satisfies models/c11_noscfences.cat -satisfies models/c11_normws.cat -events 5 -expect 1 $(ARGS)

c11_repairing2:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing3.cat -satisfies models/c11_repairing2.cat -satisfies models/c11_normws.cat -events 7 -expect 1 $(ARGS)

c11_repairing3:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing3.cat -satisfies models/c11_repairing4.cat -satisfies models/c11_noscfences.cat -satisfies models/c11_normws.cat -events 6 -expect 1 $(ARGS)

c11_repairing4:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing5.cat -satisfies models/c11_repairing4.cat -satisfies models/c11_noscfences.cat -satisfies models/c11_normws.cat -events 6 -expect 1 $(ARGS)

c11_repairing5:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -violates models/c11_repairing3.cat -satisfies models/c11_repairing5.cat -satisfies models/c11_noscfences.cat -satisfies models/c11_normws.cat -events 6 -expect 1 $(ARGS)

ptx_orig_not_cumul_5ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 5 -iter -expect 14 $(ARGS)

ptx_orig_not_cumul_6ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch PTX -fencerels -violates models/ptx_cumul.cat -satisfies models/ptx_orig.cat -satisfies models/normws.cat -satisfies models/ptx_singlegl.cat -events 6 -exact -iter -expect 189 $(ARGS) #takes about 2hrs

compile_c11_ppc:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -arch2 PPC -fencerels -violates models/c11_simp.cat -satisfies models/ppc.cat -mapping mappings/fences_as_relations/c11_ppc.als -events 5 -events2 5 -expect 1 $(ARGS)

compile_c11_arm7_v1:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/nofences.cat -events 5 -events2 6 -expect 1 $(ARGS)

compile_c11_arm7_v2:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch C -arch2 ARM7 -fencerels -violates models/c11_simp.cat -satisfies models/arm7.cat -mapping mappings/fences_as_relations/c11_arm7.als -alsosatisfies models/c11_normws.cat -alsosatisfies models/nofences.cat -events 6 -events2 6 -expect 1 $(ARGS)

compile_opencl_ptx_orig:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_orig.cat -mapping mappings/fences_as_relations/opencl_ptx.als -events 5 -events2 5 -expect 1 $(ARGS)

compile_opencl_ptx_cumul:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch OpenCL -arch2 PTX -fencerels -violates models/opencl_scoped.cat -satisfies models/ptx_cumul.cat -mapping mappings/fences_as_relations/opencl_ptx_buggy.als -events 5 -events2 5 -expect 1 $(ARGS)

compile_ocaml_ppc_v1:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch OCaml -arch2 PPC -fencerels -violates models/ocaml.cat -satisfies models/ppc.cat -alsosatisfies models/ocaml_restrictions.cat -mapping mappings/fences_as_relations/ocaml_ppc_v1.als -events 4 -events2 4 -expect 1 $(ARGS)

compile_ocaml_ppc_v2:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch OCaml -arch2 PPC -fencerels -violates models/ocaml.cat -satisfies models/ppc.cat -alsosatisfies models/ocaml_restrictions.cat -mapping mappings/fences_as_relations/ocaml_ppc_v2.als -events 4 -events2 4 -expect 0 $(ARGS)

TXN_TESTS=weak_vs_strong_atomicity_3ev \
	weak_vs_strong_atomicity_5ev \
	adding_txns_arm8_3ev \
	adding_txns_arm8_4ev

weak_vs_strong_atomicity_3ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch HW -satisfies models/txn_weak_atomicity.cat -satisfies models/sc.cat -violates models/txn_strong_atomicity.cat -events 3 -iter -expect 4 $(ARGS) # takes about 3 seconds

weak_vs_strong_atomicity_5ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch HW -satisfies models/txn_weak_atomicity.cat -satisfies models/sc.cat -violates models/txn_strong_atomicity.cat -events 5 -exact -iter -expect 46 $(ARGS) # takes about 20 seconds

adding_txns_arm8_3ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch ARM8 -satisfies models/aarch64.cat -violates models/aarch64_txn.cat -fencerels -events 3 -iter -expect 10 $(ARGS) # takes about 6 seconds

adding_txns_arm8_4ev:
	@ echo "\n\n=============\nmake $@"
	./comparator -arch ARM8 -satisfies models/aarch64.cat -violates models/aarch64_txn.cat -fencerels -events 4 -iter -expect 139 $(ARGS) # takes about 3 minutes
