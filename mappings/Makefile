.PHONY: all clean

FAR = fences_as_relations

all:
	@ echo "Adapting each mapping for 'fences as relations' mode."
	@ rm -rf $(FAR)
	@ mkdir $(FAR)
	@ for f in *.als; do \
	  cp $$f $(FAR); \
	  sed -i '1s|../archs/|../../archs/$(FAR)/|' $(FAR)/$$f; \
	  sed -i '2s|../archs/|../../archs/$(FAR)/|' $(FAR)/$$f; \
	done
	@ rm $(FAR)/opencl_amd.als # this mapping is a bit different

clean:
	rm -rf $(FAR)
