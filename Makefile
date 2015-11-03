PROJECT = observer_cli

DEPS = recon

dep_recon = git git://github.com/ferd/recon.git 2.2.1

include erlang.mk


.PHONY: clean_all

clean_all:
	@echo "clean_all start"
	for file_a in `ls ./deps`; do \
	cd ./deps/$$file_a;\
	make clean;\
	cd -;\
	done; \
	make clean
	@echo "clean_all done"


