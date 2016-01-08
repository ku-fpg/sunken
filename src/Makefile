srcs=Main.hs MainScript.hs Types.hs Shallow.hs Deep.hs
HERMIT_SHELL=hermit-shell Main.hs +Main MainScript.hs
GHC_ARGS=-threaded

no_goal_given:
	@echo
	@echo "Please choose one of the following targets:"
	@echo -e "\tshallow, deep, exec, repl"
	@echo "(Note: exec requires either deep or shallow to have been built first)"
	@echo
	@exit 2

shallow: clean $(srcs)
	ghc Main.hs -threaded -O2

deep: clean $(srcs)
	$(HERMIT_SHELL) resume -- $(GHC_ARGS)

repl: clean $(srcs)
	$(HERMIT_SHELL) -- $(GHC_ARGS)

exec: Main
	@echo "Starting server at localhost:3000..."
	@echo "This server can be terminated with Ctrl-c"
	./Main

Main:
	@echo "Before running 'make exec', you must run either 'make shallow' or 'make deep'"
	@echo
	@exit 3

clean:
	rm -f *.o *.hi *.dyn_hi *.dyn_o Main

