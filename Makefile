srcs=src/Main.hs src/MainScript.hs src/Send.hs src/Types.hs src/Shallow.hs src/Deep.hs

no_goal_given:
	@echo "Please choose one of the following targets:"
	@echo -e "\tshallow, deep, execute"
	@echo "(Note: exec requires either deep or shallow to have been built first)"
	@echo
	@exit 2

shallow: clean $(srcs)
	cd src ; \
		ghc Main.hs -threaded

deep: clean $(srcs)
	cd src ; \
		hermit-shell Main.hs +Main MainScript.hs resume -- -threaded

exec: src/Main
	@echo "Starting server at localhost:3000..."
	@echo "This server can be terminated with Ctrl-c"
	cd src ; \
		./Main

src/Main:
	@echo "Before running exec, you must run either 'make shallow' or 'make deep'"
	@echo
	@exit 3

clean:
	cd src ; \
		rm -f *.o *.hi *.dyn_hi *.dyn_o Main

