srcs=src/Main.hs src/MainScript.hs src/Send.hs src/Types.hs src/Shallow.hs src/Deep.hs

no_goal_given:
	@echo "Please choose one of the following targets:"
	@echo -e "\tshallow, deep, exec"
	@echo "(Note: exec requires either deep or shallow to have been built first)"
	@echo
	@exit 2

shallow: clean $(srcs)
	cd src ; \
		ghc Main.hs -threaded -O2

# XXX: Why is it necessary for this rule
#      to compile all the .o files first and we can't
#      just make clean instead?
deep: clean $(srcs)
	cd src ; \
		ghc -fno-code -fobject-code Main.hs -threaded -O2 ; \
		hermit-shell Main.hs +Main MainScript.hs resume -- -threaded

exec: src/Main
	@echo "Starting server at localhost:3000..."
	@echo "This server can be terminated with Ctrl-c"
	cd src ; \
		./Main

src/Main:
	@echo "Before running 'make exec', you must run either 'make shallow' or 'make deep'"
	@echo
	@exit 3

clean:
	cd src ; \
		rm -f *.o *.hi *.dyn_hi *.dyn_o Main

