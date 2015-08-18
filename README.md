# sunken (Shallow -> Deep)

## Instructions
You can build an executable using either a shallow or deep embedding. Inside the `src` directory:

* The shallow embedding can be built with this command:

        ./clean.sh && ghc Main.hs -threaded

* The deep embedding can be built with this command

        ./clean.sh && hermit-shell Main.hs +Main MainScript.hs resume -- -threaded

Regardless of which embedding was built, it can then be run with `./Main`. This starts a web server than can be accessed at localhost:3000.
