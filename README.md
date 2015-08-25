# sunken (Shallow &rarr; Deep)

## Instructions
You can build an executable using either a shallow or deep embedding.

* The shallow embedding can be built with this command:

        make shallow

* The deep embedding can be built with this command

        make deep

Regardless of which embedding was built, it can then be run with `make exec`.
This starts a web server that can be accessed at `http://localhost:3000`. The
web server can be terminated with Ctrl-c.

This page will show a representation of four LEDs. Use the `h` and `j` keys to
interact with the LEDs.
