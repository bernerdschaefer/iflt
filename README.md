iflt
----

Implementing Functional Languages: a Tutorial

http://research.microsoft.com/en-us/um/people/simonpj/Papers/pj-lester-book/

Miranda
=======

The code of Implementing Functional Languages
is implemented in [Miranda].

  [Miranda]: http://miranda.org.uk/

The provided [default.nix](./default.nix)
can be used with `nix-shell`
to get a development environment:

    NIXPKGS_ALLOW_UNFREE=1 nix-shell . -A ifltEnv

Be sure to read the Miranda [license]
before running.

  [license]: http://www.cs.kent.ac.uk/people/staff/dat/miranda/downloads/license.html
