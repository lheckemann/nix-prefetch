# nix-prefetch

## Purpose

- Use Haskell for a real-world tool rather than just uni assignments
- Provide a prefetching tool that's useful for nixpkgs's `fetchFromGitHub`
- Output as nix so it can be copied straight into a nix file


## To do

- Output expressions with no free variables, instead filling them from a function
  argument so the output can be written to a standalone nix file which can then
  be used from other expressions. The current output requires context.
- Better command line UI and error handling
- Proper packaging (currently built using `nix-shell --run 'ghc nix-prefetch'`)
- Support more fetching functions
- Make code less ugly
