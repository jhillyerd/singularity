-module(singularity_ffi).

-export([cons_variant_name/1]).

cons_variant_name(Varfn) ->
  {Name, {_, _}} = Varfn({nil, nil}),
  atom_to_binary(Name).
