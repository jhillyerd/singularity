-module(singularity_ffi).

-export([cons_variant_name/1, get_act_variant_name/1]).

cons_variant_name(Varfn) ->
  get_act_variant_name(Varfn({nil, nil})).

get_act_variant_name(Wrapped) ->
  {Name, _} = Wrapped,
  atom_to_binary(Name).
