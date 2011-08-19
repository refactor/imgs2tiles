-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).
-export([get_meta/1]).

-on_load(init/0).

init() ->
    erlang:load_nif("./gdal_nifs", 0).

open(_Filename) ->
    "NIF library not loaded".

close(_Handle) ->
    "NIF library not loaded".

get_meta(_Handle) ->
    "NIF library not loaded".
