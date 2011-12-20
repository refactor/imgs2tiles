-module(tc_sup).

-behaviour(supervisor).

-export([start_link/0, start_child/2]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(TileInfo, ImgFileName) ->
    supervisor:start_child(?SERVER, [TileInfo, ImgFileName]).

init([]) ->
    tc_store:init(),
    Element = { tile_collector, {tile_collector, start_link, []},
                temporary, brutal_kill, worker, [tile_collector]},
    Children = [Element],
    RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, {RestartStrategy, Children}}.

