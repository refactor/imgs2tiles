-module(tile_builder).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([build/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

build(Tile, {Tx, Ty, Tz}) ->
    gen_server:cast(?MODULE, {build_tile, Tile, {Tx, Ty, Tz}}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({build_tile, Tile, {Tx, Ty, Tz}}, State) ->
    io:format("build tile: ~p~n", [Tile]),
    
    gdal_nifs:build_tile(Tile),
    {ok, DefaultTilesDir} = application:get_env(imgs2tiles, default_tiles_dir),
    {ok, DefaultTileFileExt} = application:get_env(imgs2tiles, default_tilefile_ext),
    TileFilename = filename:join([DefaultTilesDir, 
            integer_to_list(Tz), integer_to_list(Tx), 
            integer_to_list(Ty) ++ "." ++ DefaultTileFileExt]),
    %% Create directories for the tile
    ok = filelib:ensure_dir(TileFilename),

    gdal_nifs:save_tile(Tile, TileFilename),
    {noreply, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

