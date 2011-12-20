-module(tile_collect).

-export([reduce/2, delete/1]).

reduce({_Tile, Tx, Ty, Tz} = TileInfo, ImgFileName) ->
    {ParentQuadtree, _ChildPosition} = mercator_tiles:parent_quadtree(Tx, Ty, Tz),
    Key = ParentQuadtree,
    Pid2 = 
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            Pid;
        {error, _} ->
            {ok, Pid} = tile_collector:create(TileInfo, ImgFileName),
            tc_store:insert(Key, Pid),
            Pid
    end,
    tile_collector:reduce_tile(Pid2, TileInfo, ImgFileName).


delete(Key) ->
    case tc_store:lookup(Key) of
        {ok, Pid} ->
            tile_collector:delete(Pid);
        {error, _Reason} ->
            ok
    end.
