%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @doc Purpose:  Convert a raster into TMS (Tile Map Service) tiles in a directory or 
%%%              something else as fast as possible.
%%%           - support of global tiles (Spherical Mercator) for compatibility
%%%               with interactive web maps such as Google Maps
%%% 
%%% this is a clone implementent of gdal2tiles.py, but use elang do some 
%%% parallel work for fast speed 
%%% 
%%% gdal2tiles.py is the work of Klokan Petr Pridal, klokan at klokan dot cz
%%%      Web:      http://www.klokan.cz/projects/gdal2tiles/
%%% @end
%%% -------------------------------------------------------------------
%%  Copyright (c) 2011
%%
%%   Permission is hereby granted, free of charge, to any person obtaining a
%%   copy of this software and associated documentation files (the "Software"),
%%   to deal in the Software without restriction, including without limitation
%%   the rights to use, copy, modify, merge, publish, distribute, sublicense,
%%   and/or sell copies of the Software, and to permit persons to whom the
%%   Software is furnished to do so, subject to the following conditions:
%%   
%%   The above copyright notice and this permission notice shall be included
%%   in all copies or substantial portions of the Software.
%%   
%%   THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
%%   OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%%   FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
%%   THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%%   LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
%%   FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
%%   DEALINGS IN THE SOFTWARE.
%% -------------------------------------------------------------------
-module(gdal_nifs).

-export([init/0]).
-export([open/1,
        close/1]).

-export([get_meta/1]).

-export([copyout_tile/3,
         build_tile/1,
         generate_overview_tile/4,
         save_tile/2]).

-include("gdal2tiles.hrl").

-on_load(init/0).

init() ->
    PrivDir = case code:priv_dir(?MODULE) of
                {error, bad_name} ->
                    EbinDir = filename:dirname(code:which(?MODULE)),
                    AppPath = filename:dirname(EbinDir),
                    filename:join(AppPath, "priv");
                Path ->
                    Path
            end,
    erlang:load_nif(filename:join(PrivDir, atom_to_list(?MODULE)), 0).


-spec(open(Filename::string()) -> {ok, #imghandler{}} | {error, string()}).
open(Filename) ->
    case open_img(Filename) of
        {ok, Hdataset} = _Res->
            calc_nodatavalue(Hdataset),
            calc_srs(Hdataset),
            {ok, RasterInfo} = warp_dataset(Hdataset),
            _DataBandsCount = calc_data_bandscount(Hdataset),
            ImgHandler = #imghandler{ img = Hdataset, 
                                      rasterinfo = RasterInfo, 
                                      sizeinfo = {4 * ?TILE_SIZE, ?TILE_SIZE}},
            {ok, ImgHandler};
        {error, _} = Err ->
            Err
    end.


-spec close(#imghandler{}) -> ok.
close(ImgHandler) ->
    close_img(ImgHandler#imghandler.img).


get_meta(Ref) ->
    erlang:error(function_clause, ["NIF library not loaded",Ref]).


%% ---------------------------------------------------
%% private stub nif functions
%% ---------------------------------------------------

-spec open_img(string()) -> {ok, img()} | {error, string()}.
open_img(_Filename) ->
    erlang:nif_error(nif_not_loaded).

-spec close_img(reference()) -> ok.
close_img(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_nodatavalue(reference()) -> ok.
calc_nodatavalue(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_srs(reference()) -> ok.
calc_srs(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec calc_data_bandscount(reference()) -> non_neg_integer().
calc_data_bandscount(_Ref) ->
    erlang:nif_error(nif_not_loaded).

-spec copyout_tile(img(), bandregion(), bandregion()) -> {ok, tile()} | {error, string()}.
copyout_tile(_Img, _R, _W) ->
    erlang:nif_error(nif_not_loaded).

generate_overview_tile(_Tile0, _Tile1, _Tile2, _Tile3) ->
    erlang:nif_error(nif_not_loaded).

-spec build_tile(Tile::tile()) -> ok.
build_tile(_Tile) ->
    erlang:nif_error(nif_not_loaded).

-spec save_tile(Tile::tile(), TileFileName::string()) -> ok.
save_tile(_Tile, _TileFileName) ->
    erlang:nif_error(nif_not_loaded).

-spec warp_dataset(reference()) -> {ok, rasterinfo()}.
warp_dataset(_Ref) ->
    erlang:nif_error(nif_not_loaded).

