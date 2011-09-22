%%% -------------------------------------------------------------------
%%% @author wulei <mjollnir.ray@gmail.com>
%%% @copyright 2011
%%% @doc img_scanner: used to scan the original raster imagine file and 
%%%              divide it into primal tiles
%%%              the copyout primal tile will send to tile_builder for 
%%%              generating finished tile
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
-module(img_scanner).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([scan_img/1]).

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

scan_img(ImgFileName) ->
    gen_server:cast(?MODULE, {scan, ImgFileName}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({scan, ImgFileName}, State) ->
    {ok, HI} = gdal_nifs:open(ImgFileName),
    {ElapseTime, ok} = timer:tc(gdal_nifs, generate_base_tiles, [HI]),
    io:format("scan img time: ~p~n", [ElapseTime]),
    gdal_nifs:close(HI),
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

