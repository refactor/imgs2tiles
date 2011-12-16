-module(scan_monitor).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

-define(TABLE_ID, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

-export([
        begin_scan/2,
        report_scan/0,
        report_scan/1,
        checkout_scan/2
        ]).

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


begin_scan(ImgFile, ScanTime) ->
    gen_server:cast(?SERVER, {begin_scan, ImgFile, ScanTime}).

checkout_scan(ImgFile, CheckoutTime) ->
    gen_server:cast(?SERVER, {checkout_scan, ImgFile, CheckoutTime}).

report_scan() ->
    io:format("TODO~n"),
    ok.


report_scan(ImgFile) ->
    {HandleTime, Count} = gen_server:call(?SERVER, {end_scan, ImgFile}),
    io:format("handle time: ~p, build tiles: ~p~n", [HandleTime, Count]).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    ets:new(?TABLE_ID, [public, named_table]),
    {ok, Args}.

handle_call({end_scan, ImgFile}, _From, State) ->
    Res = 
    case ets:lookup(?TABLE_ID, ImgFile) of
        [] ->
            error;
        [{ImgFile, BeginScanTime, BuildEndTime, Count}] ->
            {timer:now_diff(BuildEndTime, BeginScanTime)/1000000, Count}
    end,
    {reply, Res, State};
handle_call(_Request, _From, State) ->
    {noreply, ok, State}.

handle_cast({begin_scan, ImgFile, BeginScanTime}, State) ->
    ets:insert(?TABLE_ID, {ImgFile, BeginScanTime, BeginScanTime, 0}),
    {noreply, State};
handle_cast({checkout_scan, ImgFile, BuildEndTime}, State) ->
    case ets:lookup(?TABLE_ID, ImgFile) of
        [] ->
            error;
        [{ImgFile, BeginScanTime, _LastScanTime, Count}] ->
            ets:insert(?TABLE_ID, {ImgFile, BeginScanTime, BuildEndTime, Count+1}),
            ok
    end,
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

