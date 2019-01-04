
%%%-------------------------------------------------------------------
%% @doc amoveo-feed public API
%% @end
%%%-------------------------------------------------------------------

-module(amoveo_feed_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    inets:start(),
    start_http(),
    accounts:cron(),
    amoveo_feed_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
start_http() ->
    Dispatch =
        cowboy_router:compile(
          [{'_', [{"/:file", file_handler, []},
		  {"/", http_handler, []}
		 ]}]),
    {ok, Port} = application:get_env(amoveo_feed, port),
    {ok, _} = cowboy:start_http(
                http, 100,
                [{ip, {0, 0, 0, 0}}, {port, Port}],
                [{env, [{dispatch, Dispatch}]}]),
    ok.
