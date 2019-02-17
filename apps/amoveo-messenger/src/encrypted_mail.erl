-module(encrypted_mail).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
cron/0,
new/2, read/1, clean/0, test/0]).
-record(msg, {time, data, to}).
-define(limit, 7200000000).%two hours is 2*60*60*1000000 microseconds
-define(LOC, "encrypted_mail.db").
init(ok) -> 
    process_flag(trap_exit, true),
    utils:init(dict:new(), ?LOC).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    utils:save(X, ?LOC),
    io:format("encrypted_mail died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast(clean, X) -> 
    X2 = clean_helper(X),
    {noreply, X2};
handle_cast({new, Msg, To}, X) -> 
    NM = #msg{time = erlang:timestamp(), data = Msg, to = To},
    L2 = case dict:find(To, X) of
             error -> [NM];
             {ok, L} -> [NM|L]
         end,
    X2 = dict:store(To, L2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, To}, _From, X) -> 
    Y = case dict:find(To, X) of
            error -> [];
            {ok, A} -> get_data(A)
        end,
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

clean() -> gen_server:cast(?MODULE, clean).
cron() ->
    spawn(fun() -> cron2() end).
cron2() ->
    timer:sleep(300000),
    clean(),
    cron2().
new(Msg, To) -> 
    true = utils:valid_address(To),
    gen_server:cast(?MODULE, {new, Msg, To}).
read(To) -> 
    true = utils:valid_address(To),
    gen_server:call(?MODULE, {read, To}).

clean_helper(X) ->
    Keys = dict:fetch_keys(X),
    io:fwrite(packer:pack(Keys)),
    io:fwrite("\n"),
    clean_accounts(Keys, X).
clean_accounts([], X) -> X;%loop over each account in dict
clean_accounts([H|T], X) ->
    A = dict:fetch(H, X),
    A2 = clean_msgs(A),
    X2 = dict:store(H, A2, X),
    clean_accounts(T, X2).
clean_msgs([]) -> [];%loop over each message to this account
clean_msgs([H|T]) ->
    ND = timer:now_diff(erlang:timestamp(), H#msg.time),
    H2 = if
             ND > ?limit -> [];
             true -> [H]
         end,
    H2 ++ clean_msgs(T).
get_data([]) -> [];
get_data([H|T]) ->
    M = H#msg.data,
    [M|get_data(T)].
test() ->
    To = base64:decode("BOuze97rZS1nHwZEEUNZJZmPaILyAViRLMPSVi24EIH1T4fG+fHVDg0eVmhCCprjy+aUhXAQafyR2Rx5Rwt4I08="),
    Msg = <<"abcdef">>,
    new(Msg, To),
    read(To),
    clean().
