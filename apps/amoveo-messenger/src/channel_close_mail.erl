-module(channel_close_mail).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
new/3, read/1, clean/0, test/0]).
-record(msg, {time, data, to}).
-define(limit, 1209600000000).%two weeks is 14*24*60*60*1000000 microseconds
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
handle_cast({new, Stx, To}, X) -> 
    %Tx = element(2, Msg),
    %Tx = Msg#msg.data,
    %CID = Tx#ctc.id,
    NM = #msg{time = erlang:timestamp(), data = Stx, to = To},
    L2 = case dict:find(CID, X) of
             error -> [NM];
             {ok, L} -> max2(L, NM)
         end,
    X2 = dict:store(CID, L2, X),
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({read, CID, To}, _From, X) -> 
    Y = case dict:find(CID, X) of
            error -> [];
            {ok, A} -> get_data(A)
        end,
    {reply, Y, X};
handle_call(_, _From, X) -> {reply, X, X}.

clean() -> gen_server:cast(?MODULE, clean).
new(Stx, To) -> 
    Tx = signed:data(Stx),
    CID = element(6, Tx),
    ok = case To of
            Acc1 -> ok;
            Acc2 -> ok;
            _ -> error
        end,
    true = utils:valid_address(To),
    {ok, N} = talker:talk({height}),
    {ok, Header} = talker:ext_talk({header, N}),
    Hash = element(3, Header),
    {ok, X} = talker:ext_talk({proof, <<"channels">>, CID, Hash}),
    Channel = element(4, X),
    CS = element(11, Channel),
    CS = 0,%verifies that the channel is live
    %make sure that To is the opposite of whoever has already signed the tx.
    %make sure that the signature is valid
    gen_server:cast(?MODULE, {new, STx, To}).
read(To) -> 
    true = utils:valid_address(To),
    gen_server:call(?MODULE, {read, To}).

clean_helper(X) ->
    Keys = dict:fetch_keys(X),
    clean_accounts(Keys, X).
clean_accounts([], X) -> X;%loop over each account in dict
clean_accounts([H|T], X) ->
    A = dict:fetch(H, X),
    A2 = clean_msgs(A),
    X2 = dict:store(A2, X),
    clean_accounts(T, X2).
clean_msgs([]) -> [];%loop over each message to this account
clean_msgs([H|T]) ->
    ND = timer:now_diff(erlang:timestamp(), H#msg.time),
    %check if the channel is already closed, if it is, delete it.
    H2 = if
             ND > ?limit -> [];
             true -> [H]
         end,
    H2 ++ clean_msgs(T).
get_data([]) -> [];
get_data([H|T]) ->
    M = H#msg.data,
    [M|get_data(T)].
max2([], X) -> [X]; %only store 1 message each from the two accounts in the channel.
max2([A], X) ->
    XTO = X#msg.to,
    case A#msg.to of 
        XTO -> [X];
        _ -> [X|A]
    end;
max2([A|B], X) ->
    ATO = A#msg.to,
    BTO = B#msg.to,
    case X#msg.to of
        ATO -> [X|B];
        BTO -> [X|A]
    end.

test() ->
    To = base64:decode("BOuze97rZS1nHwZEEUNZJZmPaILyAViRLMPSVi24EIH1T4fG+fHVDg0eVmhCCprjy+aUhXAQafyR2Rx5Rwt4I08="),
    Msg = <<"abcdef">>,
    CID = base64:decode("vVhSBIjO7fU0V4v08WH2O2crgjtl9wTODuIk+jeB2NM="),
    new(Msg, To, CID),
    read(To).
