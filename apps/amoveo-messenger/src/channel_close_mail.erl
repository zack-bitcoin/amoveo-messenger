-module(channel_close_mail).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
new/2, read/2, clean/0, test/0]).
-record(msg, {time, data, to, cid}).
-record(ctc, {aid1 = 0, aid2 = 0, fee = 0,
	      nonce = 0, id = 0, amount = 0}).
-record(signed, {data="", sig="", sig2=""}).
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
    Tx = signed:data(Stx),
    CID = Tx#ctc.id,
    NM = #msg{time = erlang:timestamp(), data = Stx, to = To, cid = CID},
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
    {reply, Y, X}.
%handle_call(_, _From, X) -> {reply, X, X}.

clean() -> gen_server:cast(?MODULE, clean).
new(Stx, To) -> 
    Tx = signed:data(Stx),
    Acc1 = Tx#ctc.aid1,
    Acc2 = Tx#ctc.aid2,
    CID = Tx#ctc.id,
    From = case To of %verifies that To is the opposite of whoever has already signed the tx, and that the signature is valid.
               Acc1 -> sign:verify_sig(Tx, Stx#signed.sig2, Acc2),
                       Acc2;
               Acc2 -> sign:verify_sig(Tx, Stx#signed.sig, Acc1),
                       Acc1;
               _ -> error
           end,
    true = utils:valid_address(To),
    live = channel_state(CID),
    gen_server:cast(?MODULE, {new, Stx, To}),
    From.
    
read(CID, To) -> 
    %true = utils:valid_address(To),
    gen_server:call(?MODULE, {read, CID, To}).
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
    if
        ND > ?limit -> clean_msgs(T);
        true ->
            CS = channel_state(H#msg.cid),
            case CS of
                closed -> clean_msgs(T); %check if the channel is already closed, if it is, delete it.
                open -> [H|clean_msgs(T)]
            end
    end.
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
channel_state(CID) ->%needs to be updated not to crash on non-existant channels.
    {ok, N} = talker:talk({height}),
    {ok, Header} = talker:ext_talk({header, N}),
    Hash = element(3, Header),
    {ok, X} = talker:ext_talk({proof, <<"channels">>, CID, Hash}),
    Channel = element(4, X),
    case element(11, Channel) of
        0 -> live;
        1 -> closed
    end.
    
           
test() ->
    To = base64:decode("BOuze97rZS1nHwZEEUNZJZmPaILyAViRLMPSVi24EIH1T4fG+fHVDg0eVmhCCprjy+aUhXAQafyR2Rx5Rwt4I08="),
    Msg = <<"abcdef">>,
    CID = base64:decode("vVhSBIjO7fU0V4v08WH2O2crgjtl9wTODuIk+jeB2NM="),
    new(Msg, To),
    read(CID, To).
