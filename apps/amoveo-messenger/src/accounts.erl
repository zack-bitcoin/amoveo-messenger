-module(accounts).
-behaviour(gen_server).
-export([start_link/0,code_change/3,handle_call/3,handle_cast/2,handle_info/2,init/1,terminate/2,
	cron/0,get/1,withdrawal/1,spend/3,height_below/2,
        nonce_increment/1]).
-record(d, {height, accounts}).
-record(acc, {veo = 0, height = 0, nonce = 0}).
-define(LOC, "accounts.db").
init(ok) -> 
    process_flag(trap_exit, true),
    H = case config:mode() of
	    test -> 0;
	    production -> utils:height(veo)
	end,
    R = #d{accounts = dict:new(), height = H},
    utils:init(R, ?LOC).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, ok, []).
code_change(_OldVsn, State, _Extra) -> {ok, State}.
terminate(_, X) -> 
    utils:save(X, ?LOC),
    io:format("accounts died!"), ok.
handle_info(_, X) -> {noreply, X}.
handle_cast({withdrawal, Pubkey}, X) -> 
    A = X#d.accounts,
    X2 = case dict:find(Pubkey, A) of
	     error -> X;
	     {ok, Acc} ->
		 Amount = Acc#acc.veo,
		 utils:spend(veo, Pubkey, Amount),
		 Acc2 = Acc#acc{veo = 0},
		 A2 = dict:store(Pubkey, Acc2, A),
		 X#d{accounts = A2}
	 end,
    {noreply, X2};
handle_cast({update, NewHeight}, X) -> 
    H = X#d.height,
    Pubkey = utils:pubkey(),
    X2 = if
	NewHeight > H ->
		 M = {pubkey, Pubkey, NewHeight - H, NewHeight},%amoveo_utils:address_history
		 {ok, Txs} = talker:talk(M),
		 %io:fwrite("txs "),
		 %io:fwrite(packer:pack(Txs)),
		 %io:fwrite("\n"),
		 A2 = receive_payments(Txs, X#d.accounts, Pubkey),
		 X#d{height = NewHeight, accounts = A2};
	true -> X
    end,
    {noreply, X2};
handle_cast({spend, Pub, Amount, Height}, X) -> 
    Accs = X#d.accounts,
    {_Q, X2} = 
	case dict:find(Pub, Accs) of
	    error -> {<<"account does not exist">>, X};
	    {ok, A} ->
		if
		    Amount > A#acc.veo -> {<<"you don't have enough veo to do that">>, X};
		    true ->
			A2 = A#acc{veo = A#acc.veo - Amount, height = Height},
			Acc2 = dict:store(Pub, A2, Accs),
			X3 = X#d{accounts = Acc2},
			{success, X3}
		end
	end,
    {noreply, X2};
handle_cast(_, X) -> {noreply, X}.
handle_call({get, Pub}, _From, X) -> 
    Accs = X#d.accounts,
    Y = dict:find(Pub, Accs),
    {reply, Y, X};
handle_call({nonce_increment, Pub, N}, _From, X) ->
    Accs = X#d.accounts,
    A = dict:find(Pub, Accs),
    ON = A#acc.nonce,
    if
        N > ON -> 
            A2 = A#acc{nonce = N},
            Accs2 = dict:store(Pub, A2, Accs),
            X2 = X#d{accounts = Accs2},
            {reply, true, X2};
        true -> {reply, false, X}
    end;
handle_call(_, _From, X) -> {reply, X, X}.

withdrawal(Pub) -> gen_server:cast(?MODULE, {withdrawal, Pub}).
spend(Pub, Amount, Height) ->
    gen_server:cast(?MODULE, {spend, Pub, Amount, Height}).
update() -> 
    spawn(fun() ->
		  Height = utils:height(veo),
		  gen_server:cast(?MODULE, {update, Height})
	  end).
get(Pub) -> gen_server:call(?MODULE, {get, Pub}).
nonce_increment(Nonce) -> 
    true = is_integer(Nonce),
    true = Nonce > -1,
    gen_server:call(?MODULE, {nonce_increment, Nonce}).
    
receive_payments([], X, _) -> X;
receive_payments([{_, Tx}|T], X, Pubkey) ->
   %Txs is [{Height, UnsignedTx}...]
   %add the veo to their balance, minus the deposit fee.
    DF = config:deposit_fee(),
    From = utils:spend_from(veo, Tx),
    Amount = utils:spend_amount(veo, Tx),
    X2 = if
	     Pubkey == From -> X;%only look at txs that receive veo.
	     Amount < DF -> X;
	     true ->
		 rp2(From, Amount - DF, X)
		     %dict:store(From, A2, X)
		     %X#d{accounts = A2
    end,
    receive_payments(T, X2, Pubkey).
rp2(From, A, D) ->
    Acc = case dict:find(From, D) of
	      error -> #acc{};
	      {ok, X} -> X
	  end,
    Acc2 = Acc#acc{veo = Acc#acc.veo + A},
    dict:store(From, Acc2, D).

height_below(Pub, H) ->
    {ok, A} = accounts:get(Pub),
    N = A#acc.height,
    H > N.

cron() ->
    spawn(fun() ->
		  timer:sleep(5000),
		  cron2()
	  end).
cron2() ->
    timer:sleep(5000),
    update(),
    cron2().
		  

