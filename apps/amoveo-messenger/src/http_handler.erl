-module(http_handler).
-export([init/3, handle/2, terminate/3, doit/2]).
%example: `curl -i -d '["test"]' http://localhost:8087`

-include("records.hrl").
init(_Type, Req, _Opts) -> {ok, Req, no_state}.
terminate(_Reason, _Req, _State) -> ok.
handle(Req, State) ->
    {ok, Data0, Req2} = cowboy_req:body(Req),
    {{IP, _}, Req3} = cowboy_req:peer(Req2),
    Data = packer:unpack(Data0),
    D0 = doit(Data, IP),
    D = packer:pack(D0),
    Headers=[{<<"content-type">>,<<"application/octet-stream">>},
    {<<"Access-Control-Allow-Origin">>, <<"*">>}],
    {ok, Req4} = cowboy_req:reply(200, Headers, D, Req3),
    {ok, Req4, State}.
doit({price}, _) ->
    {_, P} = posts:price(),
    {ok, P};
%doit({lookup, Start, Many}, _) ->
%    true = is_integer(Start),
%    true = is_integer(Many),
%    true = -1 < Start,
%    true = 0 < Many,
%    {ok, posts:lookup(Start, Many)};
%doit({post, SR}, _) ->
%    io:fwrite("submitting a post\n"),
    %ok = trade_limit:doit(IP),
%    R = element(2, SR),
%    {30, Pubkey, Height, Text, ServerPubkey} = R,
%    ServerPubkey = base64:encode(utils:pubkey()),
%    true = is_integer(Height),
%    true = is_binary(Pubkey),
%    true = is_binary(Text),
%    true = size(Text) < config:max_post_size(),
%    65 = size(Pubkey),
%    {ok, NodeHeight} = packer:unpack(talker:talk_helper({height}, config:full_node(), 10)),
%    true = NodeHeight < Height + 3,
%    true = NodeHeight > Height - 1,
%    Sig = element(3, SR),
%    true = sign:verify_sig(R, Sig, Pubkey),
%    true = accounts:nonce_below(Pubkey, Height),
%    {ID, Price} = posts:new(Text),
%    accounts:spend(Pubkey, Price, Height),
%    {ok, ID};
doit({test}, _) ->
    {ok, <<"success 2">>};
doit({account, X}, _) ->
    case accounts:get(X) of
	{ok, A} -> {ok, A};
	error -> {ok, 0}
    end;
doit({height}, _) ->
    talker:talk({height});
doit({pubkey}, _) -> talker:talk({pubkey});
doit({spend, SR}, _) ->
    %this withdraws your entire balance from the account.
    R = element(2, SR),
    {53410, Pubkey, Nonce} = R,
    Sig = element(3, SR),
    true = sign:verify_sig(R, Sig, Pubkey),
    true = accounts:nonce_increment(Nonce),
    accounts:withdrawal(Pubkey),
    {ok, 0};
doit({send, 0, To, SR}, _) -> %for encrypted messages
    {signed, {53412, From, Nonce, Emsg}, Sig, _} = SR,
    R = element(2, SR),
    true = sign:verify_sig(R, Sig, From),
    true = accounts:nonce_increment(From, Nonce),
    Price = config:encrypted_message_fee(),
    {ok, Height} = talker:talk({height}),
    accounts:spend(From, Price, Height),%charge a fee
    encrypted_mail:new(Emsg, To),
    {ok, 0};
doit({read, 0, To}, _) ->
    X = encrypted_mail:read(To),
    {ok, X};
doit({send, 1, To, SR}, _) -> %for channel-close-tx messages
    {signed, {53411, From, Nonce, Stx}, Sig, _} = SR,
    R = element(2, SR),
    true = sign:verify_sig(R, Sig, From),
    true = accounts:nonce_increment(From, Nonce),
    {ok, Height} = talker:talk({height}),
    Price = config:channel_close_message_fee(),
    accounts:spend(From, Price, Height),%charge a fee
    From = channel_close_mail:new(Stx, To),
    {ok, 0};
doit({read, 1, To}, _) ->
    X = channel_close_mail:read(To),
    {ok, X};
doit(X, _) ->
    io:fwrite("http handler cannot handle this "),
    io:fwrite(packer:pack(X)),
    io:fwrite("\n"),
    {ok, <<"error">>}.
    
