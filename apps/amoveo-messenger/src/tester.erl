-module(tester).
-export([test/0]).

test() ->
    To = base64:decode("BCjdlkTKyFh7BBx4grLUGFJCedmzo4e0XT1KJtbSwq5vCJHrPltHATB+maZ+Pncjnfvt9CsCcI9Rn1vO+fPLIV4="),
    {From, Priv} = sign:new_key(),
    accounts:give_credits(base64:decode(From), 100000000),
    true = accounts:nonce_increment(base64:decode(From), 1),
    %io:fwrite(accounts:get(base64:decode(From))),
    Nonce = 2,
    Emsg = <<"test">>,
    R = {53412, base64:decode(From), Nonce, Emsg},
    Sig = sign:sign(R, base64:decode(Priv)),
    true = sign:verify_sig(R, Sig, base64:decode(From)),
    http_handler:doit({send, 0, To, {signed, R, Sig, []}}, 0),%breaks here
    {ok, [<<"test">>]} = http_handler:doit({read, 0, To}, 0),
    success.
