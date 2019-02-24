-module(config).
-compile(export_all).

%mode() -> test.
%mode() -> testnet.
mode() -> production.
message_frequency() -> 1.%this is how often each ip address can check the status of a trade.
trade_frequency() -> 0.2.%this is how often each ip address can put trades into the order book.
market_data_frequency() -> 3.%this is how often each ip address can look up the open orders from the order book.
confirmations(veo) -> %this is how many confirmations we wait after a tx is included in a block until we accept the payment as spent.
    TM = mode(),
    case TM of
	test -> 0;
	testnet -> 0;
	_ -> 0
    end;
confirmations(bitcoin) -> 3.
sync_block_period(veo) -> 40000.%in miliseconds.
confirm_tx_period(veo) -> 40000.%in miliseconds.
bitcoin_height_check_delay() -> 20.%seconds
batch_period() -> 1800.%in seconds
scan_history() -> 100.%when turning on the node with empty databases, how far back in the past do you include transactions from?
external_full_node() -> 
    TM = mode(),
    case TM of
	test -> "http://localhost:3010/";
	testnet -> "http://localhost:8070/";
	_ -> "http://localhost:8080/"
    end.
full_node() -> 
    TM = mode(),
    case TM of
	test -> "http://localhost:3011/";
	testnet -> "http://localhost:8071/";
	_ -> "http://localhost:8081/"
    end.
id_lookup_file() -> "id_lookup.db".
file(X) -> atom_to_list(X) ++ ".db".
trade_time_limit() -> 12 * 60 * 60.%12 hours in seconds
    
order_book_stale_period() ->    
    10000.%in miliseconds
min_trade_time() ->%in seconds
    case mode() of
	test -> 0;
	_ -> 60*60
    end.
max_trade_time() ->
    12*60*60.%in seconds
post_fee() ->
    5000000. % 50 mVEO
deposit_fee() ->
    70000.  %0.0007 veo
trade_fee() ->
    5000000. %0.05 veo
trade_fee_refund() ->
    3000000.  %0.03
%from(Tx) -> element(2, Tx).
%amount(Tx) -> element(6, Tx).
trades_cron_period() ->
    case mode() of
	test -> 3000;
	_ -> 40000%40 seconds
    end.
max_post_size() ->
    %in bytes
    2000.
history_to_remember() ->
    100.
target_posts_per_month() ->
    60.
    
encrypted_message_fee() -> 100000.% 1 milibits
channel_close_message_fee() -> 500000.% 5 milibits
    
