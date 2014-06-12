-module(elementary).

-behaviour(application).

% Application Callbacks
-export([start/2]).
-export([stop/1]).

% API
-export([open/2]).
-export([get/2]).
-export([close/1]).

-record(bucket, {
    pool,
    endpoint,
    access_key,
    secret_access_key
}).

%--- Application Callbacks ----------------------------------------------------

start(_StartType, _StartArgs) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, self()}.

stop(_State) ->
    ets:delete(?MODULE).

%--- API ----------------------------------------------------------------------

open(Bucket, Options) ->
    AccessKey = get_option(access_key, Options),
    SecretAccessKey = get_option(secret_access_key, Options),
    Endpoint = get_option(endpoint, Options),

    PoolName = pool_name(Bucket),
    Config = #bucket{
        pool = PoolName,
        endpoint = Endpoint,
        access_key = AccessKey,
        secret_access_key = SecretAccessKey
    },
    case ets:insert_new(?MODULE, {Bucket, Config}) of
        true  -> ok;
        false -> error({bucket_already_exists, Bucket})
    end,

    ok = hackney_pool:start_pool(PoolName, [
        {timeout, get_option(connection_timeout, Options, 5000)},
        {max_connections, get_option(max_connections, Options, 20)}
    ]).

get(Bucket, Key) ->
    Config = get_bucket(Bucket),

    Host = [Bucket, <<".">>, Config#bucket.endpoint],
    Path = [<<"/">>, Key],
    URL = [Host, Path],
    Payload = <<>>,
    Options = [{pool, Config#bucket.pool}],
    Headers = [
        {<<"Host">>, Host}
    ],
    AuthHeaders = elementary_signature:headers(
        get,
        Path,
        Headers,
        Payload,
        Config#bucket.access_key,
        Config#bucket.secret_access_key,
        <<"eu-west-1">>,
        <<"s3">>
    ),
    case hackney:get(URL, Headers ++ AuthHeaders, Payload, Options) of
        {ok, 404, _RespHeaders, _ClientRef} ->
            not_found;
        {ok, 200, _RespHeaders, ClientRef} ->
            {ok, Body} = hackney:body(ClientRef),
            Body
    end.

close(Bucket) ->
    Config = get_bucket(Bucket),
    ok = hackney_pool:stop_pool(Config#bucket.pool),
    ets:delete(?MODULE, Bucket),
    ok.

%--- Internal Functions -------------------------------------------------------

get_option(Key, Options) ->
    case proplists:lookup(Key, Options) of
        none         -> error({missing_option, Key});
        {Key, Value} -> Value
    end.

get_option(Key, Options, Default) ->
    proplists:get_value(Key, Options, Default).

get_bucket(Bucket) ->
    try
        ets:lookup_element(?MODULE, Bucket, 2)
    catch
        error:badarg -> error({bucket_not_found, Bucket})
    end.

pool_name(Bucket) ->
    binary_to_atom(iolist_to_binary([<<"elementary_">>, Bucket]), utf8).
