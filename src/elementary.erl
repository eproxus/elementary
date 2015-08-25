-module(elementary).

-behaviour(application).

% Application Callbacks
-export([start/2]).
-export([stop/1]).

% API
-export([open/2]).
-export([get/2]).
-export([get/3]).
-export([put/3]).
-export([close/1]).

-record(bucket, {
    pool,
    endpoint,
    access_key,
    secret_access_key
}).

-type etag() :: {etag, iodata()}.
-type expires() :: {expires, {Name::iodata(), DateTime::calendar:datetime()}}.

-type option() :: etag().
-type property() :: etag() | expires().

%--- Application Callbacks ----------------------------------------------------

% @hidden
start(_StartType, _StartArgs) ->
    ets:new(?MODULE, [named_table, public]),
    {ok, self()}.

% @hidden
stop(_State) ->
    ets:delete(?MODULE).

%--- API ----------------------------------------------------------------------

% @doc Open a bucket.
%
% Valid options are:
% <ul>
%     <li>`access_key': Amazon AWS access key</li>
%     <li>`secret_access_key': Amazon AWS secret access key</li>
%     <li>
%         `endpoint': S3 endpoint to use (defaults to `<<"s3.amazonaws.com">>')
%     </li>
%     <li>
%         `connection_timeout': The connection timeout for requests in
%          milliseconds (defaults to `5000')
%     </li>
%     <li>
%         `max_connections': Max simultaneous connections to S3 (defaults
%         to `20')
%     </li>
% </ul>
-spec open(Bucket::iodata(), Options::[option()]) -> ok.
open(Bucket, Options) ->
    AccessKey = get_option(access_key, Options),
    SecretAccessKey = get_option(secret_access_key, Options),
    Endpoint = get_option(endpoint, Options, <<"s3.amazonaws.com">>),

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

% @doc Equivalent to `get(Bucket, Key, [])'.
% @see get/3
get(Bucket, Key) -> get(Bucket, Key, []).

% @doc Get an object from a bucket.
%
% Returns the data for a key in an open bucket (must have been opened with
% {@link open/2}). If available, the properties will contain the ETag value
% and the expiration information associated with the key.
%
% If an ETag is supplied (with the option `{etag, ETag}'), it is possible that
% the key has not been modified since the last time. In this case,
% `not_modified' is then returned instead of the data.
-spec get(Bucket::iodata(), Key::iodata(), Options::[option()]) ->
    {Data::iodata() | not_mofified | not_found, Properties::[property()]}.
get(Bucket, Key, Options) ->
    case request(Bucket, Key, get, <<>>, Options) of
        {200, Headers, BodyRef}  ->
            {get_body(BodyRef), headers(
                [{etag, <<"ETag">>}, {expires, <<"x-amz-expiration">>}],
                Headers
            )};
        {304, Headers, _BodyRef} ->
            {not_modified, headers(
                [{etag, <<"ETag">>}, {expires, <<"x-amz-expiration">>}],
                Headers
            )};
        {404, _Headers, _BodyRef} ->
            {not_found, []};
        Response ->
            error({unknown_response, Response})
    end.

put(Bucket, Key, Data) ->
    case request(Bucket, Key, put, Data, []) of
        {200, Headers, _BodyRef} ->
            headers(
                [{etag, <<"ETag">>}, {expires, <<"x-amz-expiration">>}],
                Headers
            );
        Response ->
            error({unknown_response, Response})
    end.

close(Bucket) ->
    Config = get_bucket(Bucket),
    ok = hackney_pool:stop_pool(Config#bucket.pool),
    ets:delete(?MODULE, Bucket),
    ok.

%--- Internal Functions -------------------------------------------------------

request(Bucket, Key, Method, Payload, Options) ->
    Config = get_bucket(Bucket),

    Host = [Bucket, <<".">>, Config#bucket.endpoint],
    Path = [<<"/">>, Key],
    URL = [Host, Path],
    HackneyOptions = [{pool, Config#bucket.pool}],
    Headers = [
        {<<"Host">>, Host},
        {<<"Content-Length">>, integer_to_list(byte_size(Payload))}
    ] ++ to_headers(Options),
    Auth =  elementary_signature:headers(
        Method,
        Path,
        Headers,
        Payload,
        Config#bucket.access_key,
        Config#bucket.secret_access_key,
        <<"eu-west-1">>,
        <<"s3">>
    ),
    {ok, StatusCode, RespHeaders, ClientRef} =
        hackney:request(Method, URL, Headers ++ Auth, Payload, HackneyOptions),
    {StatusCode, RespHeaders, ClientRef}.

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

get_body(BodyRef) ->
    {ok, Body} = hackney:body(BodyRef),
    Body.

headers([], _Headers) ->
    [];
headers([{Key, Header}|Keys], Headers) ->
    case lists:keyfind(Header, 1, Headers) of
        {Header, Value} ->
            [{Key, header(Key, Value)}|headers(Keys, Headers)];
        false ->
            headers(Keys, Headers)
    end.

header(etag, Value) ->
    Value;
header(expires, Value) ->
    {match, [Day, Month, Year, Hour, Minute, Second, Name]} = re:run(
        Value,
        <<"
            .*?,\\s
            (\\d+)\\s                      # Day
            (\\w{3})\\s                   # Month
            (\\d+)\\s                     # Year
            (\\d{2}):(\\d{2}):(\\d{2})\\s # Time
            GMT \\\",\\s
            rule-id=\\\"(.*?)\\\"         # Rule name
        ">>,
        [{capture, all_but_first, binary}, extended]
    ),
    {Name, {
        {
            binary_to_integer(Year),
            binary_to_month(Month),
            binary_to_integer(Day)},
        {
            binary_to_integer(Hour),
            binary_to_integer(Minute),
            binary_to_integer(Second)
        }}
    }.

to_headers([]) ->
    [];
to_headers([{etag, Value}|Options]) ->
    [{<<"If-None-Match">>, Value}|to_headers(Options)].

binary_to_month(<<"Jan">>) -> 1;
binary_to_month(<<"Feb">>) -> 2;
binary_to_month(<<"Mar">>) -> 3;
binary_to_month(<<"Apr">>) -> 4;
binary_to_month(<<"May">>) -> 5;
binary_to_month(<<"Jun">>) -> 6;
binary_to_month(<<"Jul">>) -> 7;
binary_to_month(<<"Aug">>) -> 8;
binary_to_month(<<"Sep">>) -> 9;
binary_to_month(<<"Oct">>) -> 10;
binary_to_month(<<"Nov">>) -> 11;
binary_to_month(<<"Dec">>) -> 12.
