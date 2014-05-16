-module(elementary_signature).

% API
-export([headers/8]).

-ifdef(TEST).
-export([headers/9]).
-export([canonical_request/4]).
-export([string_to_sign/3]).
-export([signature/5]).
-endif.

%--- API ----------------------------------------------------------------------

headers(Method, CanonicalURI, Headers, Payload, AccessKey, SecretAccessKey,
        Region, Service) ->
    Now = calendar:now_to_universal_time(now()),
    headers(Method, CanonicalURI, Headers, Payload, AccessKey, SecretAccessKey,
        Region, Service, Now).

%--- Internal Functions -------------------------------------------------------

headers(Method, CanonicalURI, Headers, Payload, AccessKey, SecretAccessKey,
        Region, Service, {D, T}) ->
    Date = date(D),
    TimeStamp = timestamp(Date, T),
    Scope = scope(Date, Region, Service),
    HashedPayload = hash(Payload),
    AMZHeaders = [
        {<<"x-amz-content-sha256">>, HashedPayload},
        {<<"x-amz-date">>, TimeStamp}
    ],
    AllHeaders = AMZHeaders ++ Headers,
    {CanonicalRequest, SignedHeaders} =
        canonical_request(Method, CanonicalURI, AllHeaders, HashedPayload),
    StringToSign = string_to_sign(TimeStamp, Scope, CanonicalRequest),
    Signature =
        signature(SecretAccessKey, Date, Region, Service, StringToSign),
    [
        {
            <<"Authorization">>, [
                <<"AWS4-HMAC-SHA256 Credential=">>,
                AccessKey, $/, Scope, $,,
                <<"SignedHeaders=">>, SignedHeaders, $,,
                <<"Signature=">>, Signature
            ]
        }
        | AMZHeaders
    ].

canonical_request(Method, CanonicalURI, Headers, HashedPayload) ->
    {CanonicalHeaders, SignedHeaders} = canonical_headers(Headers),
    {
        [
            method(Method), $\n,
            CanonicalURI, $\n,
            $\n,
            CanonicalHeaders, $\n,
            SignedHeaders, $\n,
            HashedPayload
        ],
        SignedHeaders
    }.

string_to_sign(TimeStamp, Scope, CanonicalRequest) ->
    [
        <<"AWS4-HMAC-SHA256">>, $\n,
        TimeStamp, $\n,
        Scope, $\n,
        hash(CanonicalRequest)
    ].

signature(SecretAccessKey, Date, Region, Service, StringToSign) ->
    DateKey = hmac:hmac256([<<"AWS4">>, SecretAccessKey], Date),
    DateRegionKey = hmac:hmac256(DateKey, Region),
    DateRegionServiceKey = hmac:hmac256(DateRegionKey, Service),
    SigningKey = hmac:hmac256(DateRegionServiceKey, "aws4_request"),
    string:to_lower(hmac:hexlify(hmac:hmac256(SigningKey, StringToSign))).

%--- Parts --------------------------------------------------------------------

method(get) -> <<"GET">>.

canonical_headers(Headers) ->
    canonical_headers(lists:reverse(lists:sort(Headers)), [], []).

canonical_headers([], CanonicalHeaders, SignedHeaders) ->
    {CanonicalHeaders, iolist_join(SignedHeaders, $;)};
canonical_headers([Header|Headers], CanonicalHeaders, SignedHeaders) ->
    {C, S} = canonical_header(Header),
    canonical_headers(Headers, [C|CanonicalHeaders], [S|SignedHeaders]).

canonical_header({H, V}) ->
    Lower = lowercase(H),
    {[Lower, $:, trim(V), $\n], Lower}.

scope(Date, Region, Service) ->
    [Date, $/, Region, $/, Service, $/, <<"aws4_request">>].

%--- Utility Functions --------------------------------------------------------

timestamp(Date, {Hour, Minute, Second}) ->
    io_lib:format("~sT~2..0w~2..0w~2..0wZ", [Date, Hour, Minute, Second]).

date({Year, Month, Day}) ->
    io_lib:format("~4..0w~2..0w~2..0w", [Year, Month, Day]).

lowercase(Binary) when is_binary(Binary) -> lowercase(binary_to_list(Binary));
lowercase(String) when is_list(String)   -> string:to_lower(String).

trim(IOData) ->
    re:replace(IOData, <<"^\\s+|\\s+$">>, [], [dotall, multiline]).

hash(<<>>) ->
    <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>;
hash(Payload) ->
    string:to_lower(hmac:hexlify(erlsha2:sha256(Payload))).

iolist_join([], _Separator)          -> [];
iolist_join([First|List], Separator) -> [First, [[Separator, I] || I <- List]].
