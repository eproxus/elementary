-module(elementary_signature_test).

-include_lib("eunit/include/eunit.hrl").

% AWS Documentation Examples

-define(EXAMPLE_CANONICAL_REQUEST,
<<"GET
/test.txt

host:examplebucket.s3.amazonaws.com
range:bytes=0-9
x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
x-amz-date:20130524T000000Z

host;range;x-amz-content-sha256;x-amz-date
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).

-define(EXAMPLE_STRING_TO_SIGN,
<<"AWS4-HMAC-SHA256
20130524T000000Z
20130524/us-east-1/s3/aws4_request
7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972">>).

-define(EXAMPLE_SIGNATURE, <<"f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41">>).
-define(EXAMPLE_ACCESS_KEY, <<"AKIAIOSFODNN7EXAMPLE">>).
-define(EXAMPLE_SECRET_ACCESS_KEY, <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>).
-define(EXAMPLE_REGION, "us-east-1").
-define(EXAMPLE_SERVICE, "s3").

-define(EXAMPLE_AUTHORIZATION_HEADER, <<"AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41">>).

%--- Tests --------------------------------------------------------------------

headers_test() ->
    Headers = elementary_signature:headers(
        get,
        "/test.txt",
        [
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"Range", "bytes=0-9"}
        ],
        <<>>,
        ?EXAMPLE_ACCESS_KEY,
        ?EXAMPLE_SECRET_ACCESS_KEY,
        ?EXAMPLE_REGION,
        ?EXAMPLE_SERVICE,
        {{2013, 5, 24}, {0, 0, 0}}
    ),
    ?assertEqual(
        [
            {<<"Authorization">>, ?EXAMPLE_AUTHORIZATION_HEADER},
            {<<"x-amz-content-sha256">>, <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>},
            {<<"x-amz-date">>, <<"20130524T000000Z">>}
        ],
        [{H, iolist_to_binary(V)} || {H, V} <- Headers]
    ).

canonical_request_test() ->
    {CanonicalRequest, _SignedHeaders} = canonical_request(),
    ?assertEqual(
        ?EXAMPLE_CANONICAL_REQUEST,
        iolist_to_binary(CanonicalRequest)
    ).

string_to_sign_test() ->
    {CanonicalRequest, _SignedHeaders} = canonical_request(),
    ?assertEqual(
        ?EXAMPLE_STRING_TO_SIGN,
        iolist_to_binary(string_to_sign(CanonicalRequest))
    ).

signature_test() ->
    {CanonicalRequest, _SignedHeaders} = canonical_request(),
    ?assertEqual(
        ?EXAMPLE_SIGNATURE,
        iolist_to_binary(signature(string_to_sign(CanonicalRequest)))
    ).

%--- Internal Functions -------------------------------------------------------

canonical_request() ->
    elementary_signature:canonical_request(
        get,
        "/test.txt",
        [
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"Range", "bytes=0-9"},
            {"x-amz-content-sha256", "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"},
            {"x-amz-date", "20130524T000000Z"}
        ],
        "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"
    ).

string_to_sign(CanonicalRequest) ->
    elementary_signature:string_to_sign(
        "20130524T000000Z",
        "20130524/us-east-1/s3/aws4_request",
        CanonicalRequest
    ).

signature(StringToSign) ->
    elementary_signature:signature(
        ?EXAMPLE_SECRET_ACCESS_KEY,
        "20130524",
        "us-east-1",
        "s3",
        StringToSign
    ).
