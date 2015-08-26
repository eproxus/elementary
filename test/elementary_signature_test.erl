-module(elementary_signature_test).

-include_lib("eunit/include/eunit.hrl").

% AWS Documentation Examples
% http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html


% GET
-define(EXAMPLE_GET_SHA, <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).
-define(EXAMPLE_GET_CANONICAL_REQUEST,
<<"GET
/test.txt

host:examplebucket.s3.amazonaws.com
range:bytes=0-9
x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
x-amz-date:20130524T000000Z

host;range;x-amz-content-sha256;x-amz-date
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).

-define(EXAMPLE_GET_STRING_TO_SIGN,
<<"AWS4-HMAC-SHA256
20130524T000000Z
20130524/us-east-1/s3/aws4_request
7344ae5b7ee6c3e7e6b0fe0640412a37625d1fbfff95c48bbb2dc43964946972">>).

-define(EXAMPLE_GET_SIGNATURE, <<"f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41">>).
-define(EXAMPLE_GET_AUTHORIZATION_HEADER, <<"AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;range;x-amz-content-sha256;x-amz-date,Signature=f0e8bdb87c964420e857bd35b5d6ed310bd44f0170aba48dd91039c6036bdb41">>).

% GET (List Objects)
% http://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-header-based-auth.html#example-signature-list-bucket
-define(EXAMPLE_LIST_SHA, <<"e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).
-define(EXAMPLE_LIST_CANONICAL_REQUEST,
<<"GET
/
max-keys=2&prefix=J
host:examplebucket.s3.amazonaws.com
x-amz-content-sha256:e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855
x-amz-date:20130524T000000Z

host;x-amz-content-sha256;x-amz-date
e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855">>).
-define(EXAMPLE_LIST_STRING_TO_SIGN,
<<"AWS4-HMAC-SHA256
20130524T000000Z
20130524/us-east-1/s3/aws4_request
df57d21db20da04d7fa30298dd4488ba3a2b47ca3a489c74750e0f1e7df1b9b7">>).
-define(EXAMPLE_LIST_SIGNATURE, <<"34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7">>).
-define(EXAMPLE_LIST_AUTHORIZATION_HEADER, <<"AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=host;x-amz-content-sha256;x-amz-date,Signature=34b48302e7b5fa45bde8084f4b7868a86f0a534bc59db6670ed5711ef69dc6f7">>).

% PUT
-define(EXAMPLE_PUT_SHA, <<"44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072">>).
-define(EXAMPLE_PUT_CANONICAL_REQUEST,
<<"PUT
/test%24file.text

date:Fri, 24 May 2013 00:00:00 GMT
host:examplebucket.s3.amazonaws.com
x-amz-content-sha256:44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072
x-amz-date:20130524T000000Z
x-amz-storage-class:REDUCED_REDUNDANCY

date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class
44ce7dd67c959e0d3524ffac1771dfbba87d2b6b4b4e99e42034a8b803f8b072">>).

-define(EXAMPLE_PUT_STRING_TO_SIGN,
<<"AWS4-HMAC-SHA256
20130524T000000Z
20130524/us-east-1/s3/aws4_request
9e0e90d9c76de8fa5b200d8c849cd5b8dc7a3be3951ddb7f6a76b4158342019d">>).

-define(EXAMPLE_PUT_SIGNATURE, <<"98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd">>).
-define(EXAMPLE_PUT_AUTHORIZATION_HEADER, <<"AWS4-HMAC-SHA256 Credential=AKIAIOSFODNN7EXAMPLE/20130524/us-east-1/s3/aws4_request,SignedHeaders=date;host;x-amz-content-sha256;x-amz-date;x-amz-storage-class,Signature=98ad721746da40c64f1a55b78f14c238d841ea1380cd77a1b5971af0ece108bd">>).

% Shared
-define(EXAMPLE_ACCESS_KEY, <<"AKIAIOSFODNN7EXAMPLE">>).
-define(EXAMPLE_SECRET_ACCESS_KEY, <<"wJalrXUtnFEMI/K7MDENG/bPxRfiCYEXAMPLEKEY">>).
-define(EXAMPLE_REGION, "us-east-1").
-define(EXAMPLE_SERVICE, "s3").

%--- Tests --------------------------------------------------------------------

get_headers_test() ->
    {Headers, _QS} = elementary_signature:headers(
        get,
        "test.txt",
        [],
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
            {<<"Authorization">>, ?EXAMPLE_GET_AUTHORIZATION_HEADER},
            {<<"x-amz-content-sha256">>, ?EXAMPLE_GET_SHA},
            {<<"x-amz-date">>, <<"20130524T000000Z">>}
        ],
        [{H, iolist_to_binary(V)} || {H, V} <- Headers]
    ).

get_canonical_request_test() ->
    SHA = ?EXAMPLE_GET_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = get_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_GET_CANONICAL_REQUEST,
        iolist_to_binary(CanonicalRequest)
    ).

get_string_to_sign_test() ->
    SHA = ?EXAMPLE_GET_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = get_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_GET_STRING_TO_SIGN,
        iolist_to_binary(string_to_sign(CanonicalRequest))
    ).

get_signature_test() ->
    SHA = ?EXAMPLE_GET_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = get_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_GET_SIGNATURE,
        iolist_to_binary(signature(string_to_sign(CanonicalRequest)))
    ).

list_headers_test() ->
    {Headers, _QS} = elementary_signature:headers(
        get,
        "",
        [
            {<<"prefix">>, 'J'},
            {<<"max-keys">>, 2}
        ],
        [
            {"Host", "examplebucket.s3.amazonaws.com"}
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
            {<<"Authorization">>, ?EXAMPLE_LIST_AUTHORIZATION_HEADER},
            {<<"x-amz-content-sha256">>, ?EXAMPLE_LIST_SHA},
            {<<"x-amz-date">>, <<"20130524T000000Z">>}
        ],
        [{H, iolist_to_binary(V)} || {H, V} <- Headers]
    ).

list_canonical_request_test() ->
    SHA = ?EXAMPLE_LIST_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = list_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_LIST_CANONICAL_REQUEST,
        iolist_to_binary(CanonicalRequest)
    ).

list_string_to_sign_test() ->
    SHA = ?EXAMPLE_LIST_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = list_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_LIST_STRING_TO_SIGN,
        iolist_to_binary(string_to_sign(CanonicalRequest))
    ).

list_signature_test() ->
    SHA = ?EXAMPLE_LIST_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = list_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_LIST_SIGNATURE,
        iolist_to_binary(signature(string_to_sign(CanonicalRequest)))
    ).

put_headers_test() ->
    {Headers, _QS} = elementary_signature:headers(
        put,
        "test%24file.text",
        [],
        [
            {"Date", "Fri, 24 May 2013 00:00:00 GMT"},
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"x-amz-storage-class", "REDUCED_REDUNDANCY"}
        ],
        <<"Welcome to Amazon S3.">>,
        ?EXAMPLE_ACCESS_KEY,
        ?EXAMPLE_SECRET_ACCESS_KEY,
        ?EXAMPLE_REGION,
        ?EXAMPLE_SERVICE,
        {{2013, 5, 24}, {0, 0, 0}}
    ),
    ?assertEqual(
        [
            {<<"Authorization">>, ?EXAMPLE_PUT_AUTHORIZATION_HEADER},
            {<<"x-amz-content-sha256">>, ?EXAMPLE_PUT_SHA},
            {<<"x-amz-date">>, <<"20130524T000000Z">>}
        ],
        [{H, iolist_to_binary(V)} || {H, V} <- Headers]
    ).

put_canonical_request_test() ->
    SHA = ?EXAMPLE_PUT_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = put_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_PUT_CANONICAL_REQUEST,
        iolist_to_binary(CanonicalRequest)
    ).

put_string_to_sign_test() ->
    SHA = ?EXAMPLE_PUT_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = put_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_PUT_STRING_TO_SIGN,
        iolist_to_binary(string_to_sign(CanonicalRequest))
    ).

put_signature_test() ->
    SHA = ?EXAMPLE_PUT_SHA,
    {CanonicalRequest, _SignedHeaders, _QS} = put_canonical_request(SHA),
    ?assertEqual(
        ?EXAMPLE_PUT_SIGNATURE,
        iolist_to_binary(signature(string_to_sign(CanonicalRequest)))
    ).

%--- Internal Functions -------------------------------------------------------

put_canonical_request(SHA) ->
    elementary_signature:canonical_request(
        put,
        "test%24file.text",
        [],
        [
            {"Date", "Fri, 24 May 2013 00:00:00 GMT"},
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"x-amz-storage-class", "REDUCED_REDUNDANCY"},
            {"x-amz-content-sha256", SHA},
            {"x-amz-date", "20130524T000000Z"}
        ],
        SHA
    ).

get_canonical_request(SHA) ->
    elementary_signature:canonical_request(
        get,
        "test.txt",
        [],
        [
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"Range", "bytes=0-9"},
            {"x-amz-content-sha256", SHA},
            {"x-amz-date", "20130524T000000Z"}
        ],
        SHA
    ).

list_canonical_request(SHA) ->
    elementary_signature:canonical_request(
        get,
        "",
        [{<<"max-keys">>, 2}, {<<"prefix">>, 'J'}],
        [
            {"Host", "examplebucket.s3.amazonaws.com"},
            {"x-amz-content-sha256", SHA},
            {"x-amz-date", "20130524T000000Z"}
        ],
        SHA
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
