

# Module elementary_signature #
* [Function Index](#index)
* [Function Details](#functions)

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#canonical_request-4">canonical_request/4</a></td><td></td></tr><tr><td valign="top"><a href="#headers-8">headers/8</a></td><td></td></tr><tr><td valign="top"><a href="#headers-9">headers/9</a></td><td></td></tr><tr><td valign="top"><a href="#signature-5">signature/5</a></td><td></td></tr><tr><td valign="top"><a href="#string_to_sign-3">string_to_sign/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="canonical_request-4"></a>

### canonical_request/4 ###

`canonical_request(Method, CanonicalURI, Headers, HashedPayload) -> any()`

<a name="headers-8"></a>

### headers/8 ###

`headers(Method, CanonicalURI, Headers, Payload, AccessKey, SecretAccessKey, Region, Service) -> any()`

<a name="headers-9"></a>

### headers/9 ###

`headers(Method, CanonicalURI, Headers, Payload, AccessKey, SecretAccessKey, Region, Service, X9) -> any()`

<a name="signature-5"></a>

### signature/5 ###

`signature(SecretAccessKey, Date, Region, Service, StringToSign) -> any()`

<a name="string_to_sign-3"></a>

### string_to_sign/3 ###

`string_to_sign(TimeStamp, Scope, CanonicalRequest) -> any()`

