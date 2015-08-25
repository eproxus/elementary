

# Module elementary #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`application`](application.md).

<a name="types"></a>

## Data Types ##




### <a name="type-etag">etag()</a> ###



<pre><code>
etag() = {etag, iodata()}
</code></pre>





### <a name="type-expires">expires()</a> ###



<pre><code>
expires() = {expires, {Name::iodata(), DateTime::<a href="calendar.md#type-datetime">calendar:datetime()</a>}}
</code></pre>





### <a name="type-option">option()</a> ###



<pre><code>
option() = <a href="#type-etag">etag()</a>
</code></pre>





### <a name="type-property">property()</a> ###



<pre><code>
property() = <a href="#type-etag">etag()</a> | <a href="#type-expires">expires()</a>
</code></pre>


<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#close-1">close/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-2">get/2</a></td><td>Equivalent to <code>get(Bucket, Key, [])</code>.</td></tr><tr><td valign="top"><a href="#get-3">get/3</a></td><td>Get an object from a bucket.</td></tr><tr><td valign="top"><a href="#open-2">open/2</a></td><td>Open a bucket.</td></tr><tr><td valign="top"><a href="#put-3">put/3</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="close-1"></a>

### close/1 ###

`close(Bucket) -> any()`


<a name="get-2"></a>

### get/2 ###

`get(Bucket, Key) -> any()`

Equivalent to `get(Bucket, Key, [])`.

__See also:__ [get/3](#get-3).
<a name="get-3"></a>

### get/3 ###


<pre><code>
get(Bucket::iodata(), Key::iodata(), Options::[<a href="#type-option">option()</a>]) -&gt; {Data::iodata() | not_mofified | not_found, Properties::[<a href="#type-property">property()</a>]}
</code></pre>

<br></br>



Get an object from a bucket.



Returns the data for a key in an open bucket (must have been opened with
[`open/2`](#open-2)). If available, the properties will contain the ETag value 
and the expiration information associated with the key.


If an ETag is supplied (with the option `{etag, ETag}`), it is possible that
the key has not been modified since the last time. In this case,
`not_modified` is then returned instead of the data.
<a name="open-2"></a>

### open/2 ###


<pre><code>
open(Bucket::iodata(), Options::[<a href="#type-option">option()</a>]) -&gt; ok
</code></pre>

<br></br>



Open a bucket.


Valid options are:

* `access_key`: Amazon AWS access key

* `secret_access_key`: Amazon AWS secret access key

* `endpoint`: S3 endpoint to use (defaults to `<<"s3.amazonaws.com">>`)


<a name="put-3"></a>

### put/3 ###

`put(Bucket, Key, Data) -> any()`


