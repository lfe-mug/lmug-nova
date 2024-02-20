# lmug-inets

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*An lmug adapter for the OTP inets web server*

[![][logo]][logo-large]

##### Contents

* [Introduction](#introduction-)
  * [EWSAPI](#ewsapi-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

Like Clojure's Ring before it, LFE's lmug provides the LFE programmer a means
for creating middleware between an HTTP server request and the response that
is returned to the client.

In particular, lmug-inets implements an lmug adaptor for use with the
Erlang/OTP inets http server, allowing lmug middleware to run on the OTP inets
web server by adapting lmug requests, responses, and handlers to the Erlang Web
Server API (EWSAPI). For more backgound and some detailed context, be sure to 
checkout [our EWSAPI & lmug notes](./docs/ewsapi.md).

## Installation [&#x219F;](#contents)

```erlang
{deps, [
   {lmug-inets, "0.1.1", {pkg, lmug_inets}}
  ]}.
```

## Documentation [&#x219F;](#contents)

* [lmug Core Concepts](https://github.com/lfe-mug/lmug/blob/main/docs/core-concepts.md)
* [lmug Specification](https://github.com/lfe-mug/lmug/blob/main/docs/lmug-spec.md)
* [Usage Details for lmug adapters](https://github.com/lfe-mug/lmug/blob/main/docs/usage-details.md)
* [inets EWSAPI and lmug](./docs/ewsapi.md)

## Usage [&#x219F;](#contents)

Define an app with a middleware chain. First, let's make a little static resource content:

``` shell
mkdir static
echo "<html><body>lmug-inets dev server</body></html>" > static/index.html
```

```lisp
lfe> (set app (clj:-> (lmug:app)
                      (lmug-mw-request-id:wrap)
                      (lmug-mw-content-type:wrap)
                      (lmug-mw-resource:wrap #m(doc-root "static"))
                      (lmug-mw-status-body:wrap)
                      (lmug-mw-log-request:wrap #m(log-level notice))))
```

```lisp
(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap)))

(lmug-inets:start app #m(port 5099))
(lmug-state:get-metadata)

(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap #m(doc-root "../"))))
```

```lisp
lfe> (lmug-inets:start app #m(port 5099))
```

This can be tested from another terminal with `curl`:

``` shell
curl -v "http://alice:sekr1t@localhost:5099/response.txt"
```

Which will give something like the following:

``` shell
*   Trying 127.0.0.1:5099...
* Connected to localhost (127.0.0.1) port 5099 (#0)
* Server auth using Basic with user 'alice'
> GET /response.txt HTTP/1.1
> Host: localhost:5099
> Authorization: Basic YWxpY2U6c2VrcjF0
> User-Agent: curl/8.1.2
> Accept: */*
>
< HTTP/1.1 200 OK
< Date: Tue, 20 Feb 2024 01:41:26 GMT
< Server: inets/9.1 (unix/darwin) OTP/26
< X-Request-ID: 11548829628205025075258581696865370112
< Content-Type: text/plain
< Content-Length: 3
<
* Connection #0 to host localhost left intact
200
```

Then, to test the static resource middleware:

``` shell
curl -v "http://localhost:5099/index.html"
```

or:

``` shell
curl -v "http://localhost:5099/"
```

Will give something like the following:

``` shell
*   Trying 127.0.0.1:5099...
* Connected to localhost (127.0.0.1) port 5099 (#0)
> GET /index.html HTTP/1.1
> Host: localhost:5099
> User-Agent: curl/8.1.2
> Accept: */*
>
< HTTP/1.1 200 OK
< Date: Tue, 20 Feb 2024 01:42:54 GMT
< Server: inets/9.1 (unix/darwin) OTP/26
< Content-Type: text/html
< Etag: tCTQL448
< Content-Length: 48
< Last-Modified: Mon, 19 Feb 2024 22:11:55 GMT
<
<html><body>lmug-inets dev server</body></html>
* Connection #0 to host localhost left intact
```

## License [&#x219F;](#contents)

```
Copyright © 2016-2024, LFE Dragon Team

Distributed under the Apache License, Version 2.0.
```

[//]: ---Named-Links---

[logo]: priv/images/lmug-inets.png
[logo-large]: priv/images/lmug-inets-large.png
[gh-actions-badge]: https://github.com/lfe-mug/lmug-inets/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfe-mug/lmug-inets/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfe-mug/lmug-inets/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/lfe-mug/lmug-inets/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfe-mug/lmug-inets.svg
