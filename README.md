# lmug-nova

[![Build Status][gh-actions-badge]][gh-actions]
[![LFE Versions][lfe-badge]][lfe]
[![Erlang Versions][erlang-badge]][versions]
[![Tags][github-tags-badge]][github-tags]

*An lmug adapter for the Nova web server*

[![][logo]][logo-large]

##### Contents

* [Introduction](#introduction-)
* [Installation](#installation-)
* [Documentation](#documentation-)
* [Usage](#usage-)
* [License](#license-)


## Introduction [&#x219F;](#contents)

Like Clojure's Ring before it, LFE's lmug provides the LFE programmer a means
for creating middleware between an HTTP server request and the response that
is returned to the client.

In particular, lmug-nova implements an lmug adaptor for use with the
Nova framework's HTTP server, allowing lmug middleware to run on it by adapting
lmug requests, responses, and handlers to the Nova API.

## Installation [&#x219F;](#contents)

```erlang
{deps, [
   {lmug-nova, "0.1.0", {pkg, lmug_nova}}
  ]}.
```

## Documentation [&#x219F;](#contents)

* [lmug Core Concepts](https://github.com/lfe-mug/lmug/blob/main/docs/core-concepts.md)
* [lmug Specification](https://github.com/lfe-mug/lmug/blob/main/docs/lmug-spec.md)
* [Usage Details for lmug adapters](https://github.com/lfe-mug/lmug/blob/main/docs/usage-details.md)

## Usage [&#x219F;](#contents)

Define an app with a middleware chain. First, let's make a little static resource content:

``` shell
mkdir static
echo "<html><body>lmug-nova dev server</body></html>" > static/index.html
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

(lmug-nova:start app #m(port 5099))
(lmug-state:get-metadata)

(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap #m(doc-root "../"))))

(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap #m(doc-root "priv"))))

(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap #m(doc-root "../lmug/priv"))))

(set app (clj:-> (lmug:app)
                 (lmug-mw-resource:wrap
                     #m(doc-root "../lmug/priv"
                        watcher? true))))

(lmug-state:get-metadata)

(application:ensure_all_started 'fs)
(fs:subscribe)
(fs_demo:start_looper)
;; do stuff on filesystem
(flush)

(fs:start_link '
```

```lisp
lfe> (lmug-nova:start app #m(port 5099))
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
< Server: XXX/TBD
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
< Server: XXX/TBD
< Content-Type: text/html
< Etag: tCTQL448
< Content-Length: 48
< Last-Modified: Mon, 19 Feb 2024 22:11:55 GMT
<
<html><body>lmug-nova dev server</body></html>
* Connection #0 to host localhost left intact
```

## License [&#x219F;](#contents)

```
Copyright © 2016-2024, LFE Dragon Team

Distributed under the Apache License, Version 2.0.
```

[//]: ---Named-Links---

[logo]: priv/images/lmug.png
[logo-large]: priv/images/lmug-large.png
[gh-actions-badge]: https://github.com/lfe-mug/lmug-nova/workflows/ci%2Fcd/badge.svg
[gh-actions]: https://github.com/lfe-mug/lmug-nova/actions
[lfe]: https://github.com/lfe/lfe
[lfe-badge]: https://img.shields.io/badge/lfe-2.1-blue.svg
[erlang-badge]: https://img.shields.io/badge/erlang-21%20to%2026-blue.svg
[versions]: https://github.com/lfe-mug/lmug-nova/blob/master/.github/workflows/cicd.yml
[github-tags]: https://github.com/lfe-mug/lmug-nova/tags
[github-tags-badge]: https://img.shields.io/github/tag/lfe-mug/lmug-nova.svg
