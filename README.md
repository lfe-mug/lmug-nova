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
Server API (EWSAPI).


### EWSAPI [&#x219F;](#contents)

While lmug middleware accept handlers as arguments with these handlers each
being functions in their own right (and which accept a request record as an
argument), EWSAPI modules (the inets http version of "middleware") are a little
more diverse. Unlike lmug middleware, they don't just return additional
handlers, rather they may return any one of the following (see
[inets httpd Reference Manual](http://erlang.org/doc/man/httpd.html)):

* ``#(proceed old-data)``
* ``#(proceed new-data)``
* ``#(break new-data)``
* ``done``

Although this allows for a great deal of flexibility when writting EWSAPI
middleware, it also means that not all middleware may be treated equally --
some will expect data in one format and not in others, some will be used
to prepare data for different middleware further down the chain.

To keep things simple during the initial implementation of the inet http
lmug adaptor, the only ``new-data`` that is supported has the following
form:

```lisp
`(#(response #(response
               (#(code ,status-code)
                #(,header-name ,header-value)
                ...)
               ,body)))
```

EWSAPI modules define a ``do/1`` function that gets called when the web
server processes each request. A list of modules is provided to the
server in the configuration, and all requests are sent through this chain
of modules.

The lmug-inets adaptor provides the ``lmug-inets:do/1`` function that may
be inserted into the chain of EWSAPI modules when configuring a EWSAPI
web server. ``lmug-inets:do/1`` utilizes the lmug-inets adaptor and
appropriately converts requests, responses, and handlers.


## Installation [&#x219F;](#contents)

```erlang
{deps, [
   {lmuginets, "0.1.0"}}}
  ]}.
```

## Documentation [&#x219F;](#contents)

TBD


## Usage [&#x219F;](#contents)

Define an app with a middleware chain:

```lisp
> (set app (-> (lmug:response)
               (lmug-mw-identity:wrap)
               (lmug-mw-content-type:wrap)
               (lmug-mw-identity:wrap)))
#Fun<lmug-mw-content-type.0.7090676>
```

```lisp
> (set opts '(#(server_name "lmuginets")
              #(document_root "/tmp")
              #(server_root "/tmp")
              #(port 5099)))
```

```lisp
> (lmug-inets:start app opts)
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
