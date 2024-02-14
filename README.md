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
   {lmuginets, "0.1.0"}}}
  ]}.
```

## Documentation [&#x219F;](#contents)

* [lmug Core Concepts](https://github.com/lfe-mug/lmug/blob/main/docs/core-concepts.md)
* [lmug Specification](https://github.com/lfe-mug/lmug/blob/main/docs/lmug-spec.md)
* [Usage Details for lmug adapters](https://github.com/lfe-mug/lmug/blob/main/docs/usage-details.md)
* [inets EWSAPI and lmug](./docs/ewsapi.md)

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
