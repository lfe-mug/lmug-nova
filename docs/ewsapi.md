# EWSAPI

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
