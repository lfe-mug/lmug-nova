(defmodule lmug-inets-request
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun inets->map
  "Convert inets http request (mod record) to lmug request.

  The incoming request when using Erlang's inets/http server is the `mod`
  record defined in the inets httpd include."
  (((= (match-mod data ewsapi-data
                  method method
                  absolute_uri abs-uri
                  request_uri req-uri
                  http_version http-version
                  request_line req-line
                  parsed_header headers
                  entity_body body) req))
   (log-debug "ewsapi-data: ~p" (list ewsapi-data))
   (log-debug "req-uri ~p" (list req-uri))
   (log-debug "abs-uri ~p" (list abs-uri))
   (log-debug "headers: ~p" (list headers))
   (log-debug "req-line ~p" (list req-line))

   `#m(method ,(list_to_atom (string:to_lower method))
       version ,http-version
       url ,abs-uri
       ;;remote-addr (proplists:get_value 'remote_adress ewsapi-data)
       headers ,(http.header:list->map headers)
       body ,(list_to_binary body)))
       
       
    ;;(make-request method (list_to_atom (string:to_lower method))
    ;;              body (list_to_binary body)
    ;;              mw-data `(#(ewsapi-data ,ewsapi-data))))
  ((_)
    #(err "Couldn't match args aginst inets record")))
