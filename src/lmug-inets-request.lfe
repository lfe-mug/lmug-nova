(defmodule lmug-inets-request
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun inets->map
  "Convert an inets/http request (mod record) to an lmug request.

  Note that the incoming request when using Erlang's inets/http server is the
  `mod` record defined in the inets httpd include."
  (((= (match-mod init_data init-data
                  ;;data ewsapi-resp
                  ;;config_db cfg
                  method method
                  absolute_uri abs-uri
                  ;;request_uri req-uri
                  http_version http-version
                  ;;request_line req-line
                  parsed_header headers
                  ;;connection connected?
                  ;;socket_type socket-type
                  ;;socket socket
                  entity_body body) mod-data))
   (let ((`#(init_data #(,remote-port ,remote-addr) ,_ ,_) init-data))
     `#m(method ,(lmug:method method)
         version ,http-version
         url ,abs-uri
         remote-addr ,remote-addr
         headers ,(lmug:headers headers)
         body ,(lmug:body body))))
  ((_)
   #(err "Couldn't match args aginst inets record")))
