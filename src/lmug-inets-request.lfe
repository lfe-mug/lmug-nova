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
   (let* ((`#(init_data #(,remote-port ,remote-addr) ,_ ,_) init-data)
          (req (http.request:new
                (lmug:method method)
                (parse-abs-url abs-uri)
                (lmug:body body)
                (lmug:headers headers))))
     (clj:-> req
             (mupd 'version (parse-http-version http-version))
             (mupd 'remote-addr remote-addr))))
  ((inets-req)
   (let ((msg "Couldn't match args aginst inets record"))
     (log-error "~s" (list msg))
     #(err ,msg))))

(defun parse-http-version (version-string)
  ;; TODO implement this
  version-string)

(defun parse-abs-url (abs-uri)
  ;; TODO implement this
  (io_lib:format "http://~s" `(,abs-uri)))
