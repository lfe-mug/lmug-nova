(defmodule lmug-nova-request
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun nova->map
  "Convert a Nova request to an lmug request."
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
  ((nova-req)
   (let ((msg "Couldn't match args aginst the Nova record"))
     (log-error "~s" (list msg))
     #(err ,msg))))

(defun parse-http-version (version-string)
  ;; TODO implement this
  version-string)

(defun parse-abs-url (abs-uri)
  ;; TODO implement this
  (io_lib:format "http://~s" `(,abs-uri)))
