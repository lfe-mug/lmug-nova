(defmodule lmug-inets-adptr
  (behaviour lmug-adptr)
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "clj/include/compose.lfe")
(include-lib "lmug/include/request.lfe")
(include-lib "lmug/include/response.lfe")

(defun convert-request
  "Convert inets http request to lmug request."
  (((= (match-mod data ewsapi-data
                  method method
                  absolute_uri abs-uri
                  request_uri req-uri
                  http_version http-version
                  request_line req-line
                  parsed_header headers
                  entity_body body) req))
    (logjam:debug "Converting inets http request ...")
    ;; XXX add the rest of the request data to the new record here
    (make-request method (list_to_atom (string:to_lower method))
                  body (list_to_binary body)
                  mw-data `(#(ewsapi-data ,ewsapi-data))))
  ((data)
    (logjam:error "Couldn't match args aginst inets record")
    data))

(defun convert-response (resp)
  "Convert lmug response to inets http response."
  (logjam:debug "Converting lmug http response to inets response ...")
  `#(proceed
    (#(response
      #(response ,(++ `(#(code ,(response-status resp)))
                      (convert-headers (response-headers resp)))
                 ,(binary_to_list (response-body resp)))))))

(defun set-handler (handler)
  "Set the top-level lmug application handler."
  (lmug-inets-svr:set-handler handler))

(defun get-handler ()
  "Get the top-level lmug application handler."
  (lmug-inets-svr:get-handler))

(defun call-handler (req)
  "Call the top-level lmug application handler by name and with the given
  args."
  (logjam:debug "Calling top-level lmug app handler '~p' ..." `(,(get-handler)))
  (funcall (get-handler) req))

;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
;;; Utility functions
;;; >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

(defun convert-headers (headers)
  (lists:map #'convert-header/1 headers))

(defun convert-header
  ((`#(,name ,value))
    `#(,(convert-header-name name) ,(binary_to_list value))))

(defun convert-header-name (name)
  (-> name
      (binary:replace #"-" #"_")
      (binary_to_list)
      (string:to_lower)
      (list_to_atom)))
