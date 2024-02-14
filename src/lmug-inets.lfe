(defmodule lmug-inets
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun log (msg arg)
  (log-debug msg `(,arg))
  arg)

(defun do
  "
  "
  (((= (match-mod init_data init-data
                  data ewsapi-data
                  config_db cfg
                  method method
                  absolute_uri abs-uri
                  request_uri req-uri
                  http_version http-version
                  request_line req-line
                  parsed_header headers
                  connection connected?
                  socket_type socket-type
                  socket socket
                  entity_body body) mod-data))
   (let ((`#(init_data #(,remote-port ,remote-addr) ,_ ,_) init-data))
     (log-debug "Got mod-data: ~n~p" (list mod-data))
     (log-debug "cfg: ~p" (list cfg))
     (log-debug "init-data: ~p" (list init-data))
     (log-debug "remote-addr: ~p" (list remote-addr))
     (log-debug "method: ~p" (list method))
     (log-debug "method: ~p" (list (list_to_atom (string:to_lower method))))
     (log-debug "connected?: ~p" (list connected?))
     (log-debug "ewsapi-data: ~n~p" (list ewsapi-data))
     (log-debug "abs-uri ~p" (list abs-uri))
     (log-debug "headers: ~p" (list headers))
     (lfe_io:format "~n~p~n" (list headers))
     (log-debug "headers: ~p" (list (http.header:list->map headers)))
     (log-debug "req-line ~p" (list req-line))
     (log-debug "socket-type: ~p" (list socket-type))
     (log-debug "socket: ~p" (list socket))
     (log-debug "body: ~p" (list body))
     (log-debug "body: ~p" (list (list_to_binary body)))
     ;;`#(proceed ,ewsapi-data))))

   (let ((req (lmug-inets-request:inets->map mod-data)))
     (log-debug "Created req: ~n~p" `(,req))
     (let ((resp (lmug-inets-state:call-handler req)))
       (log-debug "Created resp: ~n~p" `(,resp))
       (let ((inets-resp (lmug-inets-response:map->inets resp)))
         (log-debug "Created inets resp: ~n~p" `(,inets-resp))
         inets-resp))))))

(defun start (lmug-handler opts)
  ""
  (logjam:set-dev-config)
  (application:ensure_all_started 'logjam)
  (lmug-inets-state:start)
  (lmug-inets-state:set-handler lmug-handler)
  (inets:start)
  (let ((`#(ok ,pid) (inets:start 'httpd opts)))
    (log-debug "Server info: ~p~n" (list (httpd:info pid)))))

(defun stop ()
  (lmug-inets-state:stop)
  (inets:stop))

(defun version ()
  (lmug-inets-version:get))
