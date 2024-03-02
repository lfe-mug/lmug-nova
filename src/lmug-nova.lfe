(defmodule lmug-nova
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun do (ewsapi-mod-record)
  "The `do` function is part of the EWSAPI specification.

  This is the means by which the EWSAPI request/response may be intercepted by
  lmug, allowing us to insert arbitrary middleware in the request processing
  path."
  (log-debug "ewsapi-mod-record: ~p" (list ewsapi-mod-record))
  (let* ((req (lmug-nova-request:nova->map ewsapi-mod-record))
         (_ (log-debug "Parsed request: ~p" (list req)))
         (resp (lmug-state:call-handler req))
         (_ (log-debug "lmug response: ~p" (list resp)))
         (nova-resp (lmug-nova-response:map->nova resp)))
    (log-debug "Converted Nova response: ~p" (list nova-resp))
    `#(proceed ,nova-resp)))

(defun start (handler)
  (start handler #m()))

(defun start (handler opts-map)
  "Start the Nova http server."
  (let* ((doc-root (case (lmug-state:get-docroot)
                     ('undefined (lmug-nova-opts:doc-root))
                     (root root)))
         (opts-map  `#m(server_name ,(maps:get 'name opts-map (lmug-nova-opts:server-name))
                        port ,(maps:get 'port opts-map (lmug-nova-opts:port))
                        server_root ,doc-root
                        document_root ,doc-root))
         (opts (maps:to_list opts-map)))
    (log-debug "Nova HTTP server opts: ~p" (list opts))
    (application:ensure_all_started 'nova)
    (lmug-state:set-handler handler)
    (let* ((opts (lmug-nova-opts:add-default opts))
           (`#(ok ,pid) (nova:start 'httpd opts)))
      (log-debug "Server info: ~p~n" (list (httpd:info pid))))))

(defun stop ()
  (lmug-state:stop)
  (nova:stop))

(defun version ()
  (lmug-nova-version:get))
