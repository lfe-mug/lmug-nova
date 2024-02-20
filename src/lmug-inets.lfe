(defmodule lmug-inets
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun do (ewsapi-mod-record)
  "The `do` function is part of the EWSAPI specification.

  This is the means by which the EWSAPI request/response may be intercepted by
  lmug, allowing us to insert arbitrary middleware in the request processing
  path."
  (log-debug "ewsapi-mod-record: ~p" (list ewsapi-mod-record))
  (let* ((req (lmug-inets-request:inets->map ewsapi-mod-record))
         (_ (log-debug "Parsed request: ~p" (list req)))
         (resp (lmug-state:call-handler req))
         (_ (log-debug "lmug response: ~p" (list resp)))
         (inets-resp (lmug-inets-response:map->inets resp)))
    (log-debug "Converted inets/http response: ~p" (list inets-resp))
    `#(proceed ,inets-resp)))

(defun start (handler)
  (start handler #m()))

(defun start (handler opts-map)
  "Start the inets http server."
  (let* ((doc-root (case (lmug-state:get-docroot)
                     ('undefined (lmug-inets-opts:doc-root))
                     (root root)))
         (opts-map  `#m(server_name ,(maps:get 'name opts-map (lmug-inets-opts:server-name))
                        port ,(maps:get 'port opts-map (lmug-inets-opts:port))
                        server_root ,doc-root
                        document_root ,doc-root))
         (opts (maps:to_list opts-map)))
    (log-debug "inets HTTP server opts: ~p" (list opts))
    (application:ensure_all_started 'inets)
    (lmug-state:set-handler handler)
    (let* ((opts (lmug-inets-opts:add-default opts))
           (`#(ok ,pid) (inets:start 'httpd opts)))
      (log-debug "Server info: ~p~n" (list (httpd:info pid))))))

(defun stop ()
  (lmug-state:stop)
  (inets:stop))

(defun version ()
  (lmug-inets-version:get))
