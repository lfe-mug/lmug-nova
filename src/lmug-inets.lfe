(defmodule lmug-inets
  (export all))

(include-lib "inets/include/httpd.hrl")
(include-lib "logjam/include/logjam.hrl")

(defun do (ewsapi-mod-record)
  "The `do` function is part of the EWSAPI specification.

  This is the means by which the EWSAPI request/response may be intercepted by
  lmug, allowing us to insert arbitrary middleware in the request processing
  path."
  (let* ((req (lmug-inets-request:inets->map ewsapi-mod-record))
         (resp (lmug-state:call-handler req))
         (inets-resp (lmug-inets-response:map->inets resp)))
    (log-debug "Converted inets/http response: ~p" (list inets-resp))
    `#(proceed ,inets-resp)))

(defun start (handler opts)
  "Start the inets http server."
  (logjam:set-dev-config)
  (application:ensure_all_started 'logjam)
  (lmug-state:start)
  (lmug-state:set-handler handler)
  (let* ((opts (lmug-inets-opts:add-default opts))
         (`#(ok ,pid) (inets:start 'httpd opts)))
    (log-debug "Server info: ~p~n" (list (httpd:info pid)))))

(defun stop ()
  (lmug-state:stop)
  (inets:stop))

(defun version ()
  (lmug-inets-version:get))
