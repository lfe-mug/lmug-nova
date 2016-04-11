(defmodule lmug-inets
  (export all))

(include-lib "clj/include/compose.lfe")
(include-lib "lmug/include/response.lfe")

(defun log (msg arg)
  (logjam:debug msg `(,arg))
  arg)

(defun do (mod-data)
  ""
  (logjam:debug "Got mod-data: ~n~p" `(,mod-data))
  (->> mod-data
       (lmug-inets-adptr:convert-request)
       ;; XXX debug
       (log "Request data: ~p")
       (lmug-inets-adptr:call-handler)
       (log "Handler data: ~p")
       (lmug-inets-adptr:convert-response)
       (log "Response data: ~p")))

(defun run (lmug-handler opts)
  ""
  (logjam:start)
  (lmug-inets-svr:start)
  (lmug-inets-adptr:set-handler lmug-handler)
  (inets:start)
  (inets:start
    'httpd
    (-> opts
        (lmug-inets-opts:set-defaults)
        (lmug-inets-opts:add-module 'lmug-inets))))
