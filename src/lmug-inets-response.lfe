(defmodule lmug-inets-response
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun map->inets (resp)
  "Convert lmug/LFE http response map to an inets http response."
  (log-debug "Converting resp: ~p" (list resp))
  (let ((body (mref resp 'body)))
    `#(proceed
       (#(response
          #(response ,(++ (mref resp 'headers)
                          `(#(code ,(mref resp 'status))
                            #(content_length ,(integer_to_list (size body)))))
                     ,body))))))