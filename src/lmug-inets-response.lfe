(defmodule lmug-inets-response
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun map->inets
  "Convert lmug/LFE http response map to an inets http response."
  (((= `#m(status ,status headers ,headers body ,body) resp))
   (log-debug "Converting resp: ~p" (list resp))
   `(#(response
       #(response ,(headers->inets status headers body)
                  ,body)))))

(defun headers->inets (status headers body)
  (let* ((ct-key #"Content-Type")
         (ct (content-type headers ct-key))
         (hs (maps:without `(,ct-key) headers))
         (hs (lmug-inets-header:bins-map->proplist hs)))
    (++ hs
        `(#(code ,status)
          #(content_type ,ct)
          #(content_length ,(integer_to_list (size body)))))))

(defun content-type (headers ct-key)
  (let ((ct (mref headers ct-key)))
    (if (is_binary ct)
      (binary_to_list ct)
      ct)))
