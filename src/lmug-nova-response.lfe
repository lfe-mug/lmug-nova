(defmodule lmug-nova-response
  (export all))

(include-lib "logjam/include/logjam.hrl")

(defun map->nova
  "Convert lmug/LFE http response map to a Nova response."
  (((= `#m(status ,status headers ,headers body ,body) resp))
   (log-debug "Converting resp: ~p" (list resp))
   `(#(response
       #(response ,(headers->nova status headers body)
                  ,body)))))

(defun headers->nova (status headers body)
  (let* ((ct-key #"Content-Type")
         (ct (content-type headers ct-key))
         (hs (maps:without `(,ct-key) headers))
         (hs (lmug-nova-header:bins-map->proplist hs)))
    (++ hs
        `(#(code ,status)
          #(content_type ,ct)
          #(content_length ,(integer_to_list (size body)))))))

(defun content-type (headers ct-key)
  (let ((ct (mref headers ct-key)))
    (if (is_binary ct)
      (binary_to_list ct)
      ct)))
