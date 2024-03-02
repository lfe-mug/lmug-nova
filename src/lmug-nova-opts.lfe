(defmodule lmug-nova-opts
  (export all))

(defun server-name ()
  "lmugnova")

(defun port ()
  5099)

(defun doc-root ()
  ".")

(defun add-default-mimetypes (opts)
  (++ opts
      `(#(mime_types ,(lmug:content-types->proplist)))))

(defun add-logging (opts)
  (++ opts
      `(#(logger (#(error ,(list_to_atom (server-name)))))
        #(log_format combined))))

(defun add-server-tokens (opts)
  (++ opts
      '(#(server_tokens full))))

(defun add-server-name (opts)
  (++ opts
      `(#(server_name ,(server-name)))))

(defun add-default (opts)
  (clj:-> opts
          (add-default-mimetypes)
          (add-logging)
          (add-server-tokens)
          (add-server-name)))