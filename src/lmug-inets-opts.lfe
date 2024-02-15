(defmodule lmug-inets-opts
  (export all))

(defun server-name ()
  "lmuginets")

(defun default-modules ()
  "The default EWSAPI modules."
  '(mod_alias mod_auth mod_esi mod_actions mod_cgi mod_dir mod_get mod_head
    mod_log mod_disk_log lmug-inets))

(defun minimal-modules ()
  "The default EWSAPI modules."
  '(mod_alias))

(defun add-default-modules (opts)
  "Set all the EWSAPI option defaults."
  (++ opts
      `(#(modules ,(default-modules)))))

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
          (add-default-modules)
          (add-default-mimetypes)
          (add-logging)
          (add-server-tokens)
          (add-server-name)))