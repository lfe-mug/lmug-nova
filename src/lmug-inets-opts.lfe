(defmodule lmug-inets-opts
  (export all))

(include-lib "clj/include/compose.lfe")

(defun update (opts key value)
  "Given a proplist, a key, and a value, prepend a new #(key value) to the
  opts proplist. Subsequent calls to ``(proplist:get_value key opts)`` will
  return the new value.

  This function signature is designed to be used with the ``->`` thrushing
  macro."
  (cons `#(,key ,value) opts))

(defun get-value (key proplist)
  "The same as ``get-value/3``, with an empty list as the default value.

  This function signature is designed to be used with the ``->>`` thrushing
  macro."
  (get-value key '() proplist))

(defun get-value (key default proplist)
  "Get the value associated with a proplist key; if undefined, use the default
  value instead.

  This function signature is designed to be used with the ``->>`` thrushing
  macro."
  (proplists:get_value key proplist default))

(defun prepend (opts key value)
  "Given a proplist, a key, and a value, and assuming that the key points
  to a sub-proplist, prepend a new value to the sub-proplist accessible
  at the key and return an updated opts proplist with the key and updated
  sub-proplist prepended to the opts proplist. Subsequent calls to
  ``(proplist:get_value key opts)`` will return the new sub-proplist.

  This function signature is designed to be used with the ``->`` thrushing
  macro."
  (cons (->> opts
             (get-value key)
             (cons value)
             (tuple key))
        opts))

(defun append (back front)
  "Given two lists, append the first to the second.

  This function signature is designed to be used with the ``->>`` thrushing
  macro."
  (++ front back))

(defun append (opts key value)
  "Given a proplist of options, get the list of items associated with ``key``,
  append to those the list of items in ``value``, and update the options with
  a this key-value pair.

  This function signature is designed to be used with the ``->`` thrushing
  macro."
  (->> opts
       (get-value key)
       (append value)
       (tuple key)
       (list)
       (append opts)))

(defun add-module (opts mod)
  "Add a module to the list of modules associated with the options data by the
  ``modules`` key."
  (append opts 'modules `(,mod)))

(defun get-default-modules ()
  "The default EWSAPI modules."
  '(mod_alias mod_auth mod_esi mod_actions mod_cgi mod_dir mod_get mod_head
    mod_log mod_disk_log))

(defun get-minimal-modules ()
  "The default EWSAPI modules."
  '(mod_alias))

(defun set-default-modules (opts)
  "Set the default list of EWSAPI modules."
  (update opts 'modules
    (case (proplists:get_value 'modules opts)
      ('undefined (get-default-modules))
      ('() (get-default-modules))
      (modules (++ (get-default-modules) modules)))))

(defun set-minimal-modules (opts)
  (update opts 'modules (get-minimal-modules)))

(defun set-defaults (opts)
  "Set all the EWSAPI option defaults."
  (-> opts
      (set-default-modules)))
