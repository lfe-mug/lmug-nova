(defmodule lmug-inets-header
  (export all))

(defun bins-map->proplist (headers)
  (lists:map #'bins-tuple->vals/1 (maps:to_list headers)))

(defun bins-tuple->vals
  ((`#(,name ,value))
   (vals->strings-tuple name value)))

(defun vals->strings-tuple
  ((name value) (when (and (is_list name) (is_list value)))
   `#(,name ,value))
  ((name value) (when (is_binary name))
   (vals->strings-tuple (binary_to_list name) value))
  ((name value) (when (is_binary value))
   (vals->strings-tuple name (binary_to_list value))))
