(defmodule lmug-inets-header
  (export all))

(defun strings->bins (headers)
  (lists:map #'bins->atom-string/1 headers))

(defun bins->proplist (headers)
  (lists:map #'bins->atom-string/1 headers))

(defun bins->atom-string
  ((`#(,name ,value))
    `#(,(convert-header-name name) ,(binary_to_list value))))

(defun convert-header-name (name)
  (clj:-> name
          (binary:replace #"-" #"_")
          (binary_to_list)
          (string:to_lower)
          (list_to_atom)))
