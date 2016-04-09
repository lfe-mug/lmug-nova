(defmodule lmug-inets
  (export all))

(defun do
  ((`#(response #(,status-code ,body)))
    'noop)
  ((`#(response #(response ,head ,body)))
    'noop))
