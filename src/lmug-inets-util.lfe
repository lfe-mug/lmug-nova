(defmodule lmug-inets-util
  (export all))

(defun get-version ()
  (lutil:get-app-version 'barista))

(defun get-version ()
  (++ (lutil:get-versions)
      `(#(barista ,(get-version)))))
