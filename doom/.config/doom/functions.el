;;; functions.el -*- lexical-binding: t; -*-

(defun +my/make-word-constituents ()
  "Make specific symbols into word constituents so they can be used in abbrevs."
  (modify-syntax-entry ?! "w")
  (modify-syntax-entry ?+ "w")
  (modify-syntax-entry ?- "w")
  (modify-syntax-entry ?. "w")
  (modify-syntax-entry ?< "w")
  (modify-syntax-entry ?= "w")
  (modify-syntax-entry ?> "w"))
