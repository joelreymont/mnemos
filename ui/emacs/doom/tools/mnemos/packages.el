;;; tools/mnemos/packages.el -*- lexical-binding: t; -*-

;; Mnemos frontend from local repo
(package! mnemos
  :recipe (:local-repo "/Users/joel/Work/mnemos/ui/emacs"
           :files ("mnemos.el")))
