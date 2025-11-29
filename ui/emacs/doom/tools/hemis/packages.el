;;; tools/hemis/packages.el -*- lexical-binding: t; -*-

;; Hemis frontend from local repo
(package! hemis
  :recipe (:local-repo "/Users/joel/Work/hemis/emacs"
           :files ("hemis.el")))
