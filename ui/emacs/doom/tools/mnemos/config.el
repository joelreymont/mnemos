;;; tools/mnemos/config.el -*- lexical-binding: t; -*-

(use-package! mnemos
  :commands (mnemos-notes-mode mnemos-add-note mnemos-list-notes mnemos-refresh-notes)
  :hook (prog-mode . mnemos-notes-mode)
  :config
  ;; Adjust these to point at your Mnemos backend (protocol v2).
  (setq mnemos-backend (or (getenv "MNEMOS_BACKEND")
                          mnemos--default-backend)))
