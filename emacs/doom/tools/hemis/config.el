;;; tools/hemis/config.el -*- lexical-binding: t; -*-

(use-package! hemis
  :commands (hemis-notes-mode hemis-add-note hemis-list-notes hemis-refresh-notes)
  :hook (prog-mode . hemis-notes-mode)
  :config
  ;; Adjust these to point at your Hemis backend (protocol v2).
  (setq hemis-backend (or (getenv "HEMIS_BACKEND")
                          hemis--default-backend)))
