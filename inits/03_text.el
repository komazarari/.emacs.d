;; google 翻訳
;; from http://emacs.rubikitch.com/google-translate/

(use-package google-translate
  :config
  (defvar google-translate-english-chars "[:ascii:]’“”–"
  "これらの文字が含まれているときは英語とみなす")
  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))
  (global-set-key (kbd "C-c t") 'google-translate-enja-or-jaen)
  )


;;
(setq org-use-speed-commands t)
(defun my-ac-mozc-setup ()
  (setq ac-sources
        '(ac-source-mozc ac-source-abbrev ac-source-dictionary ac-source-words-in-same-mode-buffers))
  (setq ac-mozc-remove-space nil)
  (setq ac-auto-start 2)
  )
(add-hook 'org-mode-hook 'my-ac-mozc-setup)
(add-hook 'text-mode-hook 'my-ac-mozc-setup)
