;; 空白とかを可視化
(use-package whitespace
  :ensure t
  :config
  (setq whitespace-style '(
                           face           ; faceで可視化
                           trailing       ; 末尾
                           tabs           ; タブ
                           space
                           empty          ; 先頭/末尾の空行
                           space-mark     ; 表示のマッピング
                           tab-mark
                           ))
                                        ; from http://cloverrose.hateblo.jp/entry/2013/04/12/041758
  (setq whitespace-display-mappings
        '(
          ;; (space-mark   ?\     [?\u00B7]     [?.]) ; space - centered dot
          (space-mark   ?\xA0  [?\u00A4]     [?_]) ; hard space - currency
          (space-mark   ?\x8A0 [?\x8A4]      [?_]) ; hard space - currency
          (space-mark   ?\x920 [?\x924]      [?_]) ; hard space - currency
          (space-mark   ?\xE20 [?\xE24]      [?_]) ; hard space - currency
          (space-mark   ?\xF20 [?\xF24]      [?_]) ; hard space - currency
          (space-mark ?\u3000 [?\u25a1] [?_ ?_]) ; full-width-space - square
          ;; NEWLINE is displayed using the face `whitespace-newline'
          ;; (newline-mark ?\n    [?$ ?\n])  ; eol - dollar sign
          ;; (newline-mark ?\n    [?\u21B5 ?\n] [?$ ?\n])	; eol - downwards arrow
          ;; (newline-mark ?\n    [?\u00B6 ?\n] [?$ ?\n])	; eol - pilcrow
          ;; (newline-mark ?\n    [?\x8AF ?\n]  [?$ ?\n])	; eol - overscore
          ;; (newline-mark ?\n    [?\x8AC ?\n]  [?$ ?\n])	; eol - negation
          ;; (newline-mark ?\n    [?\x8B0 ?\n]  [?$ ?\n])	; eol - grade
          ;;
          ;; WARNING: the mapping below has a problem.
          ;; When a TAB occupies exactly one column, it will display the
          ;; character ?\xBB at that column followed by a TAB which goes to
          ;; the next TAB column.
          ;; If this is a problem for you, please, comment the line below.
          (tab-mark     ?\t    [?\u00BB ?\t] [?\\ ?\t]) ; tab - left quote mark
          ))
  (defvar my/bg-color "#232323")
  (set-face-attribute 'whitespace-trailing nil
                      :background my/bg-color
                      :foreground "DeepPink"
                      :underline t)
  (set-face-attribute 'whitespace-tab nil
                      :background my/bg-color
                      :foreground "LightSkyBlue"
                      :underline t)
  (set-face-attribute 'whitespace-space nil
                      :background my/bg-color
                      :foreground "GreenYellow"
                      :weight 'bold)
  (set-face-attribute 'whitespace-empty nil
                      :background my/bg-color)
  (setq whitespace-action '(auto-cleanup))
  (global-whitespace-mode 1)
  )
