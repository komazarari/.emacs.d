(when (and (eq system-type 'gnu/linux)
           (getenv "WSLENV"))

  (defun wsl-paste ()
    (interactive)
    (insert (shell-command-to-string "/mnt/c/Program\\ Files/PowerShell/7/pwsh.exe -command 'Get-Clipboard'")))
  (global-set-key (kbd "C-c C-v") 'wsl-paste)  

  )
