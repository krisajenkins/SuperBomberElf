;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil (eval local-set-key
               (kbd "M-e")
               '(lambda ()
                  (interactive)
                  (require 'magit)
                  (let ((compile-command (format "cd %sclient ; elm-make --warn --yes --output=dist/index.html src/App.elm"
                                                 (magit-toplevel))))
                    (save-buffer)
                    (compile compile-command))))))
