;;; Directory Local Variables
;;; For more information see (info "(emacs) Directory Variables")

((nil .
      ;; Reduce emacs hang time; see
      ;; https://www.reddit.com/r/emacs/comments/mq2znn/no_file_descriptors_left/
      ((lsp-enable-file-watchers . nil)
       (compile-command . "./gradlew assemble"))))
