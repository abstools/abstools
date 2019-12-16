This directory contains notes towards getting language server support for abs implemented.

## Compiling

After running `cd .. ; ./gradlew assemble` (i.e., run the gradlew script in the project root directory), the language server will be in a file called `./build/libs/org.abs_models.xtext.ide-<something>-ls.jar` (where `<something>` is a version number, number of commits since that version, and the git hash).

Additionally, the task `plainJar` copies the result of the most recent build into `dist/abs-language-server.jar`.

## Running in Emacs

First, install `abs-mode` and `lsp-mode`.

To use the abs language server, turn off `abs-mode`’s normal syntax checker and register the language server:

```elisp
(remove-hook 'abs-mode-hook 'abs-flymake-mode-on)
(add-to-list 'lsp-language-id-configuration
             '(abs-mode . "abs"))
(lsp-register-client
 (make-lsp-client :major-modes '(abs-mode)
                  :server-id 'abs-ls
                  :new-connection (lsp-stdio-connection '("java" "-jar" "/path/to/org.abs_models.xtext.ide/dist/abs-language-server.jar"))))
```

Then, open an abs file and turn on lsp support via `M-x lsp`.

### Debugging

- `M-x lsp-describe-session` to see a list of language servers and their respective directories

- `(setq lsp-log-io t)` to turn on logging
