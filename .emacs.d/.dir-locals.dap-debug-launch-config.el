;; .dir-locals.el — project-specific, lives in the repo
;; Name them with the project name so they're identifiable in the picker
((nil . ((eval . (with-eval-after-load 'dap-mode
                   (dap-register-debug-template
                     "myproject: main"
                     (list :type "lldb-vscode"
                           :request "launch"
                           :name "myproject: main"
                           :program "${workspaceFolder}/build/myproject"
                           :args []
                           :cwd "${workspaceFolder}"
                           :stopOnEntry nil))
                   (dap-register-debug-template
                     "myproject: tests"
                     (list :type "lldb-vscode"
                           :request "launch"
                           :name "myproject: tests"
                           :program "${workspaceFolder}/build/tests/myproject_tests"
                           :args ["--gtest_filter=*"]
                           :cwd "${workspaceFolder}"
                           :stopOnEntry nil)))))))
