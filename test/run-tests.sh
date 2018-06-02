emacs -batch -l ert \
      -l ../stream.el \
      -l stream-test.el \
      -l markdown-mode.el \
      -l ../task-tags-mode.el \
      -l task-tags-test.el \
      -f ert-run-tests-batch-and-exit
