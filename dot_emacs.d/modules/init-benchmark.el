;;; init-benchmark.el --- Measure startup and require times -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

(use-package benchmark-init
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(provide 'init-benchmark)
;;; init-benchmark.el ends here
