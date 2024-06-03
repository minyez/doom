;;; config-cjk.el -*- lexical-binding: t; -*-

(map! :leader
      (:prefix-map ("e" . "extra-my")
        (:prefix-map ("i" . "input")
         :desc "Chinese (Pyim)"  "c"  '(lambda () (interactive) (set-input-method "pyim"))
         :desc "Japanese"        "j"  '(lambda () (interactive) (set-input-method "japanese"))
         :desc "Tex"             "t"  '(lambda () (interactive) (set-input-method "TeX"))
)))

(use-package! pyim
  :custom
  ;; Disable word search in current buffer.
  (pyim-candidates-search-buffer-p nil)
  :config
  (global-set-key (kbd "M-\\") 'pyim-convert-string-at-point)
  (setq pyim-page-tooltip '(posframe popup))
  (setq pyim-dcache-auto-update t)
  ;; pyim-shuangping is derived from xiaohe-shuangpin
  ;;(pyim-default-scheme 'pyim-shuangpin)
  ;; disable pyim-outcome-trigger for pyim-shuangpin otherwise cannot type o for 欧
  ;;(setq pyim-outcome-trigger nil)
  (pyim-default-scheme 'xiaohe-shuangpin)
  (setq pyim-page-length 9)
  ;; 中文使用全角标点，英文使用半角标点。
  (setq pyim-punctuation-translate-p '(auto yes no))
  ;; 加入个人词库，包括发音和词频。
  (setq my/personal-dicts
        '(("手动记录" . "manual_personal.pyim")
          ("搜狗导出 (2022-02-19)" . "sougou_out_2022_02_19.pyim")))
  (dolist (elem my/personal-dicts)
    (add-to-list 'pyim-dicts
        `(:name ,(car elem)
          :file ,(concat doom-private-dir "dict/" (cdr elem)))))
  ;; painless CN/EN switch by probe
  (defun my/pyim-probe-org-src-block ()
    "自定义探针, 进入 org-mode source block 之后自动切换到英文输入"
    (when (eq major-mode 'org-mode)
      (not (eq (org-in-src-block-p) nil)))
    )
  ;; auto-english 会根据之前的字符来判断是否切换到英文输入, 输入空格时自动切换到英文
  ;; 具体可用 describe-function 查看 docstring 来了解
  ;; 在 latex 块和源码块中全部为英文输入
  (setq-default pyim-english-input-switch-functions
                '(pyim-probe-auto-english
                  pyim-probe-org-latex-mode
                  my/pyim-probe-org-src-block
                  ; pyim-probe-org-structure-template
                  pyim-probe-program-mode))
  ;; 半角标点。主要情形是在行首使用 yasnippet 时有用
  (setq-default pyim-punctuation-half-width-functions
              '(pyim-probe-punctuation-line-beginning pyim-probe-punctuation-after-punctuation))
  ;; Enable key-cord when pyim input is switched on,
  ;; This is very useful in evil-insert-mode, if
  ;; from https://emacs-china.org/t/pyim-key-chord-pyim/20633/12
  ;; 这比之前用的 pyim-self-insert-command 延迟更长一点，但泛用性更高
  (defun pyim--enable-key-chord-fun (orig key)
    (if (key-chord-lookup-key (vector 'key-chord key))
        (let ((result (key-chord-input-method key)))
          (if (eq (car result) 'key-chord)
              result
            (funcall orig key)))
      (funcall orig key)))
  (advice-add 'pyim-input-method :around #'pyim--enable-key-chord-fun)

  ;; orderless 支持拼音搜索候选项功能
  (defun my-orderless-regexp (orig-func component)
    (let ((result (funcall orig-func component)))
      (pyim-cregexp-build result)))
  (advice-add 'orderless-regexp :around #'my-orderless-regexp)
)

(provide 'config-cjk)
;;; config-cjk.el ends here
