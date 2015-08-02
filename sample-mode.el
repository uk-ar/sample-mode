(require 'smie)

(defvar sample-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [foo] 'sample-do-foo)
    map)
  "Keymap for `sample-mode'.")

(defvar sample-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "<" st)
    (modify-syntax-entry ?\n ">" st)
    st)
  "Syntax table for `sample-mode'.")

(defvar sample-keywords-regexp
  (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" ":=" "=" "if" "then" "begin" "end")))

(defvar sample-font-lock-keywords
  `(,sample-keywords-regexp
    ("function \\(\\sw+\\)" (1 font-lock-function-name-face))
    )
  "Keyword highlighting specification for `sample-mode'.")

 ;;;###autoload
(define-derived-mode sample-mode prog-mode "Sample"
  "A major mode for editing Sample files."
  :syntax-table sample-mode-syntax-table
  (setq-local comment-start "# ")
  (setq-local comment-start-skip "#+\\s-*")
  (setq-local font-lock-defaults
              '(sample-font-lock-keywords))
  (smie-setup sample-smie-grammar 'verbose-sample-smie-rules
              :forward-token 'sample-smie-forward-token
              :backward-token 'sample-smie-backward-token)
  ;;(setq-local indent-line-function 'sample-indent-line)
  ;; (setq-local imenu-generic-expression
  ;;             sample-imenu-generic-expression)
  ;;(setq-local outline-regexp sample-outline-regexp)
  )

(defun sample-smie-forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at "else if")
    (goto-char (match-end 0))
    "elseif")
   ((looking-at sample-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))

(defun sample-smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back "else if" (- (point) 7) t)
    (goto-char (match-beginning 0))
    "elseif")
   ((looking-back sample-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))

(defvar sample-indent-basic 2)

(defvar sample-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("begin" insts "end")
            ;;("if" exp "then" inst "else" inst "end")
            ;;("if" exp "then" inst "else" inst "}")
            ;; ("if" inst "else" inst "}")
            ;;("else" inst "}")
            (id ":=" exp)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("(" exps ")")
           ("{" inst "}")
           ;;("if" inst "}")
           ("if" if-body "}")
           ("class" exp "}")
           ;; ("if" inst "else" inst "}")
           ;; ("if" inst "elseif" inst "}")
           ;; ("if" inst "elseif" inst "else" inst "}")
           )
      (exps (exps "," exps) (exp))

      ;;(itheni (insts) (exp "then" insts))
      ;;(ielsei (itheni) (itheni "else" insts))
      ;; (if-body (ielsei) (if-body "elseif" if-body))
      ;;(else );;
      (if-body (inst) (if-body "else" if-body) (if-body "elseif" if-body))
      )
    '((assoc "else") (assoc "elseif"))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "+") (assoc "*")))))

(defun sample-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sample-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . ":=") sample-indent-basic)
    (`(:before . ,(or `"begin" `"(" `"{"))
     (cond
      ;;((smie-rule-parent-p "if") 0)
      ;;((smie-rule-bolp) (smie-rule-parent))
      ((smie-rule-hanging-p) (smie-rule-parent))
      ))
    (`(:after . "elseif") (smie-rule-parent))
    ;;(`(:close-all . "}") (smie-rule-parent))
    ;; (`(:after . "if") (smie-rule-parent))
    (`(:after . "if") 0)
    ;;(`(:after . "{") (smie-rule-parent))
    ;;((smie-rule-hanging-p) (smie-rule-parent)))
    ;;   (if (smie-rule-parent-p "if") 0))
    (`(:before . "if")
     (cond
      ;;(smie-rule-hanging-p) (smie-rule-parent)
      ;;((smie-rule-bolp) 0)
      ((smie-rule-bolp) 0)
      ((and (not (smie-rule-bolp))
            (smie-rule-prev-p "else"))
       (smie-rule-parent))
      ;;((smie-rule-hanging-p) (smie-rule-parent))
      (t nil)
      ))))

(defun verbose-sample-smie-rules (kind token)
  (let ((value (sample-smie-rules kind token)))
    (message "%s '%s'; sib-p:%s parent:%s:%s bolp:%s hang:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             (ignore-errors (smie-rule-parent-p))
             (ignore-errors (smie-rule-bolp))
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

(and (global-indent-folding-mode -1)
     (global-electric-formatter-mode -1))

;;(and (sample-mode) (indent-buffer))
(provide 'sample-mode)
;;; sample-mode.el ends here
