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
   ((looking-back sample-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))

(defvar sample-indent-basic 2)

;; (defvar sample-smie-grammar
;;   (smie-prec2->grammar
;;    (smie-bnf->prec2
;;     '((id)
;;       (inst ("begin" insts "end")
;;             ;;(block )
;;             ("{" inst "}")
;;             ("if" exp "then" inst "else" inst "end")
;;             ("if" exp "then" inst "else" inst "}")
;;             ("if" inst "else" inst "}")
;;             (id ":=" exp)
;;             (exp))
;;       (insts (insts ";" insts) (inst))
;;       (exp (exp "+" exp)
;;            (exp "" exp)
;;            ("(" exps ")"))
;;       (exps (exps "," exps) (exp)))
;;     '((assoc ";"))
;;     '((assoc ","))
;;     '((assoc "+") (assoc "")))))

(defvar sample-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (inst ("begin" insts "end")
            ("if" exp "then" inst "else" inst)
            (id ":=" exp)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("(" exps ")"))
      (exps (exps "," exps) (exp)))
    '((assoc ";"))
    '((assoc ","))
    '((assoc "+") (assoc "*")))))

;; (defun sample-smie-rules (kind token)
;;   (pcase (cons kind token)
;;     (`(:elem . basic) sample-indent-basic)
;;     (`(,_ . ",") (smie-rule-separator kind))
;;     (`(:after . ":=") sample-indent-basic)
;;     (`(:before . ,(or "begin""(" "{"))
;;      (if (smie-rule-hanging-p) (smie-rule-parent)))
;;     ;;((:after . "if") '(column . 0))
;;     (`(:close-all . "}") sample-indent-basic)
;;     ;;((:after . "if") '(column . 0))
;;     (`(:after . ,(or"end" ")""}")) 0)
;;     (`(:before . "if")
;;      (and (not (smie-rule-bolp))
;;           (smie-rule-prev-p "else")
;;           ;; (save-excursion
;;           ;; (sample-smie-backward-token);;beginning of else
;;           ;; (cons 'column (current-column)))
;;           (smie-rule-parent)
;;           ))))
(defun sample-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sample-indent-basic)
    (`(,_ . ",") (smie-rule-separator kind))
    (`(:after . ":=") sample-indent-basic)
    (`(:before . ,(or `"begin" `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
          (smie-rule-parent)))))

(defun verbose-sample-smie-rules (kind token)
  (let ((value (sample-smie-rules kind token)))
    (message "%s '%s'; sibling-p:%s parent:%s:%s bolp:%s hanging:%s == %s" kind token
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
