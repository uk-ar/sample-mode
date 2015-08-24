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
    (modify-syntax-entry ?\{ "(}" st)
    (modify-syntax-entry ?\} "){" st)
    st)
  "Syntax table for `sample-mode'.")

(defvar sample-keywords-regexp
  (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" ":=" "=" "if" "then" "begin" "end" "class" ":" "else" "?" "{" "}")))
;;"else"
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

;; swift  if e { e     }else if e {e}  else{   }
;; swift+ if e { e     }elseif e {e   }else{  end
;; ruby   if e then e  elseif   e  e   else    end

(defun sample-smie-forward-token ()
  (forward-comment (point-max))
  (cond
   ((looking-at "else if");;}[ \t\n]*
    (goto-char (match-end 0))
    "elseif")
   ;; ((and (looking-at "{") (looking-back "class [^{]+"))
   ;;  (forward-char 1)
   ;;  "class-{");;
    ;; (goto-char (match-end 0))
    ;; (if (looking-back "class [^{]+{")
    ;;     "class-{"
    ;;     "{"))
   ((looking-at "else")
    (goto-char (match-end 0))
    "else")
   ((looking-at sample-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))

;; (funcall smie-forward-token-function)
;; (funcall smie-backward-token-function)
(defun sample-smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back "else if")
    (goto-char (match-beginning 0))
    "elseif")
   ;; ((and (looking-back "{") (looking-back "class [^{]+{"))
   ;;  (backward-char 1)
   ;;  "class-{");;
    ;; ;;(goto-char (match-beginning 0))
    ;; (if
    ;;   "{"))
   ((looking-back "else")
    (goto-char (match-beginning 0))
    "else")
   ((looking-back sample-keywords-regexp (- (point) 2) t)
    (goto-char (match-beginning 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-backward "w_")
              (point))))))

(defun ast ()
  (interactive)
  (let ((this
         (save-excursion (funcall smie-forward-token-function))));;a-case
    (cond
     ((equal this "")
      (let* ((result (ignore-errors (down-list) t)))
        ;;(result (ignore-errors (up-list) t))
        (if result ;;children exist
            (concat "(" (ast))
          (ignore-errors (up-list)
                         (concat ")" (ast))
                         ;;(funcall smie-forward-token-function)
                         ;;(ast)
                         )
          )))
     (t
      ;;subl
      (funcall smie-forward-token-function)
      ;;(append (list this) (ast))
      (concat this " " (ast))
      ))))

(defvar sample-indent-basic 2)

;;(defvar swift-smie-grammar
(defvar sample-smie-grammar
  (smie-prec2->grammar
   (smie-bnf->prec2
    '((id)
      (ids (ids "," ids) (id))
      (class-id (id ":" ids) (id))
      ;; (itheni (insts) (exp "then" insts))
      ;; (ielsei (itheni) (itheni "else" insts))
      ;; (if-body (ielsei) (if-body "elseif" if-body))
      ;; ("if" if-body "end")
      ;; (itheni (id "{" insts "}"))
      ;; (ielsei (itheni "else" "{" insts "}"))
      ;; (ielseifi (ielseifi "elseif" ielseifi)
      ;;           (itheni))
      ;; (if-body (itheni)
      ;;          (ielsei)
      ;;          (ielseifi "elseif" if-body))
      ;;(class-body (class-id "class-{" insts "}"))
      ;;(class-body (id "{" insts "}"))
      ;;(class (id "class" id "{" insts "}" id))
      ;;(class-body (class-id "{" insts "}"))
      (inst ;;("{" insts "}")
            ("begin" insts "end")
            (exp "?" exp ":" exp)
            ;;("if" exp "then" inst "else" inst "end")
            ;;("if" exp "{" inst "}elseif" inst "{" inst "}else{" inst "}")
            ;;    (`(:after . ,(or "?" ":")) ruby-indent-level)
            ;;("if" if-body "}")
            ;;("if" if-body)
            ;;("class" id "{" insts "}")
            ;;(inst "class" insts)
            ;;("class" class-id "class-{" insts "}")
            ;; ("class" id "{" insts "}")
            ;; ("class" id ":" ids "{" insts "}")
            ;; ("class" id "then" insts "end")
            ;; ("class" id ":" ids "then" insts "end")

            ;;("class" class-id "{" insts "}")
            ;;("class" class-id "class-{" insts "}")
            ;;("class" class-id "then" insts "end")
            ;; (setq smie-blink-matching-inners nil)
            ;;("class" class-body) ;; ng for backward-sexp
            ;;("class" class-id "then" insts "end")

            (id ":=" exp)
            ;;("class" insts "{" insts "}")
            ;;("class" id "{" id "}")
            ;;("class" insts "class-{" insts "class-}")
            (id "(" args ")");; funcall
            (exp))
      ;;(args (args "," args) (arg))
      ;;(arg (id) (id "{" inst "}"))
      ;; conflict class foo, bar {class-body}
      ;; conflict func(foo, bar {closure})

      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ;;("(" exps ")")
           )
      ;;(exps (exps "," exps) (exp))
      ;;(if-conditional (exp) (let-decl))
      ;; (if-body ("if" if-conditional "{" insts "}"))
      ;; (if-clause (if-body)
      ;;            (if-body "elseif" if-conditional "{" insts "}")
      ;;            (if-body "else" "{" insts "}"))
      )
    '((assoc "elseif"))
    '((assoc "then") (assoc "{") (left ","))
    ;;'((assoc "}else{"))
    ;;'((assoc "}") (assoc "else"))
    ;;'((assoc "if") (assoc "else") (assoc "elseif"))
    '((assoc ";"))
    '((assoc "+") (assoc "*")))))

(defun sample-smie-rules (kind token)
  ;; (pcase (cons kind token)
  ;;   (`(:elem . basic) sample-indent-basic)
  ;;   ;; (`(,_ . ",")
  ;;   ;;  (cond
  ;;   ;;   ((and (eql kind :before) (smie-rule-parent-p ":"))
  ;;   ;;    (smie-rule-parent))
  ;;   ;;   (t (smie-rule-separator kind))
  ;;   ;;   ))
  ;;   (`(:after . ":=") sample-indent-basic)
  ;;   ;; (`(:before . ,(or `"begin" `"(" `"{" `"class-{"))
  ;;   ;;  (cond
  ;;   ;;   ;;((smie-rule-parent-p "if") 0)
  ;;   ;;   ((smie-rule-bolp)
  ;;   ;;    (smie-rule-parent))
  ;;   ;;   ((smie-rule-hanging-p) (smie-rule-parent))
  ;;   ;;   ))
  ;;   (`(:before . "{") nil)
  ;;   ;;(`(:after . "elseif") (smie-rule-parent))
  ;;   ;;(`(:close-all . "}") (smie-rule-parent))
  ;;   ;; (`(:after . "if") (smie-rule-parent))
  ;;   (`(:after . "if") 0)
  ;;   ;;(`(:after . "{") sample-indent-basic);; "{" is neither
  ;;   (`(:after . "class-{") sample-indent-basic);; "{" is neither
  ;;   ;; (`(:after . "}elseif") sample-indent-basic)
  ;;   ;; (`(:after . "}else") sample-indent-basic)
  ;;   (`(:before . "else")
  ;;    (if (smie-rule-parent-p "if" "elseif")
  ;;        (smie-rule-parent)))
  ;;   (`(:before . "elseif")
  ;;    (if (smie-rule-parent-p "if" "elseif")
  ;;        (smie-rule-parent)))
  ;;   ;;(`(:after . "class") sample-indent-basic)
  ;;   ;;(`(:after . "{") (smie-rule-parent))
  ;;   ;;((smie-rule-hanging-p) (smie-rule-parent)))
  ;;   ;;   (if (smie-rule-parent-p "if") 0))
  ;;   ;; (`(:before . "class")
  ;;   ;;  (cond
  ;;   ;;   ((smie-rule-bolp) 0)
  ;;   ;;   ))
  ;;   (`(:before . ":")
  ;;    (smie-rule-parent sample-indent-basic))
  ;;   (`(:before . "if")
  ;;    (cond
  ;;     ;;(smie-rule-hanging-p) (smie-rule-parent)
  ;;     ;;((smie-rule-bolp) 0)
  ;;     ;;((smie-rule-bolp) 0)
  ;;     ;; ((and (not (smie-rule-bolp))
  ;;     ;;       (smie-rule-prev-p "else"))
  ;;     ;;  (smie-rule-parent))
  ;;     ;;((smie-rule-hanging-p) (smie-rule-parent))
  ;;     (t nil)
  ;;     )))
  )

(defun verbose-sample-smie-rules (kind token)
  (let ((value (sample-smie-rules kind token)))
    (message "%s '%s'; sib-p:%s parent:%s bolp:%s hang:%s == %s" kind token
             (ignore-errors (smie-rule-sibling-p))
             (ignore-errors smie--parent)
             ;;(ignore-errors (smie-indent--parent))
             (ignore-errors (smie-rule-bolp))
             (ignore-errors (smie-rule-hanging-p))
             value)
    value))

;; smie--parent
;; (LEFT-LEVEL POS TOKEN): we couldn't skip TOKEN because its right-level
;; is too high.  LEFT-LEVEL is the left-level of TOKEN,
;; POS is its start position in the buffer.
;; (t POS TOKEN): same thing but for an open-paren or the beginning of buffer.
;; Instead of t, the `car' can also be some other non-nil non-number value.
;; (nil POS TOKEN): we skipped over a paren-like pair.
;; nil: we skipped over an identifier, matched parentheses, ..."

(and (global-indent-folding-mode -1)
     (global-electric-formatter-mode -1))

;;(and (sample-mode) (indent-buffer))
(provide 'sample-mode)
;;; sample-mode.el ends here
