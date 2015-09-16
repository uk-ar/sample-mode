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
  (regexp-opt '("+" "*" "," ";" ">" ">=" "<" "<=" ":=" "=" "if" "then" "begin" "end" "class" ":" "?" "else" "{" "}")));;
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
   ((looking-at "} else if");;}[ \t\n]*
    (goto-char (match-end 0))
    "}elseif")
   ;; ((and (looking-at "{") (looking-back "class [^{]+"))
   ;;  (forward-char 1)
   ;;  "class-{");;
    ;; (goto-char (match-end 0))
    ;; (if (looking-back "class [^{]+{")
    ;;     "class-{"
    ;;     "{"))
   ((looking-at "} else")
    (goto-char (match-end 0))
    "}else")
   ((looking-at sample-keywords-regexp)
    (goto-char (match-end 0))
    (match-string-no-properties 0))
   (t (buffer-substring-no-properties
       (point)
       (progn (skip-syntax-forward "w_")
              (point))))))
;;(setq smie-blink-matching-inners nil)

;; (funcall smie-forward-token-function)
;; (funcall smie-backward-token-function)
(defun sample-smie-backward-token ()
  (forward-comment (- (point)))
  (cond
   ((looking-back "} else if")
    (goto-char (match-beginning 0))
    "}elseif")
   ;; ((and (looking-back "{") (looking-back "class [^{]+{"))
   ;;  (backward-char 1)
   ;;  "class-{");;
    ;; ;;(goto-char (match-beginning 0))
    ;; (if
    ;;   "{"))
   ((looking-back "} else")
    (goto-char (match-beginning 0))
    "}else")
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
      ;; (ids (ids "," ids) (id))
      ;; (class-id (id ":" ids) (id))
      (inst ;;("{" insts "}")
            ("begin" insts "end")
            ;;(exp "?" exp ":" exp)
            ;;(id ":=" exp)
            ;;("class" id ":" exps)
            ("class" exps)
            (exp))
      (insts (insts ";" insts) (inst))
      (exp (exp "+" exp)
           (exp "*" exp)
           ("{" insts "}")
           ("(" exps ")"))
      (exps (exps "," exp)
            (id ":" exp)
            (exp))
      )
    '((assoc "elseif"))
    '((assoc ":") (assoc ","));;
    '((assoc ";"))
    '((assoc "+") (assoc "*")))))

(setq offset 2)
(defun sample-smie-rules (kind token)
  (pcase (cons kind token)
    (`(:elem . basic) sample-indent-basic)
    ;;(`(,_ . ",") (smie-rule-separator kind))
    ;;(`(:after . ",") (smie-rule-parent))
    ;;(`(:before . ",") (smie-rule-parent))
    ;; (`(:after . ",")
    ;;  ;; after :
    ;;  (when offset
    ;;    (smie-rule-parent offset)
    ;;    ))
    (`(:after . ":=") sample-indent-basic)
    (`(:before . ,(or `"begin" `"(" `"{"))
     (if (smie-rule-hanging-p) (smie-rule-parent)))
    (`(:before . "if")
     (and (not (smie-rule-bolp)) (smie-rule-prev-p "else")
        (smie-rule-parent)))))

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
