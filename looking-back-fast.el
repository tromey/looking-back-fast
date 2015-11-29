;;; looking-back-fast.el - Faster looking-back. -*- lexical-binding:t -*-

(require 'lex)
(require 'lex-parse-re)

;; Subpatterns taking a sequence of sexps that can be reversed by
;; reversing the arguments.
(defconst looking-back-fast--ordinary-subpatterns
  '(and : seq sequence submatch group submatch-n group-n or |
	minimal-match maximal-match zero-or-more 0+
	* *? one-or-more 1+ + +? zero-or-one optional opt ? ??))

;; If a pattern is a list whose car is one of these symbols, it can be
;; passed through without change.
(defconst looking-back-fast--pass-through
  '(any in char not syntax category))

;; Symbols that are mapped to a mirror symbol in the reversed regexp.
;; This is a bidirectional alist, used with both assq and rassq.
(defconst looking-back-fast--mirror-alist
  '((line-start . line-end)
    (bol . eol)
    (string-start . string-end)
    (bos . eos)
    (bot . eot)
    (buffer-start . buffer-end)
    (word-start . word-end)
    (bow . eow)
    (symbol-start . symbol-end)))

(defun looking-back-fast-reverse (regexp)
  "Reverse a regular expression.

REGEXP is an Emacs regular expression in the form used by `rx'.
This returns a regular expression, also in `rx' form, that is the
reverse of REGEXP.  That is, if REGEXP matches a string, then the
result will match the reversed string."
  (pcase regexp
    ((pred stringp)
     (reverse regexp))

    ((pred symbolp)
     (let ((val (assq regexp looking-back-fast--mirror-alist)))
       (if val
	   (cdr val)
	 (car (rassq regexp looking-back-fast--mirror-alist)))))

    ((and `(,x . ,rest)
	  (guard (memq x looking-back-fast--ordinary-subpatterns)))
     (cons x (reverse (mapcar #'looking-back-fast-reverse rest))))

    (`(repeat . (,n . (,m . (,sexp . nil))))
     (list 'repeat n m (looking-back-fast-reverse sexp)))
    (`(repeat . (,n . (,sexp . nil)))
     (list 'repeat n (looking-back-fast-reverse sexp)))
    (`(** . (,n . (,m . ,rest)))
     `(= ,n ,m . (mapcar #'looking-back-fast-reverse rest)))
    (`(= . (,n . ,rest))
     `(= ,n . (mapcar #'looking-back-fast-reverse rest)))
    (`(>= . (,n . ,rest))
     `(>= ,n . (mapcar #'looking-back-fast-reverse rest)))

    (`(backref . ,_)
     ;; We could though, with a trick and more work.  The idea is,
     ;; swap backrefs with their original expression so that the
     ;; original expression comes first in the resulting regexp.
     ;; Then, after matching backwards, save just the final location,
     ;; and then do one more forward regexp match, using the original
     ;; (non-reversed) regexp.
     (error "'backref form not handled here"))

    (`(eval . ,_)
     (error "'eval form not handled here"))
    (`(regexp . ,_)
     (error "'regexp form not handled here"))

    ((and `(,op . ,_) (guard (memq op looking-back-fast--pass-through)))
     regexp)

    ((pred listp)
     (error "unrecognized form %S" regexp))

    (x x)))

(defun lex-match-buffer-backward (lex &optional stop)
  "Match LEX against buffer between point and STOP, working backwards.
Return a triplet (VALUE ENDPOS . LEXER) where VALUE is the
value of returned by the lexer for the match found (or nil), ENDPOS
is the end position of the match found (or nil), and LEXER is the
state of the engine at STOP, which can be passed back to
continue the match elsewhere."
  ;; FIXME: Move this to C.
  (unless stop  (setq stop (point-min)))
  (let ((start (point))
        (match (list nil nil))
        (lastlex lex))
    (while
        (progn
          (while (eq (car lex) 'check)
            (setq lex (if (funcall (car (nth 1 lex)) (cdr (nth 1 lex))
                                   start)
                          (nth 2 lex) (nthcdr 3 lex))))
          (when (eq (car lex) 'stop)
            ;; Don't stop yet, we're looking for the longest match.
            (setq match (list (cadr lex) start))
            (message "Found match: %s" match)
            (setq lex (cddr lex)))
          (cl-assert (not (eq (car lex) 'stop)))
          (and lex (>= start stop)))
      (let ((c (char-before start)))
        (setq start (1- start))
        (setq lex (cond
                   ((eq (car lex) 'table) (aref (cdr lex) c))
                   ((integerp (car lex)) (if (eq c (car lex)) (cdr lex)))))
        (setq lastlex lex)))
    (message "Final search pos considered: %s" start)
    ;; The difference between `lex' and `lastlex' is basically that `lex'
    ;; may depend on data after `stop' (if there was an `end-of-file' or
    ;; `word-boundary' or basically any `check').  So let's return `lastlex'
    ;; so it can be correctly used to continue the match with a different
    ;; content than what's after `stop'.
    (nconc match lastlex)))

(defun looking-back-fast--do (regexp &optional stop)
  (let ((lexer (lex-compile (looking-back-fast-reverse
			     (lex-parse-re regexp)))))
    (lex-match-buffer-backward lexer stop)))

(defmacro looking-back-fast (regexp &optional stop)
  (if (stringp regexp)
      ;; Do the work at compile time.
      (let ((lexer (lex-compile (looking-back-fast-reverse
				 (lex-parse-re regexp)))))
	`(cadr (lex-match-buffer-backward ,lexer ,stop)))
    ;; Do the work later.
    `(cadr (looking-back-fast--do ,regexp, stop))))

;;; looking-back-fast.el ends here
