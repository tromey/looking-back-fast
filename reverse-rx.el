
(defconst rvrx--ordinary-subpatterns
  '(and : seq sequence submatch group submatch-n group-n or |
	minimal-match maximal-match zero-or-more 0+
	* *? one-or-more 1+ + +? zero-or-one optional opt ? ??))

(defconst rvrx--pass-through
  '(any in char not syntax category))

(defconst rvrx--mirror-alist
  '((line-start . line-end)
    (bol . eol)
    (string-start . string-end)
    (bos . eos)
    (bot . eot)
    (buffer-start . buffer-end)
    (word-start . word-end)
    (bow . eow)
    (symbol-start . symbol-end)))

(defun rvrx-reverse (regexp)
  (pcase regexp
    ((pred stringp)
     (reverse regexp))

    ((pred symbolp)
     (let ((val (assq regexp rvrx--mirror-alist)))
       (if val
	   (cdr val)
	 (car (rassq regexp rvrx--mirror-alist)))))

    ((and `(,x . ,rest) (guard (memq x rvrx--ordinary-subpatterns)))
     (cons x (reverse (mapcar #'rvrx-reverse rest))))

    (`(repeat . (,n . (,m . (,sexp . nil))))
     (list 'repeat n m (rvrx-reverse sexp)))
    (`(repeat . (,n . (,sexp . nil)))
     (list 'repeat n (rvrx-reverse sexp)))
    (`(** . (,n . (,m . ,rest)))
     `(= ,n ,m . (mapcar #'rvrx-reverse rest)))
    (`(= . (,n . ,rest))
     `(= ,n . (mapcar #'rvrx-reverse rest)))
    (`(>= . (,n . ,rest))
     `(>= ,n . (mapcar #'rvrx-reverse rest)))

    (`(backref . ,rest)
     ;; We could though, with a trick and more work.  The idea is,
     ;; swap backrefs with their original expression so that the
     ;; original expression comes first in the resulting regexp.
     ;; Then, after matching backwards, save just the final location,
     ;; and then do one more forward regexp match, using the original
     ;; (non-reversed) regexp.
     (error "'backref form not handled here"))

    (`(eval . ,rest)
     (error "'eval form not handled here"))
    (`(regexp . ,rest)
     (error "'regexp form not handled here"))

    ((and `(,op . ,_) (guard (memq op rvrx--pass-through)))
     regexp)

    ((pred listp)
     (error "unrecognized form %S" regexp))

    (x x)))
