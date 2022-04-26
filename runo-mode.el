;; -*- lexical-binding: t; -*-
;;; package --- Summary

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(defvar runo-mode-hook nil)

(defvar runo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-c\C-s" 'runo-custom-syllabification)
    map)
  "Keymap for runo-mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.runo\\'" . runo-mode))

(defconst runo-diphtong
  (rx (or "ai" "ei" "oi" "ui" "yi" "äi" "öi" "au" "eu" "iu" "ou" "äy" "öy" "ey" "iy")))
(defconst runo-beginning-diphtong
  (rx (or "uo" "yö" "ie")))

(defconst runo-vowel
  "[aeiouyäö]")
(defconst runo-long-vowel
  (concat "\\(?1:" runo-vowel "\\)\\1"))
(defconst runo-consonant
  "[bcdfghjklmnpqrstvwxz]")
(defconst runo-double-consonant
  "\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1")
(defconst runo-resonant
  "\\(?1:[rlmn]\\)\\1\\{0\\}")
(defconst runo-resonant-consonant
  "[rlmn][bcdfghjkpqstvwxz]")
(defconst runo-resonant-double-consonant
  "[rlmn]\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1")

(defconst runo-kesuura ; word stop
  "\\(?:[,.?!:; \n]\\)")

(defun runo-syllable-color (syllable-type metron-type syllable-index metron-index)
  ""
  (if (null syllable-type) ; paint regexp matches blue
      (list :background "#e1e1ff")
    (let ((syllable-si (mod syllable-index 2))
	  (metron-si (mod metron-index 2)))
      (append
       (list :foreground
	     (elt (pcase (car syllable-type)
		    ('pitkä '("#003541" ;bluish
			      "#00359e"))
		    ('puolipitkä '("#7c0000" ;reddish
				   "#863541"))
		    ('lyhyt '("#645e00" ;yellowish
			      "#848000")))
		  syllable-si))
       (when metron-type ; if metron analysis was ok
	 (list :background
	       (elt (pcase (car metron-type)
		      ('daktyyli '("#ffe1e1""#ffd3cb"))
		      ('spondee '("#ffffe1" "#fff3cb"))
		      ('trokee '("#e1ffe1" "#d8fec0"))

		      ('kaksijalka '("#ffe1e1""#ffd3cb"))
		      ('kolmijalka '("#ffe1e1""#ffd3cb"))
		      ('nelijalka '("#ffe1e1""#ffd3cb"))
		      ('tasajalka '("#ffffe1" "#fff3cb"))
		      ('murtojalka '("#e1ffe1" "#d8fec0"))
		      ('jatkojalka '("#e1ffe1" "#d8fec0")))
		    metron-si)))))))

(defun make-metron-color-func (alist)
  ""
  (lambda (name)
    (cdr (assoc name alist))))

(defun runo-stamp-incomplete (start end)
  ""
  (add-face-text-property
   start end
   '(:underline (:color "#804000" :style wave))))

(defun runo-stamp-extra (start end)
  ""
  (add-face-text-property
   start end
   '(:strike-through "#880000")))

(defvar runo-lines-per-meter 1)

(defmacro defmeter (symbol &optional short-hands &rest body)
  ""
  (cl-labels ((traverse (tree function)
		(cond ((listp tree)
		       (cons (traverse (car tree) function)
			     (when (cdr tree)
			       (traverse (cdr tree) function))))
		      (t (funcall function tree))))
	      (flat (tree); remove child nodes duplicating parent (or (or ..) ..)
		(if (listp tree)
		    (let ((flat-tree (list (car tree))))
		      (dolist (element (cdr tree))
			(setf flat-tree
			      (append flat-tree (if (and (listp element)
							 (or (eq (car flat-tree)
								 (car element))
							     (and (eq (car flat-tree)
								      'named-seq)
								  (eq (car element)
								      'seq))))
						    (cdr element)
						  (list element)))))
		      (cons (car flat-tree) ; good enough
			    (mapcar (lambda (x)
				      (flat x))
				    (cdr flat-tree))))
		  tree)))
    (let ((length (length body)))
      `(progn
	 (,(if (boundp symbol)
	       (progn (message "Redefining meter bound to %s" symbol)
		      'setf)
	     'defvar)
	  ,symbol
	  '(,@(flat (append '(seq) (traverse body
				  (lambda (x)
				    (cond ((eq x 'name) 'named-seq)
					  ((eq x 'or) 'or)
					  ((eq x 'and) 'and)
					  ((eq x 'kesuura)
					   `(regexp ,runo-kesuura))
					  ((eq x 'seq) 'seq)
					  (t
					   (let ((short-hand (assoc x short-hands)))
					     (if short-hand
						 (cadr short-hand)
					       x))))))))))
	 ',symbol))))

(defmeter runo-eeppinen-mitta
  ((dak (seq (or pitkä puolipitkä) ; metron names will also be expanded if set here
	     (or lyhyt puolipitkä)
	     (or lyhyt puolipitkä)))
   (spo (seq (or pitkä puolipitkä)
	     (or pitkä puolipitkä)))
   (tro (seq (or pitkä puolipitkä)
	     (or lyhyt puolipitkä))))
  (or (name (daktyyli 1) dak)
      (name (spondee 1) spo))
  (or (name (daktyyli 2) dak)
      (name (spondee 2) spo))
  (or (name (daktyyli 3) dak)
      (name (spondee 3) spo))
  (or (name (daktyyli 4) dak)
      (name (spondee 4) spo))
  (name (daktyyli 5) dak)
  (or (name (spondee 6) spo)
      (name (trokee 6) tro))
  kesuura)

(defmeter runo-eleginen-distikon
  ((dak (seq (or pitkä puolipitkä)
	     (or lyhyt puolipitkä)
	     (or lyhyt puolipitkä)))
   (spo (seq (or pitkä puolipitkä)
	     (or pitkä puolipitkä)))
   (tro (seq (or pitkä puolipitkä)
	     (or lyhyt puolipitkä))))
  (or (named-seq (daktyyli 1) dak)
      (named-seq (spondee 1) spo))
  (or (name (daktyyli 2) dak)
      (name (spondee 2) spo))
  (or (name (daktyyli 3) dak)
      (name (spondee 3) spo))
  (or (name (daktyyli 4) dak)
      (name (spondee 4) spo))
  (name (daktyyli 5) dak)
  (or (name (spondee 6) spo)
      (name (trokee 6) tro))
  kesuura
  (or (name (daktyyli 7) dak)
      (name (spondee 7) spo))
  (or (name (daktyyli 8) dak)
      (name (spondee 8) spo))
  (name (trokee 9) (or pitkä puolipitkä))
  kesuura
  (or (name (daktyyli 10) dak)
      (name (spondee 10) spo))
  (or (name (daktyyli 11) dak)
      (name (spondee 11) spo))
  (name (trokee 12) (or pitkä puolipitkä))
  kesuura)

(defmeter runo-kalevalamitta
   ((tavu (or pitkä puolipitkä lyhyt))
    (ensi-pitkä (or (and ensitavu pitkä)
		    (and ensitavu puolipitkä)))
    (ensi-lyhyt (and ensitavu lyhyt))
    (jatko (or (and jatkotavu pitkä)
	       (and jatkotavu puolipitkä)
	       (and jatkotavu lyhyt))))
   (or (name (nelijalka 1)
	     tavu tavu tavu tavu)
       (name (kolmijalka 1)
	     tavu tavu tavu)
       (name (kaksijalka 1)
	     tavu tavu))
   (or (name (tasajalka 2)
	     ensi-pitkä jatko)
       (name (murtojalka 2)
	     (or ensi-pitkä jatko) ensi-lyhyt)
       (name (jatkojalka 2)
	     jatko jatko))
   (or (name (tasajalka 3)
	     ensi-pitkä jatko)
       (name (murtojalka 3)
	     (or ensi-pitkä jatko) ensi-lyhyt)
       (name (jatkojalka 3)
	     jatko jatko))
   (or (name (tasajalka 4)
	     ensi-pitkä jatko)
       (name (murtojalka 4)
	     (or ensi-pitkä jatko) ensi-lyhyt)
       (name (jatkojalka 4)
	     jatko jatko))
   kesuura)

(defvar runo-mitta nil)

;;Some testing funcs:
(defun set-ed ()
  "Setup elegiac couplet."
  (setf runo-mitta (runo-compiler-dispatch runo-eleginen-distikon)
	runo-lines-per-meter 2)
  nil)
(defun set-em ()
  "Setup epic meter."
  (setf runo-mitta (runo-compiler-dispatch runo-eeppinen-mitta)
	runo-lines-per-meter 1)
  nil)

(defun set-km ()
  "Setup kalevala meter."
  (setf runo-mitta (runo-compiler-dispatch runo-kalevalamitta)
	runo-lines-per-meter 1)
  nil)

;;; The human readable meters must be compiled into a tree to allow
;; backtracking on the OR forms.
(defun runo-compiler-dispatch (form &optional subsequent)
  "Return a tree representing !!!EVERY POSSIBLE SEQUENCE!!! of meter FORM.
SUBSEQUENT used for voodoo recursion."
  (pcase form
    (`(seq . ,sequence)
     (runo-compile-sequence sequence subsequent))
    (`(or . ,options)
     (runo-compile-options options subsequent))
    (`(named-seq ,name . ,sequence)
     (let ((rest (runo-compile-sequence sequence subsequent)))
       (if (and (listp rest)
		(listp (car rest))
		(not (equal (caar rest) :and)))
	   (cl-list* :name name rest)
	 (list :name name rest))))
    (`(and . ,properties)
     (list
      (if (and (listp subsequent)
	       (listp (car subsequent))
	       ;;(listp (caar subsequent))
	       (not (equal (caar subsequent) :and))
	       (not (equal (caar subsequent) 'regexp))
	       )
	  (cons (cons :and properties) subsequent)
	(list;(cons;(list
	 (cons :and properties) subsequent))))
    (element
     (if (and (listp subsequent)
	      (listp (car subsequent))
	      ;(listp (caar subsequent))
	      (not (equal (caar subsequent) 'regexp)))
	 (cons element subsequent)
       (list element subsequent)))))

;;'(a b c d e) -> (a (b (c (d (e . nil)))))
(defun runo-compile-sequence (form old-subs)
  "Compile linear sequence based on FORM.
Don't touch OLD-SUBS."
  (let ((ret
	 (let ((subsequent (when (cdr form)
			     (runo-compile-sequence (cdr form) old-subs))))
	   (runo-compiler-dispatch (car form) (or subsequent old-subs)))))
    (when (and (listp (car ret)) (listp (caar ret)))
      (setf ret (cl-reduce 'append ret))) ;; WTF
    ret))

;;'(a b c d e) -> ((a) (b) (c) (d) (e))
(defun runo-compile-options (form subsequent)
  "Dispatch compiler on all element in FORM.
SUBSEQUENT used for voodoo recursion."
  (mapcar (lambda (x)
	    (runo-compiler-dispatch x subsequent))
	  form))

(progn (setf runo-mitta (runo-compiler-dispatch runo-eeppinen-mitta))
       nil)

(defun runo-members (elts list)
  "Return T when all ELTS found in LIST."
  (catch 'ESCAPE
    (dolist (elt elts)
      (unless (member elt list)
	(throw 'ESCAPE nil)))
    t))

(defun runo-analyze-line (line sub-meter)
  "Return list of syllable types in LINE on match with SUB-METER.
Annotated with named-sequence names and ending with :end on success,
:incomplete when missing stuff or :extra when too much stuff."
  (catch 'FOUND
    (let ((next-syllable (runo-next-syllable line))
	  (return nil))
      (cond ((and (null sub-meter)
	          (null line))
	     (throw 'FOUND (list :end)))
	    ((and sub-meter
		  (null line)) ; ran out of line before meter
	     (throw 'FOUND (list :incomplete)))
	    ((and (null sub-meter)
		  (cl-member-if 'cddr line)) ; There's something extra over here!
	     (let* ((rest-analysis (runo-analyze-line line runo-mitta))
		    (rest-result (car (last rest-analysis))))
	       (if rest-analysis
		   ;; mark OK metre end and append subsequent analysis:
		   (throw 'FOUND (cons :end rest-analysis))
		 ;; subsequent analysis fails:
		 (throw 'FOUND (list :extra))))))
      (dolist (option sub-meter)
	(let* ((found
		(pcase option
		  (`((regexp ,rx) . ,rest)
		   (when (string-match rx (caar line))
		     'rx-match))
		  (`(:name ,name . ,section)
		   (list :name name))
		  (`((:and . ,required) . ,rest)
		   ;; Let's say these props MUST for now be syllable properties and not regexps
		   (cond (next-syllable
			  (runo-members required (cddar next-syllable)))
			 (t (throw 'FOUND (list :incomplete)))))
		  (next
		   (cond (next-syllable
			  (when (member (car next) (cddar next-syllable))
			    (car next)))
			 (t (throw 'FOUND (list :incomplete))))))))
	  (when found
	    (let ((found-rest
		   (apply 'runo-analyze-line
			  ;; essentially get name:
			  (pcase (or (and (listp found) (car found))
				     ;; regexp match?
				     (and (eq found 'rx-match) 'rx-match))
			    (:name (list line (cddr option)))
			    ('rx-match
			     (list (cdr line) ; consume one line element
				   (cdr option)))
			    ;; consume line until next-syllable:
			    (- (list (cdr next-syllable)
				     (cdr option)))))))
	      (when found-rest
		(setf return (cond ((eq found 'rx-match)
				    (cons (car line) found-rest))
				   ((and (listp found) (car found))
				    (cons found found-rest))
				   (t (cons (car next-syllable) found-rest))))
		; if we're ready there's no need to search any more:
		(when (eq (car (last found-rest))
			  :end)
		  (throw 'FOUND return)))))))
      ;; On failure to acquire :end
      ;; return either :incomplete or :extra analysis
      ;; or fail analysis with nil
      return)))

(defun runo-next-syllable (line)
  "Return list starting from next alphabetical string element in LINE."
  (cl-member-if 'cddr line))

(defun runo-clear-line-point ()
  "Remove text properties from line at point."
  (interactive)
  (set-text-properties (line-beginning-position)
		       (line-end-position)
		       nil))

(defun runo-clear-area (start end)
  "Remove text properties from position START to position END."
  (set-text-properties start end nil))

(defun runo-analyze-line-point (syllabification &optional meter)
  "Return analysis on SYLLABIFICATION when it aligns with compiled METER.
If METER unsupplied use var runo-mitta."
  (runo-analyze-line syllabification (or meter runo-mitta)))

(defun runo-paint-after-input ()
  "Move back one char and paint line."
  (save-excursion
    (backward-char 1)
    (runo-paint-line)))

(defun runo-stanza-beginning-position ()
  "Position of first char in stanza under point."
  (line-beginning-position
   (- (1- (mod (1- (line-number-at-pos))
	       runo-lines-per-meter)))))

(defun runo-stanza-end-position ()
  "Position of last char in stanza under point."
  (line-end-position
   (- runo-lines-per-meter
      (mod (1- (line-number-at-pos))
	   runo-lines-per-meter))))

(defun runo-analysis-end-position (analysis)
  "Return end position of ANALYSIS element immediately preceding :end mark.
Return nil on failure."
  (cl-do ((head analysis (cdr head)))
      ((or (eq :end (cadr head))
	   (null head))
       ;; I'm pretty sure (car head) is always a syllable/regexp element
       (when head
	 (+ (line-beginning-position) ;; Untested on multilines
	    (cadadr (car head)))))))

(defun runo-forward-stanza ()
  "Move point to the beginning of next stanza."
  (interactive)
  (forward-line
   (- runo-lines-per-meter
      (mod (1- (line-number-at-pos))
	   runo-lines-per-meter))))

(defun runo-paint-line ()
  "Paint runo-lines-per-meter lines around current position based on analysis.
Will return result of meter analysis."
  (interactive)
  (let* ((position
	  (line-beginning-position
	   (1+ (- (mod (1- (line-number-at-pos))
		       runo-lines-per-meter)))))
	 (end-position
	  (min (point-max)
	       (1+ (line-end-position
		    (- runo-lines-per-meter
		       (mod (1- (line-number-at-pos))
			    runo-lines-per-meter))))))
	 (syllabification
	  (runo-syllabificate-line
	   (buffer-substring-no-properties
	    position
	    end-position)))
	 (analysis (runo-analyze-line-point syllabification)))
    (runo-clear-area position end-position)
    (cl-loop for a-elements on (or analysis syllabification)
	     with metron-name = nil
	     with metron-index = -1
	     with index = -1
	     do (pcase (car a-elements)
		  (`(:name ,metron)
		   (setf metron-name metron
			 metron-index (1+ metron-index)))
		  (`(,- ,limits . ,syllable-type)
		   (when syllable-type (setf index (1+ index)))
		   (put-text-property
		    (+ position (car limits))
		    ;; Beginning of next syllable:
		    (+ position
		       (or (caadr (car
				   (cl-member-if 'stringp (cdr a-elements)
						 :key (lambda (x)
							(and x (listp x) (car x))))))
			   (cadr limits)))
		    'face
		    (runo-syllable-color syllable-type
					 metron-name
					 index
					 metron-index)))))
    (let ((last-2 (last analysis 2)))
      (pcase (cadr last-2)
	(:extra (runo-stamp-extra (+ position (cadadr (car last-2)))
				  (runo-stanza-end-position)))
	(:incomplete (runo-stamp-incomplete
		      (or (runo-analysis-end-position analysis)
			  (runo-stanza-beginning-position))
		      (runo-stanza-end-position))))
      ;; return analysis result:
      (cadr last-2))))


;;; A bit of testing:
(defun wtf ()
  ""
  (interactive)
  (message "Point:%s - %s" (point) (text-properties-at (point))))

(defun setprop (prop)
  ""
  (interactive "xProp?")
  (set-text-properties (line-beginning-position)
		       (line-end-position)
		       prop))
(defun addprop (prop)
  ""
  (interactive "xProp?")
  (add-text-properties (line-beginning-position)
		       (line-end-position)
		       prop))

(defun runo-describe-meter (&optional meter)
  ""
  (when (null meter)
    (setf meter runo-mitta))
  (let ((count 0)
	(set (make-hash-table :test 'eq)))
    ;; TODO: get to bottom and start storing unique counts to set instead
    (cl-labels
	((node-rec (tree)
	   (when tree
	     (setf count (1+ count))
	     (puthash tree t set)
	     (pcase tree
	       ((pred null)
		(message "NULL")
		nil)
	       (`((regexp . ,rx) . ,opts)
		(dolist (opt opts)
		  (node-rec opt)))
	       (`(:name ,name . ,opts)
		(dolist (opt opts)
		  (node-rec opt)))
	       (`((:and . ,props) . ,opts) ;untested + I think AND compilation still half broken
		(dolist (opt opts)
		  (node-rec opt)))
	       (`(,root . ,opts)
		(dolist (opt opts)
		  (node-rec opt)))))))
      (node-rec meter)
      (message "Count: %s\nUnique nodes: %s"
	       count (hash-table-count set)))))

(defun runo-syllable-length (syllable)
  "Return symbol designating SYLLABLE's length."
  (cond ((string-match (rx bol
			   (0+ (regex runo-consonant))
			   (regex runo-vowel)
			   eol)
		       syllable)
	 'lyhyt)
	((string-match (rx bol
			   (0+ (regex runo-consonant))
			   (regex runo-vowel)
			   (0+ (regex runo-consonant))
			   eol)
		       syllable)
	 'puolipitkä)
	((string-match (rx bol
			   (0+ (regex runo-consonant))
			   (or (regex runo-long-vowel)
			       (regex runo-diphtong)
			       (regex runo-beginning-diphtong))
			   (0+ (regex runo-consonant))
			   eol)
		       syllable)
	 'pitkä)))

(defun runo-syllable-index (index)
  "Return a syllable's type name based on number INDEX."
  (pcase index
    (0 'ensitavu)
    (- 'jatkotavu)))

(defun runo-syllabificate-line (line)
  "Break down string LINE into list of lists of form (string (start end) &optional syllable-length)."
  (let ((split-line (split-string line (rx word-boundary) t))
	(pos 0))
    (mapcan (lambda (string)
	      (cond ((string-match "\\w" string)
		     (mapcar (lambda (syllable-element)
			       (cl-list* (car syllable-element)
					 (list pos
					       (setf pos (+ pos
							    (length (car syllable-element)))))
					 (cdr syllable-element)))
			     (runo-syllabificate string)))
		    (t (list (list string
				   (list pos
					 (setf pos (+ pos (length string)))))))))
	    split-line)))

(defvar *runo-custom-syllabification* (make-hash-table :test 'equal))

(defun runo-format-syllabification (word)
  "Produce syllabificated string of WORD where syllable length designated by -."
  ;;;(runo-format-syllabification "noutavanuolta")
  ;;;>"nou---ta-va-nu-ol--ta-"
  (apply 'concat
	 (mapcar (lambda (syllable)
		   (concat (car syllable)
			   (pcase (cadr syllable)
			     ('pitkä "---")
			     ('puolipitkä "--")
			     ('lyhyt "-"))))
		 (runo-syllabificate word))))

(defun runo-deformat-syllabification (string)
  "Produce standard syllabification from syllable-formatted STRING."
  ;;;(runo-deformat-syllabification "nou---ta-va-nuol---ta-")
  ;;;>(("nou" pitkä)
  ;;;  ("ta" lyhyt)
  ;;;  ("va" lyhyt)
  ;;;  ("nuol" pitkä)
  ;;;  ("ta" lyhyt))
  (cl-loop for (syl length) on (split-string string (rx word-boundary) t) by 'cddr
	   collecting (list syl (pcase length
				  ("---" 'pitkä)
				  ("--" 'puolipitkä)
				  ("-" 'lyhyt)))))

(defun runo-list-match-positions (regexp)
  "Return list of beginning positions in current buffer where REGEXP matched."
  (save-excursion
    (goto-char (point-min))
    (let ((list nil))
      (while (re-search-forward regexp nil t)
	(push (match-beginning 0) list))
      list)))

(defun runo-custom-syllabification (syllabification)
  "Define SYLLABIFICATION for word under point."
  (interactive
   (save-excursion
     (let ((point-word (buffer-substring-no-properties
			(progn (backward-word) (point))
			(progn (forward-word) (point)))))
       (list (read-from-minibuffer
	      (format "Syllabification for word \"%s\": " point-word)
	      (runo-format-syllabification point-word))))))
  (let* ((new-syls (runo-deformat-syllabification syllabification))
	 (syllables (mapcar 'car new-syls))
	 (word (apply 'concat syllables)))
    (runo-defsyl word
		 syllables
		 (mapcar 'cadr new-syls))
    (save-excursion
      (let ((new-results (mapcar (lambda (pos)
				   (goto-char pos)
				   (list (runo-paint-line) pos))
				 (runo-list-match-positions (concat "\\b" word "\\b")))))
	(dolist (res new-results)
	  (when (not (eq (car res) :end))
	    (message "Line at %s: analysis result is %s!" (cadr res) (car res))))))))

(defun runo-defsyl (word syllables &optional attributes)
  "Define a custom syllabification for WORD based on list SYLLABLES.
ATTRIBUTES appended onto syllable-elements."
  (puthash word
	   (cl-loop for syl in syllables
		    for at in (or attributes (make-list (length syllables) nil))
		    for index from 0
		    collecting
		    (cons syl
			  (cond ((null at)
				 (list (runo-syllable-length syl)
				       (runo-syllable-index index)))
				((listp at)
				 at)
				(t
				 (list at)))))
	   *runo-custom-syllabification*))

 ; compound word diphtong "nuo":
(runo-defsyl "kuolinnuotiot" '("kuo" "lin" "nuo" "ti" "ot"))
 ; strange syllabification:
(runo-defsyl "Peleun" '("Pel" "eun") '((puolipitkä ensitavu) (pitkä jatkotavu)))
 ; syllable length differs from written form:
(runo-defsyl "Hadeen" '("Ha" "deen") '((pitkä ensitavu) (pitkä jatkotavu)))
(runo-defsyl "Here" '("He" "re") '((pitkä ensitavu) (pitkä jatkatavu)))

(defun runo-syllabificate (word &optional syl-index)
  "Return list of finnish syllable-elements (string . attributes)
in a single WORD.  SYL-INDEX will hold index of current syllable."
  (let ((custom-syllables (gethash word *runo-custom-syllabification*)))
    (cond ((zerop (length word)) nil)
	  (custom-syllables custom-syllables)
	  (t (let* ((syl-index (or syl-index 0))
		    (core-count (runo-tavu-ydin word syl-index))
		    (end-count (runo-tavu-loppu (cl-subseq word core-count)))
		    (end (+ core-count end-count))
		    (syllable (cl-subseq word 0 end)))
	       (cons (list syllable
			   (runo-syllable-length syllable)
			   (runo-syllable-index syl-index))
		     (runo-syllabificate (cl-subseq word end) (1+ syl-index))))))))

;; http://www.kielitoimistonohjepankki.fi/ohje/153
(defun runo-tavu-ydin (word syllable-index)
  "Return the vowels and preceeding consonants of the first syllable in WORD.
SYLLABLE-INDEX should hold the index of current syllable in colloquial word."
  (cond ((string-match
	  (cond ((zerop syllable-index)
		 (rx (0+ (regex runo-consonant))
		     (or (regex runo-diphtong)
			 (regex runo-beginning-diphtong)
			 (regex runo-long-vowel)
			 (regex runo-vowel)
			 eol))) ; a word with no vowels
		(t
		 (rx (0+ (regex runo-consonant))
		     (or (regex runo-diphtong)
			 (regex runo-long-vowel)
			 (regex runo-vowel)
			 eol))))
	  word)
	 (cadr (match-data)))
	(t (error "Strange word \"%s\"" word))))

(defun runo-tavu-loppu (word)
  "Return count of consonants at end of syllable core, which has already been cut form WORD."
  (cond
   ((zerop (length word)) 0)
   ((string-match runo-vowel (cl-subseq word 0 1))
    0) ;; Vowel -> new syllable
   ((= (length word) 1)
    1) ;; Only one letter and it isn't a vowel
   ((not (string-match runo-vowel word))
    (length word)) ;; no vowels left
   ((= (length word) 2) 0)
   ((string-match (rx (regex runo-resonant-consonant)
		      (regex runo-vowel))
		  (cl-subseq word 0 3))
    1) ;; Drop consonant & vowel
   ((string-match runo-double-consonant
		  (cl-subseq word 0 2))
    1) ;; Drop duplicate consonant
   ((string-match runo-resonant-double-consonant
		  (cl-subseq word 0 3))
    2) ;; Drop duplicate consonant
   ((string-match (rx (1+ (regex runo-consonant))
		      (regex runo-vowel))
		  word)
    (- (cadr (match-data)) 2)))) ;; Drop consonant & vowel

(defun runo-syllable-analysis (string)
  "Return set of syllable lengths listed by position on each line in STRING."
  ;; Only useful for meters with always exact count of syllables
  (let* ((syls-list (mapcar
		     (lambda (x)
		       (cl-remove-if
			'null
			(mapcar 'caddr (runo-syllabificate-line x))))
		     (split-string string "\n" t)))
	 (collection (make-list (length (car syls-list)) (list))))
    (cl-loop for syls in syls-list
	     do (cl-loop for syl in syls
			 for set on collection
			 do (unless (member syl (car set))
			      (push syl (car set)))))
    collection))

(defun runo-buffer-repaint ()
  "Repaint current buffer."
  (interactive)
  (let ((start-time (time-convert (current-time) 1000)))
    (save-excursion
      (goto-char (point-min))
      (while (< (point) (point-max))
	(runo-paint-line)
        (runo-forward-stanza)))
    (let* ((time (- (car (time-convert (current-time) 1000))
		    (car start-time)))
	   (msec (mod time 1000))
	   (min (truncate time 60000))
	   (sec (- (truncate time 1000) (* min 60))))
      (message "Runo buffer painted in %s minutes, %s seconds, %s milliseconds"
	       min sec msec))))

(defun runo-mode ()
  "Yolo."
  (interactive)
  (kill-all-local-variables)
  (use-local-map runo-mode-map)
  ;;;;(set-syntax-table runo-mode-syntax-table)
  (setq major-mode 'runo-mode)
  (setq mode-name "RUNO")
  (runo-buffer-repaint)
  (make-local-variable 'post-self-insert-hook)
  (push 'runo-paint-after-input
	post-self-insert-hook)
  (run-hooks 'runo-mode-hook))

(provide 'runo-mode)
;;; runo-mode.el ends here
