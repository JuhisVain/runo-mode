;;; package --- Summary

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(defvar runo-mode-hook nil)

(defvar runo-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "\C-j" 'newline-and-indent) ;; example
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
	     (elt (pcase syllable-type
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

(defun runo-underline-incomplete (start end)
  ""
  (add-face-text-property
   start end
   '(:underline (:color "#804000" :style wave))))

(defun runo-underline-extra (start end)
  ""
  (add-face-text-property
   start end
   '(:strike-through "#880000")))

(defvar runo-lines-per-meter 1)

(defvar runo-eeppinen-mitta
  `(seq ; säe
    (or ; 1. metron
     (named-seq (daktyyli 1)
		(or pitkä puolipitkä) ; 1. metronin 1. tavutyyppi
		(or lyhyt puolipitkä) ; 1. metronin 2. tavutyyppi
		(or lyhyt puolipitkä)) ; 1. metronin 3. tavutyyppi
     (named-seq (spondee 1)
		(or pitkä puolipitkä) ; vaihtoehtoinen metron
		(or pitkä puolipitkä)))
    (or
     (named-seq (daktyyli 2)
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq (spondee 2)
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (or
     (named-seq (daktyyli 3)
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq (spondee 3)
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (or
     (named-seq (daktyyli 4)
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq (spondee 4)
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (named-seq (daktyyli 5)
	       (or pitkä puolipitkä) ; 5. metron
	       (or lyhyt puolipitkä)
	       (or lyhyt puolipitkä))
    (or (named-seq (spondee 6)
		   (or pitkä puolipitkä)
		   (or pitkä puolipitkä))
	(named-seq (trokee 6)
		   (or pitkä puolipitkä)
		   (or lyhyt puolipitkä)))
    (regexp ,runo-kesuura)))

(defvar runo-eleginen-distikon
  `(seq (or (named-seq (daktyyli 1)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 1)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(or (named-seq (daktyyli 2)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 2)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(or (named-seq (daktyyli 3)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 3)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(or (named-seq (daktyyli 4)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 4)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(named-seq (daktyyli 5)
		   (or pitkä puolipitkä)
		   (or lyhyt puolipitkä)
		   (or lyhyt puolipitkä))
	(or (named-seq (trokee 6)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 6)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(regexp ,runo-kesuura)
	(or (named-seq (daktyyli 7)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 7)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(or (named-seq (daktyyli 8)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 8)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(named-seq (trokee 666); todo
		   (or pitkä puolipitkä))
	(regexp ,runo-kesuura)
	(or (named-seq (daktyyli 9)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	    (named-seq (spondee 9)
		       (or pitkä puolipitkä)
		       (or pitkä puolipitkä)))
	(named-seq (daktyyli 10)
		       (or pitkä puolipitkä)
		       (or lyhyt puolipitkä)
		       (or lyhyt puolipitkä))
	(named-seq (trokee 999) ;; todo
		   (or pitkä puolipitkä))
	(regexp ,runo-kesuura)))

;;; WIP
(defvar runo-kalevalamitta ; kylläpä on nyt keksitty sääntöä
  `(seq (or (named-seq (nelijalka 1)
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt))
	    (named-seq (kolmijalka 1)
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt))
	    (named-seq (kaksijalka 1) ;; Later options preferred
		       (or pitkä puolipitkä lyhyt)
		       (or pitkä puolipitkä lyhyt)))
	(or (named-seq (tasajalka 2)
		       (or (and ensitavu pitkä) ; AND properties not evaluated!
			   (and ensitavu puolipitkä))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)))
	    (named-seq (murtojalka 2)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)
			   (and ensitavu pitkä)
			   (and ensitavu puolipitkä))
		       (and ensitavu lyhyt))
	    (named-seq (jatkojalka 2)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))))
	(or (named-seq (tasajalka 3)
		       (or (and ensitavu pitkä)
			   (and ensitavu puolipitkä))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)))
	    (named-seq (murtojalka 3)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)
			   (and ensitavu pitkä)
			   (and ensitavu puolipitkä))
		       (and ensitavu lyhyt))
	    (named-seq (jatkojalka 3)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))))
	(or (named-seq (tasajalka 4)
		       (or (and ensitavu pitkä)
			   (and ensitavu puolipitkä))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)))
	    (named-seq (murtojalka 4)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt)
			   (and ensitavu pitkä)
			   (and ensitavu puolipitkä))
		       (and ensitavu lyhyt))
	    (named-seq (jatkojalka 4)
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))
		       (or (and jatkotavu pitkä)
			   (and jatkotavu puolipitkä)
			   (and jatkotavu lyhyt))))
	(regexp ,runo-kesuura)))

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
		  (cl-member-if 'cddr line))
	     (throw 'FOUND (list :extra))))
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

(defun runo-paint-line ()
  "Paint runo-lines-per-meter lines around current point position based on meter analysis."
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
		    (runo-syllable-color (car syllable-type) ; TODO: send the whole thing
					 metron-name
					 index
					 metron-index)))))
    (let ((last-2 (last analysis 2)))
      (pcase (cadr last-2)
	(:extra (runo-underline-extra (+ position (cadadr (car last-2)))
				      (line-end-position)))
	(:incomplete (runo-underline-incomplete (line-beginning-position)
						(line-end-position)))))))


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
	 (syllables (mapcar 'car new-syls)))
    (runo-defsyl (apply 'concat syllables)
		 syllables
		 (mapcar 'cadr new-syls))))

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

(defun runo-buffer-repaint ()
  "Repaint current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (runo-paint-line)
      (forward-line))))

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
