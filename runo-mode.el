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

(defun runo-syllable-color (syllable-type metron-type index)
  ""
  (if (null syllable-type) ; paint regexp matches blue
      (list :background "#e1e1ff")
    (let ((metron-name (when metron-type
			 (cl-subseq (symbol-name metron-type) 0 3)))) ; dak spo tro
      (append
       (list :foreground
	     (pcase syllable-type
	       ('pitkä "#000000")
	       ('puolipitkä "#3b3b3b")
	       ('lyhyt "#777777")))
       (when metron-type ; if metron analysis was ok
	 (list :background
	       (pcase metron-name
		 ("dak" "#ffe1e1")
		 ("spo" "#ffffe1")
		 ("tro" "#e1ffe1"))))))))

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

(defvar runo-eeppinen-mitta
  `(seq ; säe
    (or ; 1. metron
     (named-seq daktyyli-1
		(or pitkä puolipitkä) ; 1. metronin 1. tavutyyppi
		(or lyhyt puolipitkä) ; 1. metronin 2. tavutyyppi
		(or lyhyt puolipitkä)) ; 1. metronin 3. tavutyyppi
     (named-seq spondee-1
		(or pitkä puolipitkä) ; vaihtoehtoinen metron
		(or pitkä puolipitkä)))
    (or
     (named-seq daktyyli-2
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq spondee-2
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (or
     (named-seq daktyyli-3
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq spondee-3
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (or
     (named-seq daktyyli-4
		(or pitkä puolipitkä)
		(or lyhyt puolipitkä)
		(or lyhyt puolipitkä))
     (named-seq spondee-4
		(or pitkä puolipitkä)
		(or pitkä puolipitkä)))
    (named-seq daktyyli-5
	       (or pitkä puolipitkä) ; 5. metron
	       (or lyhyt puolipitkä)
	       (or lyhyt puolipitkä))
    (or (named-seq spondee-6
		   (or pitkä puolipitkä)
		   (or pitkä puolipitkä))
	(named-seq trokee-6
		   (or pitkä puolipitkä)
		   (or lyhyt puolipitkä)))
    (regexp ,runo-kesuura)))

(defvar runo-mitta nil)

(defun runo-compiler-dispatch (form &optional subsequent)
  "Return a tree representing !!!EVERY POSSIBLE SEQUENCE!!! of meter FORM.
SUBSEQUENT used for voodoo recursion."
  (pcase form
    (`(seq . ,sequence)
     (runo-compile-sequence sequence subsequent))
    (`(or . ,options)
     (runo-compile-options options subsequent))
    (`(regexp ,rx)
     (list 'regexp rx))
    (`(named-seq ,name . ,sequence)
     (let ((rest (runo-compile-sequence sequence subsequent)))
       (if (and (listp rest) (listp (car rest)))
	   (cl-list* :name name rest)
	 (list :name name rest))))
    (element ; mamma mia
     (if (and (listp subsequent)
	      (listp (car subsequent)))
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

(defun runo-analyze-line (line sub-meter)
  "Return list of syllable types in LINE on match with SUB-METER.
Annotated with named-sequence names and ending with :end on success,
:incomplete when missing stuff or :extra when too much stuff."
  ;;TODO: Will still return NIL if completely unacceptable!!
  (catch 'FOUND
    (cond ((and (null sub-meter)
		(null (cl-member-if 'cddr line)))
	   (throw 'FOUND (list :end)))
	  ((and sub-meter
		(not (equal (caar sub-meter)
			    'regexp))
		(null (cl-member-if 'cddr line)))
	   (throw 'FOUND (list :incomplete)))
	  ((and (null sub-meter)
		(cl-member-if 'cddr line))
	   (throw 'FOUND (list :extra))))
    (let ((next-syllable (runo-next-syllable line)))
      (dolist (option sub-meter)
	(let* ((found
		(pcase option
		  (`(regexp ,rx)
		   (when (string-match rx (caar line))
		     'rx-match))
		  (`(:name ,name . ,section)
		   (list :name name))
		  (next
		   (when (eq (car next)
			     (caddar next-syllable))
		     (car next))))))
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
				   (cdr (member option sub-meter))))
			    (- (list (cdr next-syllable) ; consume line until next-syllable
				     (cdr option)))))))
	      (when found-rest
		(throw 'FOUND
		       ;; if regexp matches, append matched string
		       (cond ((eq found 'rx-match)
			      (cons (car line) found-rest))
			     ((and (listp found) (car found))
			      (cons found found-rest))
			     (t (cons (car next-syllable) found-rest))))))))))))

(defun runo-next-syllable (line)
  "Return list starting from next alphabetical string element in LINE."
  (cl-member-if 'cddr line))

(defun runo-clear-line ()
  "Remove text properties from line at point."
  (interactive)
  (set-text-properties (line-beginning-position)
		       (line-end-position)
		       nil))

(defun runo-analyze-line-point (syllabification &optional meter)
  "Return analysis on SYLLABIFICATION when it aligns with compiled METER.
If METER unsupplied use var runo-mitta."
  (runo-analyze-line syllabification (or meter runo-mitta)))

(defun runo-paint-line ()
  ""
  (interactive)
  (runo-clear-line)
  (let* ((syllabification (runo-syllabificate-line
			   (buffer-substring-no-properties (line-beginning-position)
							   (1+ (line-end-position)))))
	 (analysis (runo-analyze-line-point syllabification))
	 (position (line-beginning-position)))
    (cond (analysis
	   (cl-loop for a-element in analysis
		    with metron-name = nil
		    with index = -1
		    do (pcase a-element
			 (`(:name ,metron)
			  (setf metron-name metron))
			 (`(,- ,limits . ,syllable-type)
			  (put-text-property
			   (+ position (car limits))
			   (+ position (cadr limits))
			   'face
			   (runo-syllable-color (car syllable-type) metron-name
						(setf index (1+ index)))))
			 )))
	  (t ; analysis fail
	   (cl-loop for element in syllabification
		    with index = -1
		    do (pcase element
			 (`(,- ,limits . ,syllable-type)
			  (put-text-property
			   (+ position (car limits))
			   (+ position (cadr limits))
			   'face
			   (runo-syllable-color (car syllable-type) nil
						(setf index (1+ index)))))))))
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
  (message "%s" (text-properties-at (point))))

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
	   (cl-mapcar (lambda (syl at)
			(cons syl
			      (cond ((null at)
				     (list (runo-syllable-length syl)))
				    ((listp at)
				     at)
				    (t
				     (list at)))))
		      syllables
		      (or attributes
			  (make-list (length syllables) nil)))
	   *runo-custom-syllabification*))

(runo-defsyl "kuolinnuotiot" '("kuo" "lin" "nuo" "ti" "ot")) ; compound word diphtong "nuo"
(runo-defsyl "Peleun" '("Pel" "eun")) ; strange syllabification
(runo-defsyl "Hadeen" '("Ha" "deen") '(pitkä pitkä)) ; syllable length differs from written form
(runo-defsyl "Here" '("He" "re") '(pitkä pitkä))

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
	       (cons (list syllable (runo-syllable-length syllable))
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
    (- (cadr (match-data)) 2)) ;; Drop consonant & vowel
   ))

(defun test ()
  "Test syllabification."
  (let ((poetry "
\"Vait ole, mieletön, lunnaistas älä haastele mulle.
Koittamaton kun viel' oli Patroklon tuhopäivä,
heltyen helpommin minä iliolaisia säästin,
vangiten mont' elävältä ja kauas kaupata antain;
nytp' ei kuoloa ainoakaan ole karttava, jonka
tuo kedol' Ilionin joku kuoloton kätteni valtaan
sen asujoist', ei semminkään Priamon oma poika.
Kuollos, ystävä, siis sinä myös! Älä ruikuta suotta!
Kuolihan Patrokloskin, vaikk' urohomp' oli paljon.
Katso'os, kuink' olen itse mä sankari sorja ja uljas,
korkea mulla on taatto ja kuoloton kantaja kallis;
vaan mua vartoelee toki kuolo ja mahtava Moira.
Aamu se joutuva on tai ehtoo tai sydänpäivä,
jolloin hengen vie joku multakin kamppaelussa
iskien peitsellään tai jänteelt' ampuen nuolen.\"")
	(syllables nil))
    (dolist (word (split-string poetry "\\W" t))
      (push (runo-syllabificate word) syllables))
    (reverse syllables)))

(defun runo-mode ()
  "Yolo."
  (interactive)
  (kill-all-local-variables)
  (use-local-map runo-mode-map)
  ;;;;(set-syntax-table runo-mode-syntax-table)
  (setq major-mode 'runo-mode)
  (setq mode-name "RUNO")
  (run-hooks 'runo-mode-hook))


(provide 'runo-mode)
;;; runo-mode.el ends here
