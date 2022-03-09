;;; package --- Summary

;;; Commentary:
;;

;;; Code:
(require 'cl-lib)
(defvar runo-mode-hook nil)

(defvar runo-mode-map
  (let ((map (make-keymap)))
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
  "\\(?:[,.:; ]\\)\\|\\(?:\\W*$\\)")

(defun runo-syllable-color (syllable-type index)
  ""
  (list :background
	(cl-case syllable-type
	  (pitkä (nth (mod index 2) '("#ff8e8a" "#ffbfbd")))
	  (puolipitkä (nth (mod index 2) '("#f1b476" "#f5cda4")))
	  (lyhyt (nth (mod index 2) '("#f4eb86" "#f8f3b5"))))))




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

(setf runo-mitta (runo-compiler-dispatch runo-eeppinen-mitta))

(defun runo-analyze-line (line sub-meter)
  "Return list of syllable types in LINE on match with SUB-METER."
  (catch 'FOUND
    (when (and (null sub-meter)
	       (null (cl-member-if 'cddr line)))
      (throw 'FOUND (list :end)))
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
			  (pcase (or (and (listp found) (car found)) ; essentially get name
				     (and (eq found 'rx-match) 'rx-match)) ; regexp match?
			    (:name (list line (cddr option)))
			    ('rx-match
			     (list (cdr next-syllable)
				   (cdr (member option sub-meter))))
			    (- (list (cdr next-syllable) (cdr option)))))))
	      (when found-rest
		(throw 'FOUND
		       (cond ((eq found 'rx-match) ; if regexp matches, append matched string
			      (cons (car line) found-rest))
			     ((and (listp found) (car found))
			      (cons found found-rest))
			     (t (cons (car next-syllable) found-rest))))))))))))

;;; An example:
'(runo-analyze-line (runo-syllabificate-line "Paskaa, Saatana! Ei jumalauta nyt taas vittu Perkel'!\n")
		    (runo-compiler-dispatch runo-eeppinen-mitta))
;; --> Pretty good
'((:name spondee-1)
  ("Pas" 3 puolipitkä)
  ("kaa" 3 pitkä)
  (:name daktyyli-2)
  ("Saa" 3 pitkä)
  ("ta" 2 lyhyt)
  ("na" 2 lyhyt)
  (:name daktyyli-3)
  ("Ei" 2 pitkä)
  ("ju" 2 lyhyt)
  ("ma" 2 lyhyt)
  (:name daktyyli-4)
  ("lau" 3 pitkä)
  ("ta" 2 lyhyt)
  ("nyt" 3 puolipitkä)
  (:name daktyyli-5)
  ("taas" 4 pitkä)
  ("vit" 3 puolipitkä)
  ("tu" 2 lyhyt)
  (:name spondee-6)
  ("Per" 3 puolipitkä)
  ("kel" 3 puolipitkä)
  ("'!\n" 2)
  :end)

(defun runo-meter-count (compiled-meter)
  "Count primary elements in COMPILED-METER tree."
  (if compiled-meter
      (1+ (apply '+ (mapcar 'runo-meter-count (cdr compiled-meter))))
    0))

(defun runo-next-syllable (line)
  "Return list starting from next alphabetical string element in LINE."
  (cl-member-if 'cddr line))

(defun runo-paint-line (limit)
(defun runo-clear-line ()
  "Remove text properties from line at point."
  (interactive)
  (set-text-properties (line-beginning-position)
		       (line-end-position)
		       nil))

  ""
  (interactive "nLimit?") ;; testing
  (let* ((point (point))
	 (line (progn (re-search-forward (rx bol (regex ".*") eol) limit 'GOTO-END)
		      (match-string 0)))
	 (elements (runo-syllabificate-line line))
	 (syllable-index 0))
    (dolist (e elements)
      (let ((syllable-length (caddr e)))
	(when syllable-length
	  (put-text-property point
			     (+ point (cadr e))
			     'font-lock-face
			     ;;(cl-case syllable-length
			       ;;(pitkä '(:background "red"))
			       ;;(puolipitkä '(:background "yellow"))
			       ;;(lyhyt '(:background "green")))
			     (runo-syllable-color syllable-length syllable-index)
			     )
	  (setf syllable-index (1+ syllable-index)))
	(setf point (+ point (cadr e)))))))

(defun runo-syllabificate-line (line)
  "Break down string LINE into list of lists of form (string (start end) &optional syllable-length)."
  (let ((split-line (split-string line (rx word-boundary) t))
	(pos 0))
    (mapcan (lambda (string)
	      (cond ((string-match "\\w" string)
		     (mapcar (lambda (syllable)
			       (list syllable
				     (list pos
					   (setf pos (+ pos (length syllable))))
				     (runo-syllable-length syllable)))
			     (runo-syllabificate string)))
		    (t (list (list string (length string))))))
	    split-line)))

(defun runo-syllabificate (word &optional syl-index)
  "Return list of finnish syllables in a single WORD.
SYL-INDEX will hold index of current syllable."
  (unless (zerop (length word))
    (let* ((syl-index (or syl-index 0))
	   (core-count (runo-tavu-ydin word syl-index))
	   (end-count (runo-tavu-loppu (cl-subseq word core-count)))
	   (end (+ core-count end-count)))
      (cons (cl-subseq word 0 end)
	    (runo-syllabificate (cl-subseq word end) (1+ syl-index))))))

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
			 (regex runo-vowel))))
		(t
		 (rx (0+ (regex runo-consonant))
		     (or (regex runo-diphtong)
			 (regex runo-long-vowel)
			 (regex runo-vowel)))))
	  word)
	 (cadr (match-data)))
	(t (error "No syllable core vowel found in \"%s\"" word))))

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
