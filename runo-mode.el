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
     (seq (or pitkä puolipitkä) ; 1. metronin 1. tavutyyppi
	  (or lyhyt puolipitkä) ; 1. metronin 2. tavutyyppi
	  (or lyhyt puolipitkä)) ; 1. metronin 3. tavutyyppi
     (seq (or pitkä puolipitkä) ; vaihtoehtoinen metron
	  (or pitkä puolipitkä)))
    (or
     (seq (or pitkä puolipitkä)
	  (or lyhyt puolipitkä)
	  (or lyhyt puolipitkä))
     (seq (or pitkä puolipitkä)
	  (or pitkä puolipitkä)))
    (or
     (seq (or pitkä puolipitkä)
	  (or lyhyt puolipitkä)
	  (or lyhyt puolipitkä))
     (seq (or pitkä puolipitkä)
	  (or pitkä puolipitkä)))
    (or
     (seq (or pitkä puolipitkä)
	  (or lyhyt puolipitkä)
	  (or lyhyt puolipitkä))
     (seq (or pitkä puolipitkä)
	  (or pitkä puolipitkä)))
    (seq (or pitkä puolipitkä) ; 5. metron
	 (or lyhyt puolipitkä)
	 (or lyhyt puolipitkä))
    (seq (or pitkä puolipitkä)
	 (or pitkä puolipitkä lyhyt))
    (regexp ,runo-kesuura)))

(defvar runo-mitta runo-eeppinen-mitta)

;; Should get something like this out of analysis
'((("Kuo" 3 pitkä)
   ("li" 2 lyhyt)
   ("han" 3 puolipitkä))
  (("Pat" 3 puolipitkä)
   ("rok" 3 puolipitkä))
  (("los" 3 puolipitkä)
   ("kin" 3 puolipitkä))
  (("vaikk" 5 pitkä)
   ("u" 1 lyhyt)
   ("ro" 2 lyhyt))
  (("homp" 4 puolipitkä)
   ("o" 1 lyhyt)
   ("li" 2 lyhyt))
  (("pal" 3 puolipitkä)
   ("jon" 3 puolipitkä)))

'(runo-compiler-dispatch '(seq (or f ff)
			       (or (seq (or g gg) (or i ii))
				   (seq (or j jj) (or k kk)))
			       (or h hh))
			 nil)

;; should produce:
'(:start
  (f
   (g (i (h) (hh)) (ii (h) (hh)))
   (gg (i (h) (hh)) (ii (h) (hh)))
   (j (k (h) (hh)) (kk (h) (hh)))
   (jj (k (h) (hh)) (kk (h) (hh))))
  (ff
   (g (i (h) (hh)) (ii (h) (hh)))
   (gg (i (h) (hh)) (ii (h) (hh)))
   (j (k (h) (hh)) (kk (h) (hh)))
   (jj (k (h) (hh)) (kk (h) (hh)))))

;; Only do a raw call on simple test data!
(defun runo-compiler-dispatch (form &optional subsequent)
  "Return !!!EVERY POSSIBLE SEQUENCE!!! of meter FORM.  Ignore SUBSEQUENT."
  (pcase form
    (`(seq . ,sequence)
     (runo-compile-sequence sequence subsequent))
    (`(or . ,options)
     (runo-compile-options options subsequent))
    (element
     (cons element subsequent))))

;;'(a b c d e) -> (a (b (c (d (e nil)))))
(defun runo-compile-sequence (form old-subs)
  ""
  (let ((ret
	 (let ((subsequent (when (cdr form)
			     (runo-compile-sequence (cdr form) old-subs))))
	   (runo-compiler-dispatch (car form) (or subsequent old-subs)))))
    (when (listp (caar ret))
	(setf ret (cl-reduce 'append ret))) ;; WTF
    ret))

;;'(a b c d e) -> ((a) (b) (c) (d) (e))
(defun runo-compile-options (form subsequent)
  ""
  (mapcar (lambda (x)
	    ;;(message "OR dispatches!")
	    (runo-compiler-dispatch x subsequent))
	  form))

(defun runo-paint-line (limit)
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
  "Break down string LINE into list of lists of form (string length &optional syllable-length)."
  (let ((split-line (split-string line (rx word-boundary) t)))
    (mapcan (lambda (string)
	      (cond ((string-match "\\w" string)
		     (mapcar (lambda (syllable)
			       (list syllable (length syllable)
				     (runo-syllable-length syllable)))
			     (runo-syllabificate string)))
		    (t (list (list string (length string))))))
	    split-line)))

(defun runo-syllabificate (word &optional syl-index)
  "Return list of finnish syllables a single WORD.
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
