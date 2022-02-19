;;; package --- Summary

;;; Commentary:
;;

;;; Code:
(defvar runo-mode-hook nil)

(defvar runo-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent) ;; example
    map)
  "Keymap for runo-mode.")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.runo\\'" . runo-mode))

(defconst runo-diphtong
  (rx (or "ai" "ei" "oi" "ui" "yi" "äi" "öi" "au" "eu" "iu" "ou" "äy" "öy" "ey" "iy" "uo" "yö" "ie")))

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

    ))

(defun runo-syllabificate-line (line)
  ""
  (let* ((split-line (split-string line (rx word-boundary) t))
	 (syllables (list nil))
	 (tail syllables))
    (dolist (string split-line)
      (cond ((string-match "\\w" string)
	     (dolist (syllable (runo-syllabificate string))
	       (setf (car tail) (list syllable
				      (length syllable)
				      (runo-syllable-length syllable))
		     (cdr tail) (list nil)
		     tail (cdr tail))))
	    (t (setf (car tail) (list string (length string))
		     (cdr tail) (list nil)
		     tail (cdr tail)))))
    syllables))

(defun runo-syllabificate (word)
  "Return list of finnish syllables a single WORD."
  (unless (zerop (length word))
    (let* ((core-count (runo-tavu-ydin word))
	   (end-count (runo-tavu-loppu (cl-subseq word core-count)))
	   (end (+ core-count end-count)))
      (cons (cl-subseq word 0 end)
	    (runo-syllabificate (cl-subseq word end))))))

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
			       (regex runo-diphtong))
			   (0+ (regex runo-consonant))
			   eol)
		       syllable)
	 'pitkä)))

(defun runo-tavu-ydin (word)
  "Return the core vowels and preceeding consonants of the first syllable in WORD."
  (cond ((string-match
	  (rx (0+ (regex runo-consonant))
	      (or (regex runo-diphtong)
		  (regex runo-long-vowel)
		  (regex runo-vowel)))
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


(provide 'runo-mode)
;;; runo-mode.el ends here
