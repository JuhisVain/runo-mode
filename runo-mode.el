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
(defconst runo-not-vowel
  "[^aeiouyäö]")
(defconst runo-long-vowel
  (concat "\\(?1:" runo-vowel "\\)\\1"))
(defconst runo-consonant
  "[bcdfghjklmnpqrstvwxz]")
(defconst runo-not-double-consonant
  "\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1\\{0\\}")
(defconst runo-double-consonant
  "\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1")
(defconst runo-resonant
  "\\(?1:[rlmn]\\)\\1\\{0\\}")

(defconst runo-resonant-consonant
  "[rlmn][bcdfghjkpqstvwxz]")
(defconst runo-resonant-double-consonant
  "[rlmn]\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1")

(defconst runo-not-new-syllable
  "[bcdfghjklmnpqrstvwxz]*\\(?:[bcdfghjklmnpqrstvwxz][aeiouyäö]\\)\\{0\\}")

(defconst runo-voodoo
  "\\(?:\\(?:[bcdfghjklmnpqrstvwxz][aeiouyäö]\\)\\{0\\}\\)\\|\\(?:\\(?1:[bcdfghjklmnpqrstvwxz]\\)\\1\\{0\\}\\)")

(defun runo-process-word (word)
  "Split WORD into finnish syllables."
  (let* ((first-vowel-index (string-match-p runo-vowel word))
	 (vowel-next (cl-subseq word first-vowel-index (+ 2 first-vowel-index))))
    ;; Vowel length in first syllable:
    (cond ((string-match runo-diphtong vowel-next)
	   'diphtongi)
	  ((string-match runo-long-vowel vowel-next)
	   'pitkä)
	  (t
	   'lyhyt)
	  )
    ))

;;(runo-process-word "Laulaos")

(provide 'runo-mode)
;;; runo-mode.el ends here
