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
(defconst runo-resonant
  "[rlmn]")

(defun runo-process-word (word)
  "Split WORD into finnish syllables."
  (let* ((first-vowel-index (string-match-p runo-vowel word))
	 (vowel-next (cl-subseq word first-vowel-index (+ 2 first-vowel-index))))
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
