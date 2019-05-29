;;; hangul-s3p2.el --- Korean Hangul input method, Shin Se-beol P2

;; Author: Taegil Bae <esrevinu@gmail.com>
;; Keywords: multilingual, input method, Korean, Hangul, Shin Se-beol P2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This file is to implement the following hangul automata:
;; - Hangul Shin Se-beol P2 input method

;;; Code:

(require 'hangul "quail/hangul")

;; ! " # $ % & ' ( ) * + , - . /
;; 0 1 2 3 4 5 6 7 8 9
;; : ; < = > ? @
;; A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
;; [ \ ] ^ _ `
;; a b c d e f g h i j k l m n o p q r s t u v w x y z
;; { | } ~

;; `I' and `O' are not assigned in the original Shin Se-beol P2. I assign these
;; keys to `¡' and `¿' arbitrarily.
(defconst hangul-s3p2-basic-keymap
  [ ?! ?/ ?# ?$ ?% ?& ?ㅌ ?\( ?\) ?* ?+ ?, ?- ?. ?ㅋ
       ?0 ?1 ?2 ?3 ?4 ?5 ?6 ?7 ?8 ?9
       ?: ?ㅂ ?< ?= ?> ?? ?@
       ?ㅠ ?ㅜ ?ㅔ ?ㅣ ?ㅐ ?ㅏ ?ㅡ ?□ ?¡ ?' ?\" ?· ?… ?― ?¿ ?\; ?ㅒ ?ㅓ ?ㅖ ?ㅕ ?○ ?ㅗ ?ㅑ ?ㅛ ?× ?ㆍ
       ?\[ ?\\ ?\] ?^ ?_ ?`
       [?ㅇ ?ㅠ] [?ㅊ ?ㅜ] [?ㄱ ?ㅔ] [?ㅎ ?ㅣ] [?ㅂ ?ㅐ] [?ㅍ ?ㅏ] [?ㄷ ?ㅡ] ?ㄴ ?ㅁ ?ㅇ ?ㄱ
       ?ㅈ ?ㅎ ?ㅅ ?ㅊ ?ㅍ [?ㅅ ?ㅒ] [?ㅌ ?ㅓ] [?ㄴ ?ㅖ] [?ㅋ ?ㅕ] ?ㄷ
       [?ㅈ ?ㅗ] [?ㄹ ?ㅑ] [?ㅆ ?ㅛ] ?ㄹ [?ㅁ ?ㆍ]
       ?{ ?| ?} ?~ ])

;; unassinged ones: `"' -> `㎖', `{' -> `ℓ', `}' -> `㎕'
(defconst hangul-s3p2-symbol-1-keymap
  [ ?Ⅰ ?㎖ ?Ⅲ ?Ⅳ ?Ⅴ ?Ⅶ ?㉫ ?Ⅸ ?Ⅹ ?Ⅷ ?Ⅻ ?、 ?± ?。 ?㉪
        ?§ ?µ ?㎡ ?㎥ ?￦ ?￥ ?Ω ?￡ ?€ ?￠
        ?´ ?㉥ ?≤ ?≠ ?≥ ?√ ?Ⅱ
        ?α ?β ?χ ?δ ?ε ?φ ?γ ?η ?ι ?◌ ?κ ?λ ?μ ?ν ?ο ?π ?θ ?ρ ?σ ?τ ?υ ?⋅ ?ω ?ξ ?ψ ?ζ
        ?【 ?≒ ?】 ?Ⅵ ?Ⅺ ?™
        ?◇ ?÷ ?° ?○ ?△ ?· ?… ?㉡ ?㉤ ?㉧ ?㉠ ?㉨ ?㉭ ?㉦ ?㉩ ?㉬ ?◁ ?▷ ?□ ?ː ?㉢ ?× ?▽ ?′ ?㉣ ?″
        ?ℓ ?∣ ?㎕ ?∼ ])

;; unassinged ones: `"' -> `∟' (right angle)
(defconst hangul-s3p2-symbol-2-keymap
  [ ?₁ ?∟ ?₃ ?₄ ?₅ ?₇ ?♪ ?₉ ?₀ ?₈ ?₊ ?〈 ?♂ ?〉 ?※
        ?⑩ ?① ?② ?③ ?④ ?⑤ ?⑥ ?⑦ ?⑧ ?⑨
        ?↳ ?↘ ?⊂ ?⊃ ?⤷ ?₂
        ?∀ ?≡ ?↛ ?∆ ?∃ ?ϕ ?Γ ?☐ ?∩ ?∨ ?∧ ?↲ ?⊇ ?⊆ ?↰ ?↱ ?∈ ?¬ ?∑ ?⊤ ?∪ ?∘ ?∋ ?↮ ?⊢ ?↚
        ?〔 ?¶ ?〕 ?₆ ?₋ ?©
        ?◈ ?‰ ?☎ ?◉ ?↑ ?◦ ?― ?〃 ?℃ ?‘ ?’ ?↙ ?\」 ?\「 ?↖ ?↗ ?← ?→ ?▣ ?↔ ?㈜ ?✕ ?↓ ?☆ ?↕ ?♡
        ?₍ ?∥ ?₎ ?≈ ])

;; unassinged ones: `"' -> `∮' (contour integral)
(defconst hangul-s3p2-symbol-3-keymap
  [ ?¹ ?∮ ?³ ?⁴ ?⁵ ?⁷ ?♬ ?⁹ ?⁰ ?⁸ ?⁺ ?《 ?☂ ?》 ?☠
        ?⑳ ?⑪ ?⑫ ?⑬ ?⑭ ?⑮ ?⑯ ?⑰ ?⑱ ?⑲
        ?⬎ ?⇘ ?⊄ ?☃ ?⊅ ?⤶ ?²
        ?Å ?≅ ?⊗ ?∇ ?∄ ?∅ ?∫ ?☑ ?∞ ?∵ ?∴ ?⬐ ?⊋ ?⊊ ?⬑ ?⬏ ?∉ ?∂ ?∏ ?⊥ ?∝ ?∙ ?∌ ?⊕ ?⊨ ?∠
        ?☀ ?¦ ?☁ ?⁶ ?⁻ ?®
        ?◆ ?‱ ?♨ ?● ?⇑ ?• ?￣ ?✓ ?℉ ?“ ?” ?⇙ ?\』 ?\『 ?⇖ ?⇗ ?⇐ ?⇒ ?■ ?⇔ ?㉾ ?✂ ?⇓ ?★ ?⇕ ?♥
        ?⁽ ?∦ ?⁾ ?∽ ])

(defvar hangul-gyeob-mo nil)
(defvar hangul-s3p2-symbol nil)

(defsubst jamo-offset (char)
  (- char ?ㄱ -1))

(defsubst make-hangul-character-from-queue (queue)
  (if (zerop (apply #'+ (append queue nil)))
      nil
    (hangul-character
     (+ (aref queue 0) (hangul-djamo 'cho (aref queue 0) (aref queue 1)))
     (+ (aref queue 2) (hangul-djamo 'jung (aref queue 2) (aref queue 3)))
     (+ (aref queue 4) (hangul-djamo 'jong (aref queue 4) (aref queue 5))))))

;; Support function for `hangul-s3p2-input-method'.  Actually, this
;; function handles the Hangul Shin Se-beol P2.  KEY is an entered key
;; code used for looking up `hangul-s3p2-basic-keymap'."
(defun hangul-s3p2-input-method-internal (key)
  (let ((char (aref hangul-s3p2-basic-keymap (- key 33))))
    (if (vectorp char)
        (let ((jaeum (aref char 0)) (moeum (aref char 1)))
          (if (or (and hangul-gyeob-mo
                       (notzerop (aref hangul-queue 2))
                       (zerop (aref hangul-queue 3))
                       (notzerop (hangul-djamo 'jung
                                               (aref hangul-queue 2)
                                               (jamo-offset moeum))))
                  (and (notzerop (aref hangul-queue 0))
                       (zerop (aref hangul-queue 2))))
              (progn
                (if (zerop (aref hangul-queue 2))
                    (setq hangul-gyeob-mo nil))
                (hangul3-input-method-jung (jamo-offset moeum))
                nil)
            (hangul3-input-method-jong (jamo-offset jaeum))
            nil))
      ;; not vector
      (if (or (and (>= char ?ㅏ) (<= char ?ㅣ)) (= char ?ㆍ))
          (progn
            (if (zerop (aref hangul-queue 2))
                (setq hangul-gyeob-mo nil))
            (hangul3-input-method-jung (jamo-offset char))
            nil)
        (if (and (>= char ?ㄱ) (<= char ?ㅎ))
            (if (and (zerop (aref hangul-queue 2)) (zerop (aref hangul-queue 4))
                     (= (aref hangul-queue 0) (jamo-offset ?ㅇ))
                     (memq char '(?ㄱ ?ㅈ ?ㅂ)))
                (progn (setq hangul-s3p2-symbol char)
                       nil)
              (if (and (notzerop (aref hangul-queue 0))
                       (zerop (aref hangul-queue 2))
                       (setq hangul-gyeob-mo (cdr (assq char '((?ㅋ . ?ㅗ) (?ㅊ . ?ㅜ)
                                                               (?ㅁ . ?ㅡ) (?ㅍ . ?ㆍ))))))
                  (progn
                    (hangul3-input-method-jung (jamo-offset hangul-gyeob-mo))
                    nil)
                (setq hangul-gyeob-mo nil)
                (let ((eumjeol (make-hangul-character-from-queue hangul-queue)))
                  (if (and eumjeol
                           (zerop (hangul-djamo 'cho
                                                (aref hangul-queue 0)
                                                (jamo-offset char))))
                      (progn
                        (quail-delete-region)
                        (setq hangul-queue (make-vector 6 0))
                        (setq unread-input-method-events
                              (cons (list key) unread-input-method-events))
                        (list eumjeol))
                    (hangul3-input-method-cho (jamo-offset char))
                    nil))))
          (let ((eumjeol (make-hangul-character-from-queue hangul-queue)))
            (setq hangul-queue (make-vector 6 0))
            (setq hangul-gyeob-mo nil)
            (quail-delete-region)
            (move-overlay quail-overlay (point) (point))
            (if eumjeol
                (list eumjeol char)
              (list char))))))))

(defun hangul-s3p2-symbol-input-method-internal (key)
  (let (char)
    (cond ((= hangul-s3p2-symbol ?ㄱ)
           (setq char (aref hangul-s3p2-symbol-1-keymap (- key 33))))
          ((= hangul-s3p2-symbol ?ㅈ)
           (setq char (aref hangul-s3p2-symbol-2-keymap (- key 33))))
          ((= hangul-s3p2-symbol ?ㅂ)
           (setq char (aref hangul-s3p2-symbol-3-keymap (- key 33)))))
    (setq hangul-s3p2-symbol nil)
    (setq hangul-queue (make-vector 6 0))
    (quail-delete-region)
    ;(insert (decode-char 'ucs char))
    (move-overlay quail-overlay (point) (point))
    (list char)))

(defun hangul-s3p2-input-method (key)
  "Shin Se-beol input method."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (if (and (zerop (aref hangul-queue 0)) (zerop (aref hangul-queue 2)))
        (quail-setup-overlays nil))
    (let ((input-method-function nil)
          (echo-keystrokes 0)
          (help-char nil))
      (or (hangul-s3p2-input-method-internal key)
          (catch 'exit-input-loop
            (while t
              (let* ((seq (read-key-sequence nil))
                     (cmd (lookup-key hangul-im-keymap seq))
                     key)
                (cond ((and (stringp seq)
                            (= 1 (length seq))
                            (setq key (aref seq 0))
                            (and (>= key 33) (< key 127)))
                       (if hangul-s3p2-symbol
                           (throw 'exit-input-loop
                                  (hangul-s3p2-symbol-input-method-internal
                                   key)))
                       (let ((eumjeols (hangul-s3p2-input-method-internal key)))
                         (if eumjeols
                             (throw 'exit-input-loop eumjeols))))
                      ((commandp cmd)
                       (setq hangul-s3p2-symbol nil)
                       (call-interactively cmd))
                      (t
                       (setq hangul-s3p2-symbol nil)
                       (setq unread-command-events
                             (nconc (listify-key-sequence seq)
                                    unread-command-events))
                       (let ((eumjeol (make-hangul-character-from-queue
                                       hangul-queue)))
                         (setq hangul-queue (make-vector 6 0))
                         (quail-delete-region)
                         (throw 'exit-input-loop
                                (and eumjeol (list eumjeol)))))))))))))

;; From old hangul.el
(defsubst symbol+ (&rest syms)
  "Concatenate symbols"
  (intern (mapconcat 'symbol-name syms "")))

;; From old hangul.el
(defmacro hangul-register-input-method (package-name language input-method-func
                                                     package-title package-description
                                                     package-help)
  "Define input method activate function, inactivate function, help function,
and register input method"
  (let ((activate-func (symbol+ input-method-func '-activate))
        (inactivate-func (symbol+ input-method-func '-inactivate))
        (help-func (symbol+ input-method-func '-help)))
    `(progn
       (defun ,activate-func (&optional arg)
         (if (and arg
                  (< (prefix-numeric-value arg) 0))
             (unwind-protect
                 (progn
                   (quail-hide-guidance)
                   (quail-delete-overlays)
                   (setq describe-current-input-method-function nil))
               (kill-local-variable 'input-method-function))
           (setq inactivate-current-input-method-function ',inactivate-func)
           (setq describe-current-input-method-function ',help-func)
           (quail-delete-overlays)
           (if (eq (selected-window) (minibuffer-window))
               (add-hook 'minibuffer-exit-hook 'quail-exit-from-minibuffer))
           (set (make-local-variable 'input-method-function)
                ',input-method-func)))
       (defun ,inactivate-func ()
         (interactive)
         (,activate-func -1))
       (defun ,help-func ()
         (interactive)
         (with-output-to-temp-buffer "*Help*"
           (princ ,package-help)))
       (register-input-method ,package-name
                              ,language
                              ',activate-func
                              ,package-title
                              ,package-description))))

(hangul-register-input-method
 "korean-hangul-s3p2"
 "UTF-8"
 hangul-s3p2-input-method
 "한S3P2"
 "Hangul Shin Se-beol P2 Input Method"
 "Input method: korean-hangul-s3p2 (mode line indicator:한S3P2)\n\nHangul Shin Se-beol P2 input method.")

(provide 'hangul-s3p2)

;;; hangul-s3p2.el ends here
