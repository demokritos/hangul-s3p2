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

;; unassigned ones: `"' -> `㎖', `{' -> `ℓ', `}' -> `㎕'
(defconst hangul-s3p2-symbol-1-keymap
  [ ?Ⅰ ?㎖ ?Ⅲ ?Ⅳ ?Ⅴ ?Ⅶ ?㉫ ?Ⅸ ?Ⅹ ?Ⅷ ?Ⅻ ?、 ?± ?。 ?㉪
        ?§ ?µ ?㎡ ?㎥ ?￦ ?￥ ?Ω ?￡ ?€ ?￠
        ?´ ?㉥ ?≤ ?≠ ?≥ ?√ ?Ⅱ
        ?α ?β ?χ ?δ ?ε ?φ ?γ ?η ?ι ?◌ ?κ ?λ ?μ ?ν ?ο ?π ?θ ?ρ ?σ ?τ ?υ ?⋅ ?ω ?ξ ?ψ ?ζ
        ?【 ?≒ ?】 ?Ⅵ ?Ⅺ ?™
        ?◇ ?÷ ?° ?○ ?△ ?· ?… ?㉡ ?㉤ ?㉧ ?㉠ ?㉨ ?㉭ ?㉦ ?㉩ ?㉬ ?◁ ?▷ ?□ ?ː ?㉢ ?× ?▽ ?′ ?㉣ ?″
        ?ℓ ?∣ ?㎕ ?∼ ])

;; unassigned ones: `"' -> `∟' (right angle)
(defconst hangul-s3p2-symbol-2-keymap
  [ ?₁ ?∟ ?₃ ?₄ ?₅ ?₇ ?♪ ?₉ ?₀ ?₈ ?₊ ?〈 ?♂ ?〉 ?※
        ?⑩ ?① ?② ?③ ?④ ?⑤ ?⑥ ?⑦ ?⑧ ?⑨
        ?↳ ?↘ ?⊂ ?♀ ?⊃ ?⤷ ?₂
        ?∀ ?≡ ?↛ ?∆ ?∃ ?ϕ ?Γ ?☐ ?∩ ?∨ ?∧ ?↲ ?⊇ ?⊆ ?↰ ?↱ ?∈ ?¬ ?∑ ?⊤ ?∪ ?∘ ?∋ ?↮ ?⊢ ?↚
        ?〔 ?¶ ?〕 ?₆ ?₋ ?©
        ?◈ ?‰ ?☎ ?◉ ?↑ ?◦ ?― ?〃 ?℃ ?‘ ?’ ?↙ ?\」 ?\「 ?↖ ?↗ ?← ?→ ?▣ ?↔ ?㈜ ?✕ ?↓ ?☆ ?↕ ?♡
        ?₍ ?∥ ?₎ ?≈ ])

;; unassigned ones: `"' -> `∮' (contour integral)
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

;; ㅃ은 받침으로 없는데 hangul.el에는 받침으로 쓸 수 있게 정의되어
;; 있어 가져와서 고침.
;; 아래아 겹모음을 입력할 수 있게 고침.
(defconst hangul-djamo-table
  '((cho . ((1 . [1])                   ; Choseong
            (7 . [7])
            (18 . [18])
            (21 . [21])
            (24 . [24])))
    (jung . ((39 . [31 32 51])          ; Jungseong
             (44 . [35 36 51])
             (49 . [51])
             (93 . [35 44 51 93])))     ;; 아래아 , ㅓ ㅜ ㅣ ㆍ
    (jong . ((1 . [1 21])               ; Jongseong
             (4 . [24 30])
             (9 . [1 17 18 21 28 29 30])
             (18 . [-1 21])              ;; ㅃ 자리에 -1을 넣음
             (21 . [21])))))

;; 한글 호환 자모 영역(#x3130)의 자모를 첫가끝 자모 영역(#x1100)으로 변환하기
;; 위한 표. 인덱스가 한글 호환 자모 영역의 코드임. 한글 호환 자모 영역은 초성과
;; 종성의 구분이 없으므로 종성을 #x100만큼 이동시킴. 아래아 겹모음은 한글 호환
;; 자모 영역에 arae-a-i 하나만 정의되어 있는데 임으로 #x1100 영역에 정의된 네
;; 가지로 임의로 정했다. 따라서 없는 코드를 있는 것으로 생각하자.
(defconst hangul-yet-jamo-table
  (let ((table (make-char-table 'trans-yojeum-yet 0))
        (cho [ ?ㄱ ?ㄲ ?ㄴ ?ㄷ ?ㄸ ?ㄹ ?ㅁ ?ㅂ ?ㅃ ?ㅅ ?ㅆ
                   ?ㅇ ?ㅈ ?ㅉ ?ㅊ ?ㅋ ?ㅌ ?ㅍ ?ㅎ ])
        (jong [ ?ㄱ ?ㄲ ?ㄳ ?ㄴ ?ㄵ ?ㄶ ?ㄷ ?ㄹ ?ㄺ ?ㄻ ?ㄼ ?ㄽ ?ㄾ ?ㄿ ?ㅀ
                    ?ㅁ ?ㅂ ?ㅄ ?ㅅ ?ㅆ ?ㅇ ?ㅈ ?ㅊ ?ㅋ ?ㅌ ?ㅍ ?ㅎ ])
        (i 0))
    (while (< i (length cho))
      (aset table (aref cho i) (+ #x1100 i))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 21)
      (aset table (+ ?ㅏ i) (+ #x1161 i))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i 5)
      (aset table (+ ?ㆍ i) (+ #x119E i))
      (setq i (1+ i)))
    (setq i 0)
    (while (< i (length jong))
      (aset table (+ #x100 (aref jong i)) (+ #x11A8 i))
      (setq i (1+ i)))
    table))

(defsubst jamo-offset (char)
  (- char ?ㄱ -1))

(defun yet-hangul-character (cho jung jong)
  "Return a cheod-ga-ggeut combination of CHO, JUNG and JONG.
It can receive modern hangul jamo for CHO, JUNG and JONG. In that
case, it translates those jamo using `hangul-yet-jamo-table'."
  (if (> cho #x3130)
      (setq cho (aref hangul-yet-jamo-table cho)))
  (if (> jung #x3130)
      (setq jung (aref hangul-yet-jamo-table jung)))
  (if (> jong #x3130)
      (setq jong (aref hangul-yet-jamo-table (+ #x100 jong))))
  (if (and (/= cho 0) (/= jung 0))
      (if (= jong 0)
          (string cho jung)
        (string cho jung jong))
    (if (/= cho 0)
        (if (= jong 0)
            (string cho #x1160)
          (string cho #x1160 jong))
      (if (/= jung 0)
          (if (= jong 0)
              (string #x115F jung)
            (string #x115F jung jong))
        (if (/= jong 0)
            (string #x115F #x1160 jong)
          "")))))

(defun compose-hangul-character (queue)
  "Return a hangul character composed of jamo in QUEUE.
It uses the chod-ga-ggeut composition for characters containing
arae-a. Note that, actually, a chod-ga-ggeut character is
composed of multiple character codes."
  (let ((cho (+ (aref queue 0) (hangul-djamo 'cho (aref queue 0) (aref queue 1))))
        (jung (+ (aref queue 2) (hangul-djamo 'jung (aref queue 2) (aref queue 3))))
        (jong (+ (aref queue 4) (hangul-djamo 'jong (aref queue 4) (aref queue 5)))))
    (if (= (aref queue 2) (jamo-offset ?ㆍ))
        (yet-hangul-character (if (= cho 0) cho (+ #x3130 cho))
                              (if (= jung 0) jung (+ #x3130 jung))
                              (if (= jong 0) jong (+ #x3130 jong)))
      (let ((syllable (hangul-character cho jung jong)))
        (if (eq syllable "")
            ""
          (string syllable))))))

;; Redefine `hangul-insert-character' in hangul.el to enable
;; composing characters containing arae-a.
(defun hangul-insert-character (&rest queues)
  "Insert characters generated from QUEUES.
Each queue has the same form as `hangul-queue'.
Setup `quail-overlay' to the last character."
  (if (and mark-active transient-mark-mode)
      (progn
        (delete-region (region-beginning) (region-end))
        (deactivate-mark)))
  (quail-delete-region)
  (let* ((first (car queues))
         (syllables (compose-hangul-character first))
         (move (length syllables)))
    (insert syllables)
    (move-overlay quail-overlay (overlay-start quail-overlay) (point))
    (dolist (queue (cdr queues))
      (setq syllables (compose-hangul-character queue))
      (insert syllables)
      (move-overlay quail-overlay
                    (+ (overlay-start quail-overlay) move) (point))
      (setq move (length syllables)))))

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
                (hangul3-input-method-jung (jamo-offset moeum)))
            (hangul3-input-method-jong (jamo-offset jaeum))))
      ;; not vector
      (if (or (and (>= char ?ㅏ) (<= char ?ㅣ)) (= char ?ㆍ))
          (progn
            (if (zerop (aref hangul-queue 2))
                (setq hangul-gyeob-mo nil))
            (hangul3-input-method-jung (jamo-offset char)))
        (if (and (>= char ?ㄱ) (<= char ?ㅎ))
            (if (and (zerop (aref hangul-queue 2)) (zerop (aref hangul-queue 4))
                     (= (aref hangul-queue 0) (jamo-offset ?ㅇ))
                     (memq char '(?ㄱ ?ㅈ ?ㅂ)))
                (setq hangul-s3p2-symbol char)
              (if (and (notzerop (aref hangul-queue 0))
                       (zerop (aref hangul-queue 2))
                       (setq hangul-gyeob-mo (cdr (assq char '((?ㅋ . ?ㅗ) (?ㅊ . ?ㅜ)
                                                               (?ㅁ . ?ㅡ) (?ㅍ . ?ㆍ))))))
                  (hangul3-input-method-jung (jamo-offset hangul-gyeob-mo))
                (setq hangul-gyeob-mo nil)
                (hangul3-input-method-cho (jamo-offset char))))
          (setq hangul-queue (make-vector 6 0))
          (setq hangul-gyeob-mo nil)
          (insert (decode-char 'ucs char))
          (move-overlay quail-overlay (point) (point)))))))

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
    (insert (decode-char 'ucs char))
    (move-overlay quail-overlay (point) (point))))

(defun hangul-s3p2-input-method (key)
  "Shin Se-beol input method."
  (if (or buffer-read-only (< key 33) (>= key 127))
      (list key)
    (quail-setup-overlays nil)
    (let ((input-method-function nil)
          (echo-keystrokes 0)
          (help-char nil))
      (setq hangul-queue (make-vector 6 0))
      (hangul-s3p2-input-method-internal key)
      (unwind-protect
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
                           (hangul-s3p2-symbol-input-method-internal key)
                         (hangul-s3p2-input-method-internal key)))
                      ((commandp cmd)
                       (setq hangul-s3p2-symbol nil)
                       (call-interactively cmd))
                      (t
                       (setq hangul-gyeob-mo nil)
                       (setq hangul-s3p2-symbol nil)
                       (setq unread-command-events
                             (nconc (listify-key-sequence seq)
                                    unread-command-events))
                       (throw 'exit-input-loop nil))))))
        (quail-delete-overlays)))))

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
