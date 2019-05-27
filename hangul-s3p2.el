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

(require 'hangul)

;; 공세벌식 3-91
;; ㄲ · ㅈ ㄿ ㄾ “ ㅌ ' ~ ” + , ) . ㅗ
;; ㅋ ㅎ ㅆ ㅂ ㅛ ㅠ ㅑ ㅖ ㅢ ㅜ
;; 4 ㅂ , > . ! ㄺ
;; ㄷ ? ㅋ ㄼ ㄵ ㄻ ㅒ 0 7 1 2 3 " - 8 9 ㅍ ㅀ ㄶ ㄽ 6 ㄳ ㅌ ㅄ 5 ㅊ
;; ( : < = ; *
;; ㅇ ㅜ ㅔ ㅣ ㅕ ㅏ ㅡ ㄴ ㅁ ㅇ ㄱ ㅈ ㅎ ㅅ ㅊ ㅍ ㅅ ㅐ ㄴ ㅓ ㄷ ㅗ ㄹ ㄱ ㄹ ㅁ
;; % \ / ※

;; 공세벌식 3-90
;; ㅈ " # $ % & ㅌ ( ) * + , - . ㅗ
;; ㅋ ㅎ ㅆ ㅂ ㅛ ㅠ ㅑ ㅖ ㅢ ㅜ
;; : ㅂ 2 = 3 ? @
;; ㄷ ! ㄻ ㄺ ㅋ ㄲ / ' 8 4 5 6 1 0 9 > ㅍ ㅒ ㄶ ; 7 ㅀ ㅌ ㅄ < ㅊ
;; [ \ ] ^ _ `
;; ㅇ ㅜ ㅔ ㅣ ㅕ ㅏ ㅡ ㄴ ㅁ ㅇ ㄱ ㅈ ㅎ ㅅ ㅊ ㅍ ㅅ ㅐ ㄴ ㅓ ㄷ ㅗ ㄹ ㄱ ㄹ ㅁ
;; { | } ~

;; ! " # $ % & ' ( ) * + , - . /
;; 0 1 2 3 4 5 6 7 8 9
;; : ; < = > ? @
;; A B C D E F G H I J K L M N O P Q R S T U V W X Y Z
;; [ \ ] ^ _ `
;; a b c d e f g h i j k l m n o p q r s t u v w x y z
;; { | } ~

;; 신세벌식 P2
;; `I' and `O' are not assigned in the original Shin Se-beol P2. I assign this
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

(defvar hangul-gyeob-mo nil)

(defsubst jamo-offset (char)
  (- char ?ㄱ -1))

;; Support function for `hangul-s3p2-input-method'.  Actually, this
;; function handles the Hangul Shin Se-beol P2.  KEY is an entered key
;; code used for looking up `hangul-s3p2-basic-keymap'."
(defun hangul-s3p2-input-method-internal (key)
  (let ((char (aref hangul-s3p2-basic-keymap (- key 33))))
    (if (vectorp char)
        (let ((jaeum (aref char 0)) (moeum (aref char 1)))
          (if (or (and hangul-gyeob-mo
                       (notzerop (aref hangul-queue 2))
                       (notzerop (hangul-djamo 'jung
                                               (aref hangul-queue 2)
                                               (jamo-offset moeum))))
                  (zerop (aref hangul-queue 2)))
              (progn
                (setq hangul-gyeob-mo nil)
                (hangul3-input-method-jung (jamo-offset moeum)))
            (hangul3-input-method-jong (jamo-offset jaeum))))
      ;; not vector
      (if (and (>= char ?ㅏ) (<= char ?ㅣ))
          (hangul3-input-method-jung (jamo-offset char))
        ;; (if (and (zerop (aref hangul-queue 2)) (zerop (aref hangul-queue 4))
        ;;       (eq (aref hangul-queue 0) ?ㅇ) (not (vectorp char))
        ;;       (memq char '(?ㄱ ?ㅈ ?ㅂ)))
        ;;  ;; TODO not implemented
        ;;  )
        (if (and (>= char ?ㄱ) (<= char ?ㅎ))
            (if (and (notzerop (aref hangul-queue 0))
                     (setq hangul-gyeob-mo (cdr (assq char '((?ㅋ . ?ㅗ) (?ㅊ . ?ㅜ)
                                                             (?ㅁ . ?ㅡ) (?ㅍ . ?ㆍ))))))
                (hangul3-input-method-jung (jamo-offset hangul-gyeob-mo))
              (hangul3-input-method-cho (jamo-offset char)))
           (setq hangul-queue (make-vector 6 0))
           (insert (decode-char 'ucs char))
           (move-overlay quail-overlay (point) (point)))))))

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
                       (hangul-s3p2-input-method-internal key))
                      ((commandp cmd)
                       (call-interactively cmd))
                      (t
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
