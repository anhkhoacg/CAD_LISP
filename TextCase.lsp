;;; ------------------------------------------------------------------
;;; TextCase.lsp
;;; Change the case of selected TEXT / MTEXT / MULTILEADER objects.
;;;
;;;   TC  -> Choose UPPERCASE, lowercase, Capitalise Each Word, or Sentence case
;;;
;;; MTEXT and multileader formatting codes (\P, \fArial;, \H2.5;, \C1;,
;;; \S1/2; ...) are left untouched, so only the visible text changes.
;;; Load with APPLOAD (or drag & drop into the drawing).
;;; ------------------------------------------------------------------

(vl-load-com)

;; Convert a string. mode = 'UP, 'LOW, 'CAP or 'SENT. mt enables MTEXT codes.
(defun tc:convert (str mode mt / i n ch nxt out prev j len sentence-start)
  (setq i 1
        n (strlen str)
        out ""
        prev " "
        sentence-start T)
  (while (<= i n)
    (setq ch (substr str i 1))
    (cond
      ;; --- MTEXT format codes: copy through unchanged ---
      ((and mt (= ch "\\") (< i n))
       (setq nxt (substr str (1+ i) 1))
       (cond
         ;; codes terminated by ";"  (font, height, colour, stacking, ...)
         ((wcmatch nxt "[FfHCTQWASp]")
          (setq j (vl-string-search ";" str (1- i)))
          (setq len (if j (+ (- j i) 2) 2))
          (setq out (strcat out (substr str i len))
                i   (+ i len)))
         ;; unicode escape \U+XXXX
         ((and (= nxt "U") (= (substr str (+ i 2) 1) "+"))
          (setq out (strcat out (substr str i 7))
                i   (+ i 7)))
         ;; two-character codes: \P \~ \\ \{ \} \L \l \O \o \K \k
         (T
          (setq out (strcat out ch nxt)
                i   (+ i 2))
          (if (= nxt "P")
            (setq prev " "
                  sentence-start T)))))
      ;; --- normal character ---
      (T
       (setq out
              (strcat
                out
                (cond
                  ((= mode 'UP)  (strcase ch))
                  ((= mode 'LOW) (strcase ch T))
                  ((= mode 'SENT)
                   (if sentence-start (strcase ch) (strcase ch T)))
                  (T ; 'CAP : capitalise if not inside a word
                   (if (wcmatch prev "[A-Za-z0-9']")
                     (strcase ch T)
                     (strcase ch))))))
       (if (= mode 'SENT)
         (cond
           ((wcmatch ch "[A-Za-z0-9]") (setq sentence-start nil))
           ((wcmatch ch "[.!?]") (setq sentence-start T))))
       (setq prev ch
             i    (1+ i)))))
  out)

;; Run on a selection set
(defun tc:run (mode / ss i ent obj mt ok bad)
  (prompt "\nSelect TEXT / MTEXT / MULTILEADER objects: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,MULTILEADER"))))
    (progn
      (setq i 0 ok 0 bad 0)
      (repeat (sslength ss)
        (setq ent (ssname ss i)
              obj (vlax-ename->vla-object ent)
              mt  (member (cdr (assoc 0 (entget ent))) '("MTEXT" "MULTILEADER")))
        (if (vl-catch-all-error-p
              (vl-catch-all-apply
                '(lambda ()
                   (vla-put-textstring
                     obj
                     (tc:convert (vla-get-textstring obj) mode mt)))))
          (setq bad (1+ bad))   ; e.g. object on a locked layer
          (setq ok (1+ ok)))
        (setq i (1+ i)))
      (princ (strcat "\n" (itoa ok) " object(s) changed."))
      (if (> bad 0)
        (princ (strcat " " (itoa bad) " skipped (locked layer?).")))))
  (princ))

(defun c:TC (/ choice)
  (initget "Uppercase Lowercase Capitalize Sentence")
  (setq choice
         (getkword
           "\nText case [Uppercase/Lowercase/Capitalize/Sentence] <Uppercase>: "))
  (tc:run
    (cond
      ((or (null choice) (= choice "Uppercase")) 'UP)
      ((= choice "Lowercase") 'LOW)
      ((= choice "Sentence") 'SENT)
      (T 'CAP))))

(princ "\nTextCase loaded.  Command: TC.")
(princ)
