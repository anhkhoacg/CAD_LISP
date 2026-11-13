;;; ---------------------------------------------------------------
;;; TXT2ML.lsp  -  Combine TEXT / MTEXT objects into ONE multileader
;;;
;;; Command : TXT2ML
;;; Steps   : 1. Select TEXT and/or MTEXT objects
;;;           2. Choose whether to delete the originals
;;;           3. Pick the arrowhead point, then the landing point
;;; Result  : One multileader (current MLEADER style) whose content is
;;;           all selected text, ordered top to bottom (left to right
;;;           when on the same line), one line per object.
;;; ---------------------------------------------------------------

(defun txt2ml-gettext (ent / ed typ str)
  (setq ed  (entget ent)
        typ (cdr (assoc 0 ed)))
  (cond
    ((= typ "TEXT")
     (cdr (assoc 1 ed)))
    ((= typ "MTEXT")
     ;; MTEXT longer than 250 chars is split: group 3 chunks, then group 1
     (setq str "")
     (foreach x ed
       (if (= (car x) 3) (setq str (strcat str (cdr x)))))
     (strcat str (cdr (assoc 1 ed))))
    (t "")
  )
)

(defun c:TXT2ML ( / ss i ent ed pt lst str doc space p1 p2 pts arr obj del)
  (vl-load-com)

  (if (setq ss (ssget '((0 . "TEXT,MTEXT"))))
    (progn
      ;; Build list of (y x string) for sorting
      (setq i 0 lst nil)
      (while (< i (sslength ss))
        (setq ent (ssname ss i)
              ed  (entget ent)
              pt  (trans (cdr (assoc 10 ed)) ent 1)
              lst (cons (list (cadr pt) (car pt) (txt2ml-gettext ent)) lst)
              i   (1+ i))
      )

      ;; Top to bottom, then left to right (0.5 unit tolerance on "same line")
      (setq lst
        (vl-sort lst
          '(lambda (a b)
             (if (equal (car a) (car b) 0.5)
               (< (cadr a) (cadr b))
               (> (car a) (car b))))))

      ;; Join with MTEXT paragraph break
      (setq str (cadr (cdr (car lst))))
      (setq str (caddr (car lst)))
      (foreach item (cdr lst)
        (setq str (strcat str "\\P" (caddr item))))

      ;; Keep or delete originals
      (initget "Yes No")
      (setq del (getkword "\nDelete original text? [Yes/No] <No>: "))

      ;; Pick leader points
      (if (setq p1 (getpoint "\nSpecify arrowhead location: "))
        (if (setq p2 (getpoint p1 "\nSpecify landing location: "))
          (progn
            (setq p1 (trans p1 1 0)
                  p2 (trans p2 1 0))
            (setq doc   (vla-get-activedocument (vlax-get-acad-object))
                  space (vla-get-block (vla-get-activelayout doc)))

            (setq arr (vlax-make-safearray vlax-vbdouble '(0 . 5)))
            (vlax-safearray-fill arr
              (list (car p1) (cadr p1) (caddr p1)
                    (car p2) (cadr p2) (caddr p2)))

            (setq obj (vla-addmleader space arr 0))
            (vla-put-textstring obj str)

            (if (= del "Yes")
              (progn
                (setq i 0)
                (while (< i (sslength ss))
                  (entdel (ssname ss i))
                  (setq i (1+ i)))))

            (princ (strcat "\nMultileader created from "
                           (itoa (length lst)) " text object(s)."))
          )
        )
      )
    )
    (princ "\nNo text selected.")
  )
  (princ)
)

(princ "\nTXT2ML loaded. Type TXT2ML to start.")
(princ)
