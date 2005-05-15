;; arch-tag: 2f3ac941-db83-41db-beb2-92476e483697

(define (french-postfix-initialize)
  (kbd-map
    (:mode in-french?)
    ("a `" "�") ("A `" "�") ("a ^" "�") ("A ^" "�")
    ("e '" "�") ("E '" "�") ("e `" "�") ("E `" "�")
    ("e ^" "�") ("E ^" "�") ("e \"" "�") ("E \"" "�")
    ("e ' '" "e'") ("E ' '" "E'") ("e \" \"" "e\"") ("E \" \"" "E\"")
    ("i ^" "�") ("I ^" "�") ("i \"" "�") ("I \"" "�")
    ("i \" \"" "i\"") ("I \" \"" "I\"")
    ("o ^" "�") ("O ^" "�")
    ("u `" "�") ("U `" "�") ("u ^" "�") ("U ^" "�")
    ("c ," "�") ("C ," "�") ("c , ," "c,") ("C , ," "C,")
    ("a e" "�") ("A e" "�") ("A E" "�")
    ("a e e" "ae") ("A e e" "Ae") ("A E E" "AE")
    ("o e" "�") ("O e" "�") ("O E" "�")
    ("o e e" "oe") ("O e e" "Oe") ("O E E" "OE")))

(plugin-configure french-postfix
  (:require #t)
  (:initialize (french-postfix-initialize)))
