(TeX-add-style-hook
 "pips_report_david_veenstra"
 (lambda ()
   (TeX-add-to-alist 'LaTeX-provided-class-options
                     '(("article" "a4paper" "12pt")))
   (TeX-add-to-alist 'LaTeX-provided-package-options
                     '(("babel" "english")))
   (TeX-run-style-hooks
    "latex2e"
    "article"
    "art12"
    "graphicx"
    "babel"
    "fancyhdr"
    "lastpage"
    "xifthen"
    "DejaVuSansMono"
    "microtype"
    "mathtools"
    "textcomp")
   (TeX-add-symbols
    '("norm" 1)
    '("reg" 1)
    '("barg" 1)
    "titel"
    "opdracht"
    "docent"
    "cursus"
    "datum"
    "studentA"
    "uvanetidA")
   (LaTeX-add-labels
    "instruction-table"
    "pips-schematic"
    "accuracy"))
 :latex)

