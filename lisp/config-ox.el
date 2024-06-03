;;; config-ox.el -*- lexical-binding: t; -*-
(require 's)

(after! ox
  (setq org-export-with-broken-links t)  ; t/'mark/nil
  ; don't ask if treat compile-command safe if it is a string
  (put 'compile-command 'safe-local-variable #'stringp)
  ; 'mark leads to a marker in exported content with broken links
  ; nil will abort export with broken links
  ; t will continue anyway
)

(use-package! ox-extra
  :after ox
  :config
  (ox-extras-activate '(ignore-headlines)))

(defvar my/org-latex-classes-common-header-passoptions
  (s-join "\n"
    '("\\PassOptionsToPackage{dvipsnames,x11names,table}{xcolor}"
      "\\PassOptionsToPackage{colorlinks=true,linkcolor=,filecolor=Red,citecolor=Green,urlcolor=Rhodamine,pdfborder={0 0 0},breaklinks=true,linktoc=all}{hyperref}"))
  "PassOptions setting before document class, included in org-latex-classes for exporting org to latex")

(defvar my/org-latex-classes-common-header-after-default-pkgs
  (s-join "\n"
    '("% redefine quote environment - blockquote from eisvogel"
      "\\definecolor{bg}{rgb}{0.95,0.95,0.95}"
      "\\definecolor{bq-border}{RGB}{0, 63, 126}"
      "\\newmdenv[rightline=false,bottomline=false,topline=false,linewidth=3pt,backgroundcolor=bg,%"
      "           linecolor=bq-border,skipabove=\\parskip]{customblockquote}"
      "\\renewenvironment{quote}{\\begin{customblockquote}\\itshape\\list{}{\\rightmargin=6pt\\leftmargin=6pt}%"
      "\\item\\relax\\ignorespaces}{\\unskip\\unskip\\endlist\\end{customblockquote}}"
      "\\let\\Oldtextbullet\\textbullet"
      "\\renewcommand{\\textbullet}{\\textcolor{bq-border}{\\Oldtextbullet}}"
      "% compact itemize by paralist packages"
      "\\usepackage{paralist}"
      "\\let\\itemize\\compactitem"
      "\\let\\description\\compactdesc"
      "\\let\\enumerate\\compactenum"
      ))
  "Headers after default packages setting, included in org-latex-classes for exporting org to latex")

(use-package! ox-latex
  :bind
  ("C-c x l" . org-latex-export-to-latex)
  ("C-c x o" . org-latex-export-to-pdf)
  :config

  ;; use latexmk to automate toolchain
  (setq org-latex-pdf-process '("latexmk -latexoption=\"-interaction=nonstopmode -shell-escape -file-line-error -synctex=1\" -pdf -pdflatex=%latex -bibtex -f %f"))

  ;; prefer custom label
  (setq org-latex-prefer-user-labels t)
  (put 'org-latex-compiler 'safe-local-variable #'stringp)

  ;; remove default hyperset with author names included
  ;; for local variable setup, use for each file
  ;; # -*- org-latex-hyperref-template: nil; -*-

  ;; default packages to load right after documentclass at first
  (setq org-latex-default-packages-alist
    '(
      ("" "amsmath" t) ; to avoid iint and iiint error
      ("" "amssymb" t)
      ("" "wasysym" t) ; last to avoid iint and iint error
      ("AUTO" "inputenc"  t ("pdflatex"))
      ("T1"   "fontenc"   t ("pdflatex"))
      (""     "CJKutf8"   t ("pdflatex"))
      (""     "ifxetex"   nil)
      (""     "xeCJK"     nil ("xelatex", "xetex"))
      (""     "fontspec"  nil ("xelatex", "xetex", "lualatex", "luatex"))
      (""     "microtype"  nil) ; for typographic refinements
      (""     "graphicx"  t)
      (""     "xcolor"  t)
      ; ("nottoc,numbib"     "tocbibind" nil)
      ; corresponding to "setq org-latex-listings t"
      ; (""           "listings"   nil)
      ; but minted is better to use
      ("newfloat,cache=true"   "minted"   nil)
      (""     "grffile"   t)
      ; (""     "longtable" nil)
      (""     "mdframed" nil)   ; for creating blockquote
      (""     "float" nil)
      (""     "wrapfig"   nil)
      (""     "subfig"    nil)
      (""     "rotating"  nil)
      ("normalem" "ulem"  t)    ; strikeout
      (""     "textcomp"  t)
      (""     "capt-of"   nil)
      ("font={small},skip=1pt"     "caption"   nil)
      (""     "parskip"   nil)  ; better paragraph spacing
      (""     "booktabs"   nil) ; better table
	 )
  )

  ; packages to load at last
  (setq org-latex-packages-alist
    '(
      ; hyperref and cleverf should be the last packages to load
      ("" "hyperref"  nil)
      ("" "cleveref"  nil)
     )
  )

  ;; use minted as the default code block renderer
  (setq org-latex-listings 'minted
        org-latex-minted-options '(
          ("bgcolor" "bg")
          ("breaklines" "true")
          ("autogobble" "true")
          ;; ("fontsize" "\\small") ;; adjust by header \setminted{fontsize=\small}
          )
  )

  ; customized classes for latex export
  (setq org-latex-classes
               `(
                ("article"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt,a4paper]{article}"
                         "[DEFAULT-PACKAGES]"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                ("beamer"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[ignorenonframetext,presentation]{beamer}"
                         "\\usepackage{beamerseminar}"
                         "[DEFAULT-PACKAGES]"
                         "% space between caption and table"
                         "\\captionsetup[table]{belowskip=-6pt}"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}"))
                ("beamerarticle"
                  ,(s-join "\n"
                      `(
                         ,my/org-latex-classes-common-header-passoptions
                         "\\documentclass[11pt,a4paper]{article}"
                         "\\usepackage{beamerarticle}"
                         "\\usepackage{beamerseminar}"
                         "[DEFAULT-PACKAGES]"
                         "\\usepackage{physics}"
                         ,my/org-latex-classes-common-header-after-default-pkgs
                         "[EXTRA]"
                         "[PACKAGES]"))
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
                )
  )
)

(use-package! ox-beamer
  :bind
  ("C-c x b" . org-beamer-export-to-latex)
  ("C-c x O" . org-beamer-export-to-pdf)
  :config
  ; default 3rd level heading as frame.
  ; 1st and 2nd are sec and subsec
  (setq org-beamer-frame-level 3)
)

(provide 'config-ox)
;;; config-ox.el ends here
