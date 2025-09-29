(package! key-chord)

(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! svg-lib
  :recipe (:host github :repo "rougier/svg-lib"))

(unpin! pyim)
(unpin! posframe)

(unpin! org-appear)
(unpin! org-roam)

(package! ox-pandoc
      :recipe (:host github :repo "minyez/ox-pandoc"))
(unpin! ox-pandoc)

; (unpin! magit)
; (unpin! transient)

; https://github.com/countvajhula/mindstream
(package! mindstream)

(package! ox-gfm)

(package! eldoc-box)

(package! crux
  :recipe (:host github :repo "bbatsov/crux"))

(package! cm-mode
  :recipe (:host github :repo "joostkremers/criticmarkup-emacs"))

(package! org-anki
  :recipe (:host github :repo "eyeinsky/org-anki"))

(package! org-inline-pdf
  :recipe (:host github :repo "shg/org-inline-pdf.el"))

(unpin! org-download)
(package! org-download
  :recipe (:host github :repo "minyez/org-download"))

(if (modulep! :lang org +pretty)
    (package! org-fancy-priorities :disable t))

(if (modulep! :emacs dired)
    (package! dired-hacks
      :recipe (:host github :repo "minyez/dired-hacks")))

(package! consult-omni
	:recipe (:host github :repo "armindarvish/consult-omni"
           :files ("*.el" "sources/*.el")))

(package! org-recur
  :recipe (:host github :repo "mrcnski/org-recur"))

(package! org-ref
  :recipe (:host github :repo "jkitchin/org-ref"))
(package! bibtex-completion)
(package! citar)
(package! citeproc)

(package! org-bar
  :recipe (:host github :repo "tonyaldon/org-bars"))

(package! org-drill
  :recipe (:host gitlab :repo "phillord/org-drill"))

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! org-roam)
(package! citar-org-roam)
(package! org-roam-ui)

(when (modulep! :lang org +noter)
  (package! org-noter :recipe
    (:host github :repo "org-noter/org-noter"
     :files ("*.el" "modules/*.el")))
  (unpin! org-noter)
  ;; djvu/nov package required by org-noter-djvu/nov
  (package! djvu)
  (package! nov))

(package! command-log-mode)

(if IS-MAC
  (package! osx-dictionary))

(unpin! gnuplot)
(unpin! gnuplot-mode)

(package! rg)
(package! color-rg)

(package! corg
  :recipe (:host github :repo "isamert/corg.el"))

(package! easy-hugo)
(package! easy-jekyll)

(package! org-modern-indent
  :recipe (:host github :repo "jdtsmith/org-modern-indent"))

(package! eglot-booster
  :recipe (:host github :repo "jdtsmith/eglot-booster"))

(package! highlight-doxygen
  :recipe (:host github :repo "Lindydancer/highlight-doxygen"))

(package! llm-tool-collection
  :recipe (:host github :repo "skissue/llm-tool-collection"))

(package! rainbow-delimiters)
