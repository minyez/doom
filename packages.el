(package! key-chord)

(package! svg-tag-mode
  :recipe (:host github :repo "rougier/svg-tag-mode"))
(package! svg-lib
  :recipe (:host github :repo "rougier/svg-lib"))

(unpin! pyim)
(unpin! posframe)

(unpin! org-appear)

(if (modulep! :lang org +pretty)
    (package! org-fancy-priorities :disable t))

(if (modulep! :emacs dired)
    (package! dired-hacks
      :recipe (:host github :repo "minyez/dired-hacks")))

(package! org-recur
  :recipe (:host github :repo "mrcnski/org-recur"))

(package! org-ref
  :recipe (:host github :repo "jkitchin/org-ref"))
(package! bibtex-completion)
(package! citar)

(package! org-bar
  :recipe (:host github :repo "tonyaldon/org-bars"))

(package! org-drill
  :recipe (:host gitlab :repo "phillord/org-drill"))

(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! org-roam)
(package! citar-org-roam)

(when (modulep! :lang org +noter)
  (package! org-noter :recipe
    (:host github :repo "org-noter/org-noter"
     :files ("*.el" "modules/*.el")))
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

(package! easy-hugo)
(package! easy-jekyll)
