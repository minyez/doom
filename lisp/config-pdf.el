;;; config-pdf.el -*- lexical-binding: t; -*-
(after! pdf-view
  (map! :map pdf-view-mode-map
        :nv "z g"        #'pdf-view-goto-page
        :nv "z r"        #'image-rotate       ;; rotate the page (defined in image.el)
        :nv "SPC a a l"  #'pdf-annot-list-annotations
        :nv "SPC a a h"  #'pdf-annot-add-highlight-markup-annotation
        :nv "SPC a a u"  #'pdf-annot-add-underline-markup-annotation
        )
)

(after! pdf-tools
  :config
  ;; activate when opening pdf, otherwise the evil keybindings will not work
  (pdf-loader-install)
)

(provide 'config-pdf)
;;; config-pdf.el ends here
