;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)

; For org-roam second brain
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! company-org-roam
  :recipe (:host github :repo "jethrokuan/company-org-roam"))
(package! org-download)

; For hosting my Second Brain
(package! ox-texinfo+
  :recipe (:host github :repo "tarsius/ox-texinfo-plus"))

; CV support for tex
(package! org-cv
  :recipe (:host gitlab :repo "Titan-C/org-cv"))

; Themes
(package! less-theme
  :recipe (:host gitlab :repo "nobiot/less-theme"
                       :files ("*.el" "less")))

(package! beacon
  :recipe (:host github :repo "Malabarba/beacon"
           :files ("*.el" "beacon")))

; Academic
;(package! ivy-bibtex)
(package! pandoc-mode)
(package! pdf-tools)
(package! org-noter)
(package! org-ref)
;(package! helm-bibtex)

; Long form writing
;(package! olivetti)

; Study tools
(package! anki-editor)

;; Julia Language Server
;; (package! lsp-julia
;;   :recipe (:host github :repo "non-Jedi/lsp-julia"
;;            :files (:defaults "languageserver")))
(package! eglot-jl)
(package! ob-ess-julia)
  ;; :recipe (:host github :repo "frederic-santos/ob-ess-julia"
  ;;          :files ("*.el")))

(package! nano-emacs
  :recipe (:host github :repo "rougier/nano-emacs"
           :files ("*.el")))
