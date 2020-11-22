;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
;;(setq user-full-name "John Doe"
;;      user-mail-address "john@doe.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/projects/org")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; Modeline setup
(setq display-time-day-and-date t)
(setq display-time-string-forms
       '((propertize (format-time-string "%I:%M%p %a %y-%m-%d"))))
(display-time)

;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;; Startup options
;(remove-hook '+doom-dashboard-functions #'doom-dashboard-widget-shortmenu)
;(setq +doom-dashboard-banner-file (expand-file-name "banner.png" doom-private-dir))
(setq inhibit-splash-screen t)
(add-hook 'after-init-hook #'org-agenda-list)
;; ox-hugo settings
(setq hugo-base-dir "~/projects/hugo-project")

;; for native-comp branch
(setq comp-async-jobs-number 4 ;; not using all cores
      comp-deferred-compilation t
      comp-deferred-compilation-black-list '())

;; Org-capture templates
(setq org-my-anki-file "~/projects/anki/anki.org")
(after! org
        (add-to-list 'org-capture-templates
             '("a" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
        (add-to-list 'org-capture-templates
             '("A" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: Mega\n:END:\n** Text\n%x\n** Extra\n"))
        (add-to-list 'org-capture-templates
             '("m" "Meeting"
               entry
               (file org-default-notes-file)
               "* Meeting with %? :MEETING:\n" :clock-in t :clock-resume t)))

;; Enable beacon-mode to show my cursor everywhere
(beacon-mode 1)

;; org-roam config
(use-package! org-roam
  :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  :hook
  (after-init . org-roam-mode)
  :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture)
  (setq org-roam-directory (file-truename "~/projects/org-roam")
        org-roam-db-gc-threshold most-positive-fixnum
        org-roam-graph-exclude-matcher "personal"
        org-roam-tag-sources '(prop last-directory)
        org-id-link-to-org-use-id t)
  :config
  (setq org-roam-capture-templates
        '(("n" "normal" plain (function org-roam--capture-get-point)
           "%?"
           :file-name "${slug}"
           :head "#+setupfile:./hugo_setup.org
#+hugo_slug: ${slug}
#+title: ${title}\n"
           :immediate-finish t
           :unnarrowed t)
          ("p" "personal" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "personal/${slug}"
           :head "#+title: ${title}\n"
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           "%?"
           :file-name "lit/${slug}"
           :head "#+setupfile:./hugo_setup.org
#+roam_key: ${ref}
#+hugo_slug: ${slug}
#+roam_tags: website
#+title: ${title}
- source :: ${ref}"
           :unnarrowed t)))
  (set-company-backend! 'org-mode '(company-capf)))

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq orb-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        `(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "lit/${slug}"
           :head ,(concat
                   "#+setupfile: ./hugo_setup.org\n"
                   "#+title: ${=key=}: ${title}\n"
                   "#+roam_key: ${ref}\n\n"
                   "* ${title}\n"
                   "  :PROPERTIES:\n"
                   "  :Custom_ID: ${=key=}\n"
                   "  :URL: ${url}\n"
                   "  :AUTHOR: ${author-or-editor}\n"
                   "  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
                   "  :NOTER_PAGE: \n"
                   "  :END:\n")
           :unnarrowed t))))

(use-package! bibtex-completion
  :config
  (setq bibtex-completion-notes-path "~/projects/org-roam"
        bibtex-completion-bibliography "~/projects/org-roam/biblio.bib"
        bibtex-completion-pdf-field "file"
        bibtex-completion-notes-path "~/projects/lit/"
        bibtex-completion-notes-template-multiple-files
         (concat
          "#+title: ${title}\n"
          "#+roam_key: cite:${=key=}\n"
          "* TODO Notes\n"
          ":PROPERTIES:\n"
          ":Custom_ID: ${=key=}\n"
          ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
          ":AUTHOR: ${author-abbrev}\n"
          ":JOURNAL: ${journaltitle}\n"
          ":DATE: ${date}\n"
          ":YEAR: ${year}\n"
          ":DOI: ${doi}\n"
          ":URL: ${url}\n"
          ":END:\n\n"
          )))

;; org-mode, todo, and org-agenda config
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@/!)")))

;; Load local configuration
(load! "~/.local/emacs/localconfig.el")

