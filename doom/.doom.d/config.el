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

;; `load-theme' function. These settings are configured locally due to screen size:
;; (setq
;;  doom-font (font-spec :family "Iosevka Term SS04" :size 24 :weight 'light)
;;  doom-big-font (font-spec :family "Iosevka Term SS04" :size 36)
;;  doom-variable-pitch-font (font-spec :family "SF Pro Text")
;;  )

;; System specific settings
;; Ubuntu Desktop
(when (string-equal (system-name) "justin-ubuntu-desktop")
  (setq
   doom-font (font-spec :family "DejaVu Sans Mono" :size 18 :weight 'light)
   doom-big-font (font-spec :family "DejaVu Sans Mono" :size 24)
   doom-variable-pitch-font (font-spec :family "DejaVu Sans")))
;; Guix Lenovo Laptop
(when (string-equal (system-name) "spike")
  (setq
   doom-font (font-spec :family "DejaVu Sans Mono" :size 20 :weight 'light)
   doom-big-font (font-spec :family "DejaVu Sans Mono" :size 26)
   doom-variable-pitch-font (font-spec :family "DejaVu Sans")))
;; Lenovo Laptop
(when (string-equal (system-name) "PIEROGI")
  (setq
   doom-font (font-spec :family "Iosevka Term SS04" :size 24 :weight 'light)
   doom-big-font (font-spec :family "Iosevka Term SS04" :size 36)
   doom-variable-pitch-font (font-spec :family "SF Pro Text"))
  (add-to-list 'exec-path "c:/Users/jocha/anaconda3") ;; Path to Python
  (add-to-list 'exec-path "~/anaconda3/Library/bin") ;; Path to sqlite3 for org-roam
  (setenv "LANG" "en_GB") ;; Set up spell checker using Hunspell
  (setq ispell-program-name "~/bin/hunspell-1.3.2-3-w32-bin/bin/hunspell.exe")
  )

;; My attempt to change theme on startup based on day/evening.
(setq doom-theme (if (and (string-greaterp (format-time-string "%I") "05")
                     (string-equal (format-time-string "%p") "PM")
                     (string-lessp (format-time-string "%I") "12")
                     t)
 'doom-dark+
 'zaiste))

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/Nextcloud/org/")
(setq joch/zettel-dir "~/Nextcloud/zettels/")
(setq org-roam-directory (concat joch/zettel-dir "org-roam/")) ;; FIXME Try not to declare this twice...
(setq org-roam-v2-ack t)
;; I'll want to use the diary functionality for scheduling non-tasks within
;; org-agenda
(setq diary-file "~/Nextcloud/org/diary")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)
(dolist (mode '(org-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0)))
  (add-hook mode (lambda () (vi-tilde-fringe-mode 0)))
  (add-hook mode (lambda () (company-mode 0)))
  (add-hook mode (lambda () (visual-fill-column-mode 1))))

(setq recentf-max-saved-items 20)

;; Modeline setup
(setq display-time-day-and-date t)
(setq display-time-string-forms
       '((propertize (format-time-string "%I:%M%p %a %y-%m-%d"))))
(use-package! time
  :defer 10
  :config
  (display-time))

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
;;(add-hook 'after-init-hook #'org-agenda-list)
;; ox-hugo settings
;(setq hugo-base-dir "~/projects/hugo-project")

;; for native-comp branch
(setq comp-async-jobs-number 4 ;; not using all cores
      comp-deferred-compilation t
      comp-deferred-compilation-black-list '())

;; Org-capture templates
(after! org
  :init
  (require 'org-habit)
  (require 'nano-writer)
  (require 'visual-fill-column)
  (add-hook 'writer-mode-hook #'visual-fill-column-mode)
        ;; Anki card captures
        (add-to-list 'org-capture-templates
             '("a" "Anki Cards"))
        (add-to-list 'org-capture-templates
             '("aA" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: Mega\n:END:\n** Front\n%?\n** Back\n%x\n"))
        (add-to-list 'org-capture-templates
             '("aa" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   \n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze-AnKingMasterClone\n:ANKI_DECK: Mega\n:ANKI_TAGS:\n:END:\n** Text\n%i%?\n** Extra\n"))
        ;; Quick captures for stuff I need to do or notes from impromptu meeting
        (add-to-list 'org-capture-templates
             '("m" "Meeting"
               entry
               (file org-default-notes-file)
               "* Meeting with %? :MEETING:\n" :clock-in t :clock-resume t))
        (add-to-list 'org-capture-templates
             `("i" "Inbox"
               entry
               (file+headline ,(concat org-directory "todo.org") "Inbox")
               "* TODO %?\n /Entered on/ %u"))
        ;; Journal entries
        (add-to-list 'org-capture-templates
                     '("j" "Journal Entries"))
        (add-to-list 'org-capture-templates
                     '("jm" "Morning Check-in"
                       entry
                       (file+olp+datetree +org-capture-journal-file)
                       "* %U Morning Check-in\n** Three things I am grateful for\n1. %?\n** I am looking forward to\n** One thing I can do today no matter what\n- [ ]\n** Most important thing to focus on today\n** Today's items" :prepend t))
        (add-to-list 'org-capture-templates
                     '("jl" "Lunch Check-in"
                       entry
                       (file+olp+datetree +org-capture-journal-file)
                       "* %U Lunch Check-in\n FIXME ENTER SOMETHING HERE"))
        (add-to-list 'org-capture-templates
                     '("je" "End of Day Wrap"
                       entry
                       (file+olp+datetree +org-capture-journal-file)
                       "* %U Wrap up the day\n- [ ] Todo your unfinished stuff\n- [ ] Process the Inbox\n- [ ] Schedule Tommorow\n"))
        (add-to-list 'org-capture-templates
                     '("jw" "Weekly Review"
                       entry
                       (file+olp+datetree +org-capture-journal-file)
                       "* %U Weekly Review\n** Assess my performance this week\n- Did I get everything done?\n %?\n - If not, why not?\n- What worked well?\n- What could be improved?\n** Plan the upcoming week\n- What do I want to achieve?\n- When will I achieve it?\n- What new approaches/techniques can I try this week?\n"))
        (setq org-my-anki-file "~/projects/anki/anki.org")
        :config
;; org-mode, todo, and org-agenda config
(setq org-todo-keywords
      '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
        (sequence "WAITING(w@/!)" "INACTIVE(i)" "|" "CANCELLED(c@/!)")))

;; org-mode config
(setq org-tag-alist '(("@errand" . ?e)
                      ("@school" . ?s)
                      ("@home" . ?h)
                      (:newline)
                      ("CANCELLED" . ?c)))
(global-set-key (kbd "C-c l") #'org-store-link)
(setq org-refile-targets (quote ((nil :maxlevel . 1)
                                 (org-agenda-files . (:maxlevel . 1)))))

;; org-latex to pdf with bibliography
(setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode' -pdf -bibtex -f %f"))
)

;; Enable beacon-mode to show my cursor everywhere
(use-package! beacon
  :config
  (beacon-mode 1))

;; org-roam config
(after! org-roam
  ;; :commands (org-roam-insert org-roam-find-file org-roam-switch-to-buffer org-roam)
  ;; :hook
  ;; (after-init . org-roam-mode)
  ;; :init
  (map! :leader
        :prefix "n"
        :desc "org-roam" "l" #'org-roam-buffer-toggle
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
  (setq org-roam-directory (concat joch/zettel-dir "org-roam/")
        org-roam-db-gc-threshold most-positive-fixnum
        ;; org-roam-graph-exclude-matcher "personal"
        org-id-link-to-org-use-id t)
;;   ;; :config
;;   (setq org-roam-capture-templates
;;         '(("n" "normal" plain (function org-roam--capture-get-point)
;;            "%?"
;;            :file-name "${slug}"
;;            :head "#+setupfile:../hugo_setup.org
;; #+hugo_slug: ${slug}
;; #+HUGO_DRAFT: true
;; #+title: ${title}\n"
;;            :immediate-finish t
;;            :unnarrowed t)
;;           ("p" "personal" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "personal/${slug}"
;;            :head "#+title: ${title}\n"
;;            :unnarrowed t)))
;;   (setq org-roam-capture-ref-templates
;;         '(("r" "ref" plain (function org-roam-capture--get-point)
;;            "%?"
;;            :file-name "lit/${slug}"
;;            :head "#+setupfile:./hugo_setup.org
;; #+roam_key: ${ref}
;; #+hugo_slug: ${slug}
;; #+roam_tags: website
;; #+title: ${title}
;; - source :: ${ref}"
;;            :unnarrowed t)))


 (setq org-roam-mode-sections
        (list #'org-roam-backlinks-insert-section
              #'org-roam-reflinks-insert-section
              ;; #'org-roam-unlinked-references-insert-section
              ))
  (org-roam-setup)
  (setq org-roam-capture-templates
        '(("n" "normal" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+setupfile:../hugo_setup.org
#+hugo_slug: ${slug}
#+HUGO_DRAFT: true
#+title: ${title}\n")
           :immediate-finish t
           :unnarrowed t)))
  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)))

;;   (set-company-backend! 'org-mode '(company-capf))
)

(use-package! org-roam-protocol
  :after org-protocol)

(use-package! org-roam-bibtex
  :after org-roam
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
  :after org
  :config
  (map! :map global-map "<f6>" #'helm-bibtex)
  (setq bibtex-completion-notes-path org-roam-directory
        bibtex-completion-bibliography (concat org-roam-directory "biblio.bib")
        bibtex-completion-pdf-field "file"
        bibtex-completion-library-path (concat joch/zettel-dir "lit/")
        bibtex-completion-notes-path (concat bibtex-completion-library-path "litnotes/")
        ;bibtex-completion-pdf-open-function 'org-open-file
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

;; Citation configs
;; (setq reftex-default-bibliography '("~/zettels/org-roam/biblio.bib"))

;; see org-ref for use of these variables
(use-package! org-ref
  :after org
  :config
  (setq org-ref-bibliography-notes (concat joch/zettel-dir "lit/litnotes.org")
        org-ref-default-bibliography `(,(concat org-roam-directory "biblio.bib"))
  org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
  ;org-ref-open-pdf-function 'bibtex-completion-pdf-open-function
  org-ref-notes-function 'orb-edit-notes)
  (tooltip-mode 1)
  )

(use-package! org-noter
  :after
  (:any org pdf-view))

;; ;; Pandoc mode to convert org files to other formats such as .docx, .md, or .pdf via LaTex
;; ;(add-hook 'text-mode-hook 'pandoc-mode)
;; ;(add-hook 'pandoc-mode-hook 'pandoc-load-default-settings)

;; Agenda config
(use-package! org-agenda
  :defer t
  :config
  (map! :map global-map "<f1>" #'joch/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (setq org-agenda-files `(,(file-truename org-directory)))
  (defun joch/switch-to-agenda ()
    (interactive)
    (org-agenda nil "1"))
  (setq joch/org-agenda-directory (file-truename org-directory))
  (defun joch/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

  (defun joch/skip-projects ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((joch/is-project-p)
        next-headline)
       (t
        nil)))))

  (setq org-columns-default-format "%40ITEM(Task) %Effort(EE){:} %CLOCKSUM(Time Spent) %SCHEDULED(Scheduled) %DEADLINE(Deadline)")
  (setq org-agenda-custom-commands
        `(("1" "Today Agenda"
                                      ((agenda ""
                                               (
                                                (org-agenda-overriding-header "Today's Schedule:\n Views:z Filters:s\n")
                                                (org-agenda-span 'day)
                                                (org-agenda-start-day "+0d")
                                                (org-deadline-warning-days 3)
                                                ))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "todo.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "Up Next")
                                              ;; (org-agenda-files (file-truename org-directory))))
                                              (org-agenda-files `(,(file-truename org-directory)))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'joch/skip-projects)
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Outstanding Tasks")
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "personal.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))
         ("w" "Week Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-agenda-start-day "+0d")
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "todo.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "Up Next")
                                              ;; (org-agenda-files (quote ("~/Nextcloud/org/personal.org"
                                              ;;                          "~/Nextcloud/org/projects.org")))))
                                              (org-agenda-files (--map (concat joch/org-agenda-directory it)
                                                                       '("personal.org"
                                                                         "projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'joch/skip-projects)
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files `(,(concat joch/org-agenda-directory "personal.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled))))))
          )))
(after! org-agenda
(defvar joch/org-agenda-bulk-process-key ?f
  "Default key for bulk processing inbox items.")

(defun joch/org-process-inbox ()
  "Called in org-agenda-mode, processes all inbox items."
  (interactive)
  (org-agenda-bulk-mark-regexp "inbox:")
  (joch/bulk-process-entries))

(defvar joch/org-current-effort "1:00"
  "Current effort for agenda items.")

(defun joch/my-org-agenda-set-effort (effort)
  "Set the effort property for the current headline."
  (interactive
   (list (read-string (format "Effort [%s]: " joch/org-current-effort) nil nil joch/org-current-effort)))
  (setq joch/org-current-effort effort)
  (org-agenda-check-no-diary)
  (let* ((hdmarker (or (org-get-at-bol 'org-hd-marker)
                       (org-agenda-error)))
         (buffer (marker-buffer hdmarker))
         (pos (marker-position hdmarker))
         (inhibit-read-only t)
         newhead)
    (org-with-remote-undo buffer
      (with-current-buffer buffer
        (widen)
        (goto-char pos)
        (org-show-context 'agenda)
        (funcall-interactively 'org-set-effort nil joch/org-current-effort)
        (end-of-line 1)
        (setq newhead (org-get-heading)))
      (org-agenda-change-all-lines newhead hdmarker))))

(defun joch/org-agenda-process-inbox-item ()
  "Process a single item in the org-agenda."
  (org-with-wide-buffer
   (org-agenda-set-tags)
   (org-agenda-priority)
   (call-interactively 'joch/my-org-agenda-set-effort)
   (org-agenda-refile nil nil t)))

(defun joch/bulk-process-entries ()
  (if (not (null org-agenda-bulk-marked-entries))
      (let ((entries (reverse org-agenda-bulk-marked-entries))
            (processed 0)
            (skipped 0))
        (dolist (e entries)
          (let ((pos (text-property-any (point-min) (point-max) 'org-hd-marker e)))
            (if (not pos)
                (progn (message "Skipping removed entry at %s" e)
                       (cl-incf skipped))
              (goto-char pos)
              (let (org-loop-over-headlines-in-active-region) (funcall 'joch/org-agenda-process-inbox-item))
              ;; `post-command-hook' is not run yet.  We make sure any
              ;; pending log note is processed.
              (when (or (memq 'org-add-log-note (default-value 'post-command-hook))
                        (memq 'org-add-log-note post-command-hook))
                (org-add-log-note))
              (cl-incf processed))))
        (org-agenda-redo)
        (unless org-agenda-persistent-marks (org-agenda-bulk-unmark-all))
        (message "Acted on %d entries%s%s"
                 processed
                 (if (= skipped 0)
                     ""
                   (format ", skipped %d (disappeared before their turn)"
                           skipped))
                 (if (not org-agenda-persistent-marks) "" " (kept marked)")))))

(setq org-agenda-bulk-custom-functions `((,joch/org-agenda-bulk-process-key joch/org-agenda-process-inbox-item)))

(map! :map org-agenda-mode-map
      "r" #'joch/org-process-inbox
      "R" #'org-agenda-refile)
)

;; org-cv export config
(use-package! ox-moderncv
  :after org
  :init (require 'ox-moderncv))

;; ;; Load ob-ess-julia and dependencies
;; (use-package ob-ess-julia
;;   :ensure t
;;   :config
;;   ;; Add ess-julia into supported languages:
;;   (org-babel-do-load-languages 'org-babel-load-languages
;;                                (append org-babel-load-languages
;;                                        '((ess-julia . t))))
;;   ;; Link this language to ess-julia-mode (although it should be done by default):
;;   (setq org-src-lang-modes
;;         (append org-src-lang-modes '(("ess-julia" . ess-julia)))))

;; Encryption
(after! org-crypt
; Encrypt all entries before saving
(org-crypt-use-before-save-magic)
(setq org-tags-exclude-from-inheritance (quote ("crypt")))
; GPG key to use for encryption
;(setq org-crypt-key nil)
;; Either the Key ID or set to nil to use symmetric encryption.
  )
(setq auto-save-default nil)
;; Avoid #file.org# to appear
;(auto-save-visited-mode)
(setq create-lockfiles nil)
;; Avoid filename.ext~ to appear
(setq make-backup-files nil)

;; Auto-saving does not cooperate with org-crypt.el: so you need to
;; turn it off if you plan to use org-crypt.el quite often.  Otherwise,
;; you'll get an (annoying) message each time you start Org.

;; To turn it off only locally, you can insert this:
;;
;; # -*- buffer-auto-save-file-name: nil; -*-

;; Evil-mode config
;; I want evil to navigate visual lines on the "screen" not by "in the computer" lines.
(after! evil
  (setq evil-respect-visual-line-mode t)
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;; Random performance improvement attempts
(setq-default bidi-paragraph-direction 'left-to-right)
(setq bidi-inhibit-bpa t)
(use-package! so-long
  :defer t
  :config
  (global-so-long-mode 1))

;; Mode for clean writing
(use-package! nano-emacs
  :defer t)

;; anki-editor config
(use-package! anki-editor
  :defer t
  :init
  (setq anki-editor-org-tags-as-anki-tags nil)
  (map! :mode anki-editor-mode
        :leader
        :prefix "a"
        :desc "cloze region ARGS" "c" #'anki-editor-cloze-region
        :desc "cloze word" "w" #'anki-editor-cloze-dwim))

(after! conda
  (conda-env-initialize-interactive-shells))

;; A regexp search for my braindump. Disabled because it eagerly loads.
;; (use-package! rg
;;   :config
;;   (rg-define-search joch/rg-braindump
;;     "RipGrep my braindump."
;;     :query ask
;;     :format regexp
;;     :files "everything"
;;     :dir org-roam-directory
;;     :confirm prefix))

;; Julia config with lsp
(setq lsp-julia-default-environment "~/.julia/environments/v1.6")
(setq julia-repl-executable-records
          '((default "julia")
            (v1.6 "c:/Users/jocha/AppData/Local/Programs/Julia-1.6.1/bin/julia")))

;; ERC config
(setq erc-server "irc.libera.chat"
      erc-nick "joch"
      erc-user-full-name "jochalek"
      erc-track-shorten-start 8
      erc-autojoin-channels-alist '(("irc.libera.chat" "#systemcrafters" "#emacs"))
      erc-kill-buffer-on-part t
            erc-auto-query 'bury)

;; (setq explicit-shell-file-name "c:/Program Files/PowerShell/7/pwsh.exe")
;; Load local configuration
(load! "~/.local/emacs/localconfig.el")
