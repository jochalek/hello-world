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
  :init
  (require 'org-habit)
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
               "* Meeting with %? :MEETING:\n" :clock-in t :clock-resume t))
        (add-to-list 'org-capture-templates
             '("i" "Inbox"
               entry
               (file+headline "~/projects/org/todo.org" "Inbox")
               "* TODO %?\n /Entered on/ %u"))
        (add-to-list 'org-capture-templates
                     '("d" "Daily Check-in"
                       entry
                       (file+olp+datetree +org-capture-journal-file)
                       "* %U Daily Check-in\n** Three things I am grateful for\n1. %?\n** I am looking forward to\n** One thing I can do today no matter what\n- [ ]\n** Most important thing to focus on today" :prepend t)))

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

;; Agenda
(use-package! org-agenda
  :init
  (map! "<f1>" #'joch/switch-to-agenda)
  (setq org-agenda-block-separator nil
        org-agenda-start-with-log-mode t)
  (defun joch/switch-to-agenda ()
    (interactive)
    (org-agenda nil " "))
  (setq joch/org-agenda-directory (file-truename "~/projects/org/"))
  :config
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
        `((" " "Agenda"
                                      ((agenda ""
                                               ((org-agenda-span 'week)
                                                (org-agenda-start-day "+0d")
                                                (org-deadline-warning-days 365)))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Inbox")
                                              (org-agenda-files '(,(concat joch/org-agenda-directory "todo.org")))))
                                       ;(todo "TODO"
                                       ;      ((org-agenda-overriding-header "Emails")
                                       ;       (org-agenda-files '(,(concat org-agenda-directory "emails.org")))))
                                       (todo "NEXT"
                                             ((org-agenda-overriding-header "In Progress")
                                              (org-agenda-files '(,(concat joch/org-agenda-directory "projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "Active Projects")
                                              (org-agenda-skip-function #'joch/skip-projects)
                                              (org-agenda-files '(,(concat joch/org-agenda-directory "projects.org")))))
                                       (todo "TODO"
                                             ((org-agenda-overriding-header "One-off Tasks")
                                              (org-agenda-files '(,(concat joch/org-agenda-directory "personal.org")))
                                              (org-agenda-skip-function '(org-agenda-skip-entry-if 'deadline 'scheduled)))))))))
      ;; '(("o" "My Agenda"
      ;;    ((todo "TODO" (
      ;;                 (org-agenda-overriding-header "⚡ TO DO:\n")
      ;;                 (org-agenda-remove-tags t)
      ;;                 (org-agenda-prefix-format "  %-2i %-13b")
      ;;                 (org-agenda-todo-keyword-format "")))
      ;;     (agenda "" (
      ;;                 (org-agenda-skip-scheduled-if-done t)
      ;;                 (org-agenda-skip-timestamp-if-done t)
      ;;                 (org-agenda-skip-deadline-if-done t)
      ;;                 (org-agenda-start-day "+0d")
      ;;                 (org-agenda-span 5)
      ;;                 (org-agenda-overriding-header "⚡ SCHEDULE:\n")
      ;;                 (org-agenda-repeating-timestamp-show-all nil)
      ;;                 (org-agenda-remove-tags t)
      ;;                 (org-agenda-prefix-format "  %-3i  %-15b%t %s")
      ;;                  ;; (concat "  %-3i  %-15b %t%s" org-agenda-hidden-separator))
      ;;                 (org-agenda-todo-keyword-format " ☐ ")
      ;;                 (org-agenda-time)
      ;;                 (org-agenda-current-time-string "⮜┈┈┈┈┈┈┈ now")
      ;;                 ;; (org-agenda-scheduled-leaders '("" ""))
      ;;                 ;; (org-agenda-deadline-leaders '("" ""))
      ;;                 (org-agenda-time-grid (quote ((require-timed remove-match) (0900 2100) "      " "┈┈┈┈┈┈┈┈┈┈┈┈┈"))))))))))

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

;; org-cv export config
(use-package! ox-moderncv
    :init (require 'ox-moderncv))

;; Load ob-ess-julia and dependencies
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

;; Load local configuration
(load! "~/.local/emacs/localconfig.el")
