;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

;; this gives us fullscreen on startup
(add-hook 'server-switch-hook #'raise-frame)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(map! :leader
      :desc "Toggle Zen Mode"
      "z" #'+zen/toggle)
;;
;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(map! :leader
      :desc "toggle copilot"
      "t" #'copilot-mode)

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "Alexander Gusev"
      user-mail-address "goose@soulbound.xyz")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:

                                        ;(setq doom-font (font-spec :family "Uncial Antiqua" :size 15)
(setq doom-font (font-spec :family "Monoid Nerd Font Mono" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Monoid Nerd Font" :size 18)
      doom-big-font (font-spec :family "Monoid Nerd Font Mono" :size 22))
;;
;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
(after! doom-themes
  (setq
   doom-themes-enable-bold t    ; if nil, bold is universally disabled
   doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
                                        ;'(font-lock-keyword-face :slant italic)
  )
(setq doom-modeline-time nil)
;;
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type 'relative)
(setq tab-width 2)
(setq evil-shift-width 2)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")


;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
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
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(add-hook 'solidity-mode-hook
          (lambda ()
            (setq flycheck-mode nil)))
(setq flycheck-solidity-solium-soliumrcfile "/Users/goose/.soliumrc.json")
(setq solidity-flycheck-use-project t)
(setq solidity-flycheck-solc-additional-allow-paths '("/Users/goose/buidl_guidl/se-2/packages/hardhat/node_modules/"))

;; this is to prevent a new workspace from being made when emacs daemon is started
(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

;; chatgpt
(use-package! chatgpt
  :config
  (setq chatgpt-api-key (getenv "OPENAI_API_KEY")))

(map!
 :leader
 :desc "chatgpt reply"
 "cgg" #'chatgpt-reply)

(map!
 :leader
 :desc "GPT-4 reply"
 "cgG" #'chatgpt-reply-gpt-4)
(map!
 :leader
 :desc "chatgpt new reply"
 "cgn" #'chatgpt-new-reply)

(map!
 :leader
 :desc "chatgpt refactor region"
 "cgr" #'chatgpt-refactor-region)

(map!
 :leader
 :desc "GPT-4 refactor region"
 "cgR" #'chatgpt-refactor-region-gpt-4)

(map!
 :leader
 :desc "chatgpt explain region"
 "cge" #'chatgpt-explain-region)

(map!
 :leader
 :desc "GPT-4 explain region"
 "cgE" #'chatgpt-explain-region-gpt-4)

(map!
 :leader
 :desc "chatgpt debug region"
 "cgd" #'chatgpt-debug-region)

(map!
 :leader
 :desc "GPT-4 debug region"
 "cgD" #'chatgpt-debug-region-gpt-4)

(map!
 :leader
 :desc "chatgpt prompt region"
 "cgp" #'chatgpt-prompt-region)
(map!
 :leader
 :desc "GPT-4 prompt region"
 "cgP" #'chatgpt-prompt-region-gpt-4)

;; emms directory
(setq emms-source-file-default-directory "~/bandcamp/")

;; emms keybindings
(map! :leader
      :desc "open emms"
      "ee" #'emms)

(map! :leader
      :desc "toggle play/pause"
      "ep" #'emms-pause)

(map! :leader
      :desc "open playlist"
      "eo" #'emms-play-playlist)

(map! :leader
      :desc "open directory"
      "ed" #'emms-play-directory)

(setq fancy-splash-image "~/.config/doom/OrbBanner.png")
(setq frame-title-format "Wizard's Lair")

(defun play-song (song)
  (async-start
   (lambda ()
     (shell-command (concat "mpv ~/.config/doom/" song)))))

(defun shadow-money-wizard-gang ()
  (play-song "Trimmed.mp3"))

;; (if (daemonp)
;;     (add-hook 'after-make-frame-functions
;;               (lambda (frame)
;;                 (select-frame frame)
;;                 (shadow-money-wizard-gang)))
;;   (shadow-money-wizard-gang))

(defun org-quest-complete (marker)
  (when (eq (plist-get marker :type) 'todo-state-change)
    (let ((todo-state (org-get-todo-state)))
      (when (string= todo-state "DONE")
        (play-song "questDone.mp3"))
      (when (string= todo-state "TODO")
        (play-song "questStart.mp3")))))

(add-hook 'org-trigger-hook 'org-quest-complete)

;; doom modeline config
(setq display-time-mode nil)
(setq org-clock-clocked-in-display 'frame-title)

;; solidity lsp-support
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("solidity-ls" "--stdio")) :major-modes '(solidity-mode) :priority -1 :server-id 'solidity-ls)))

;; solidity lsp config
(add-hook 'solidity-mode-hook (lambda ()
                                (progn
                                  (format-all-mode -1)
                                  (lsp))))

;; rust config
(after! lsp-mode
  (progn
    (setq lsp-rust-analyzer-server-display-inlay-hints t)
    (setq lsp-rust-analyzer-inlay-hints-mode t))
  (setq company-minimum-prefix-length 1))

(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(setq tab-width 2)
(setq evil-shift-width 2)

;; org journal template
(setq org-journal-date-format
      (concat "%A, %x"
              "
** TODO Daily Tasks
*** TODO 4 hours minimum deep work on my work daily
*** TODO 1 hour minimum deep work on side project daily
*** TODO Train minimum 30 mins per day for my activity of choice
*** TODO Read for 15 mins daily
*** TODO Practice Music 15 mins per day
*** TODO Do a physical recovery activity each day
*** TODO No computer before bed
*** TODO Walk with Ariel
*** TODO No breaking my allergen diet
"))
