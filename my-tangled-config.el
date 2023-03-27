(add-hook 'server-switch-hook #'raise-frame)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(defun play-song (song)
  (async-start
   (lambda ()
     (shell-command (concat "mpv ~/.config/doom/" song)))
  (lambda ()
     nil)))
(defun shadow-money-wizard-gang ()
  (interactive)
  (play-song "Trimmed.mp3"))

(if (daemonp)
    (add-hook 'after-make-frame-functions
              (lambda (frame)
                (select-frame frame)
                (shadow-money-wizard-gang)))
  (shadow-money-wizard-gang))

(shadow-money-wizard-gang)

(defun org-quest-complete (marker)
  (when (eq (plist-get marker :type) 'todo-state-change)
    (let ((todo-state (org-get-todo-state)))
      (when (string= todo-state "DONE")
        (play-song "questDone.mp3"))
      (when (string= todo-state "TODO")
        (play-song "questStart.mp3")))))

(add-hook 'org-trigger-hook 'org-quest-complete)

(setq fancy-splash-image "~/.config/doom/OrbBanner.png")
(setq frame-title-format "Wizard's Lair")

(setq doom-font (font-spec :family "Monoid Nerd Font Mono" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Monoid Nerd Font" :size 18)
      doom-big-font (font-spec :family "Monoid Nerd Font Mono" :size 22))

(setq emms-source-file-default-directory "~/bandcamp/")

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

(package! chatgpt
  :recipe (:host github :repo "MercuricChloride/chatgpt.el"))

(use-package! chatgpt
  :config
  (setq chatgpt-api-key (getenv "OPENAI_API_KEY")))

(map! :leader
      :desc "chatgpt"
      "cgg" #'chatgpt-reply)
(map! :leader
      :desc "chatgpt"
      "cgp" #'chatgpt-paste)
(map! :leader
      :desc "chatgpt"
      "cgs" #'chatgpt-skyrimify)

(display-time)
(setq display-time-format "%H:%M")

(package! copilot
  :recipe (:host github :repo "zerolfx/copilot.el" :files ("*.el" "dist")))

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

(add-hook 'solidity-mode-hook
          (lambda ()
            (setq flycheck-mode nil)))
(setq flycheck-solidity-solium-soliumrcfile "~/.soliumrc.json")
(setq solidity-flycheck-use-project t)
(setq solidity-flycheck-solc-additional-allow-paths '("~/buidl_guidl/se-2/packages/hardhat/node_modules/")) ;; this is super hacky and honestly doesn't work yet. But i'm messing with it :)

(setq doom-theme 'doom-one)
(setq display-line-numbers-type 'relative)
(setq tab-width 2)
(setq evil-shift-width 2)

(after! doom-themes
  (setq
   doom-themes-enable-bold t    ; if nil, bold is universally disabled
   doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(custom-set-faces!
  '(font-lock-comment-face :slant italic))

(package! org-auto-tangle
  :recipe (:host github :repo "yilkalargaw/org-auto-tangle"))

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(setq org-directory "~/org/")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")

(setq user-full-name "Alexander Gusev"
      user-mail-address "goose@soulbound.xyz")

(map! :leader
      :desc "Toggle Zen Mode"
      "z" #'+zen/toggle)
