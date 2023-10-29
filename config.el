(add-hook 'server-switch-hook #'raise-frame)
(add-hook 'window-setup-hook #'toggle-frame-fullscreen)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(after! persp-mode
  (setq persp-emacsclient-init-frame-behaviour-override "main"))

(defun play-song (song)
  (async-start
   (lambda ()
     (shell-command (concat "mpv ~/.config/doom/" song)))))

(defun shadow-money-wizard-gang ()
  (interactive)
  (play-song "Trimmed.mp3"))

(defun org-quest-complete (marker)
  (interactive)
  (when (eq (plist-get marker :type) 'todo-state-change)
    (let ((todo-state (org-get-todo-state)))
      (when (string= todo-state "DONE")
        (play-song "questDone.mp3"))
      (when (string= todo-state "TODO")
        (play-song "questStart.mp3")))))

(add-hook 'org-trigger-hook 'org-quest-complete)

(setq fancy-splash-image "~/.config/doom/OrbBanner.png")
(setq frame-title-format "Castin spells")

(setq transparent-background-mode t)

(defun custom/transparent-mode ()
        (interactive)
        (if    transparent-background-mode
               (progn
                (setq transparent-background-mode nil)
                (set-frame-parameter nil 'alpha-background 100))
               (progn
                (setq transparent-background-mode )
                (set-frame-parameter nil 'alpha-background 70))))

(setq doom-font (font-spec :family "Monoid Nerd Font Mono" :size 15 :weight 'semi-light)
      doom-variable-pitch-font (font-spec :family "Monoid Nerd Font Mono" :size 18)
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

(use-package! chatgpt
  :defer t
  :bind ("C-c q" . chatgpt-query))

(setq chatgpt-code-query-map
      '(
        ;; ChatGPT.el defaults, string for each shortcut
        ("bug" . "There is a bug in the following, please help me fix it.")
        ("doc" . "Please write the documentation for the following.")
        ("improve" . "Please improve the following.")
        ("understand" . "What is the following?")
        ("refactor" . "Please refactor the following.")
        ("suggest" . "Please make suggestions for the following.")
        ;; your shortcut
        ("prompt-name" . "My custom prompt.")))
;(use-package! chatgpt
  ;:config
  ;(setq chatgpt-api-key (getenv "OPENAI_API_KEY")))

;(map! :leader
      ;:desc "chatgpt"
      ;"cgg" #'chatgpt-reply)
;(map! :leader
      ;:desc "chatgpt"
      ;"cgp" #'chatgpt-paste)
;(map! :leader
      ;:desc "chatgpt"
      ;"cgs" #'chatgpt-skyrimify)

(display-time)
(setq display-time-format "%H:%M")

;; unmap the existing undo tree keybindings
(map!
      :leader
      :desc "comment out"
      "/" #'comment-or-uncomment-region-or-line)

(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
             (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))

(map!
 :after evil-snipe-mode
   "," nil)
(setq doom-localleader-key ",")

(require 'wat-mode)

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

(map! :leader
      :desc "toggle copilot"
      "t" #'copilot-mode)

(map! "<backtab>" #'copilot-accept-completion)

(after! copilot
  (defun my/copilot-or-evil-ret ()
    "Accept GitHub Copilot suggestion or execute 'evil-ret' command."
    (interactive)
    (if (and copilot-mode (copilot--overlay-visible))
        (copilot-accept-completion)
      (evil-ret)))
  ;; Bind the modified function to the Enter key in Evil mode
  (define-key evil-normal-state-map (kbd "RET") 'my/copilot-or-evil-ret)
  (define-key evil-insert-state-map (kbd "RET") 'my/copilot-or-evil-ret)
  (define-key evil-visual-state-map (kbd "RET") 'my/copilot-or-evil-ret)
  (setq copilot-node-executable "~/.local/share/nvm/v18.16.0/bin/node"))

(setq
 dashboard-center-content t
 dashboard-startup-banner "~/.config/doom/OrbBanner.png"
 doom-fallback-buffer-name "*dashboard*"
 dashboard-week-agenda t)

(dashboard-open)

(global-tree-sitter-mode)
(setq treesit-extra-load-path nil)
(setq treesit-language-source-alist '((solidity "https://github.com/JoranHonig/tree-sitter-solidity")))

  (defun nf/treesit-install-all-languages ()
    "Install all languages specified by `treesit-language-source-alist'."
    (interactive)
    (let ((languages (mapcar 'car treesit-language-source-alist)))
      (dolist (lang languages)
	      (treesit-install-language-grammar lang)
	      (message "`%s' parser was installed." lang)
	      (sit-for 0.75))))

;; solidity lsp-support
(after! lsp-mode
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection '("nomicfoundation-solidity-language-server" "--stdio")) :major-modes '(solidity-mode) :priority -1 :server-id 'solidity-ls)))

;; solidity lsp config
(add-hook 'solidity-mode-hook (lambda ()
                                (progn
                                  (format-all-mode -1)
                                  (lsp))))

(use-package! lsp-tailwindcss
  :init (setq lsp-tailwindcss-add-on-mode t))

(dap-auto-configure-mode)
(require 'dap-lldb)
(setq dap-lldb-debug-program '("rust-lldb"))

(setq doom-theme 'wheatgrass)
(setq display-line-numbers-type 'relative)
(setq tab-width 2)
(setq evil-shift-width 2)
(setq web-mode-code-indent-offset 2)

(after! doom-themes
  (setq
   doom-themes-enable-bold t    ; if nil, bold is universally disabled
   doom-themes-enable-italic t)) ; if nil, italics is universally disabled

(custom-set-faces!
  '(font-lock-comment-face :slant italic))


(setq treemacs-display-current-project-exclusively t)
(setq which-key-idle-delay 0.1)

;(set-popup-rules! '(("^\\*doom\:vterm-popup" :size 0.75 :side bottom :select t :autosave t :modeline t :ttl t :quit nil)))

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

(setq org-directory "~/org/")
(setq org-journal-dir "~/org/journal/")
(setq org-journal-file-format "%Y-%m-%d.org")

(map!
    :mode org-mode
    :desc "org pomodoro"
    :localleader
    "cp" #'org-pomodoro)

(defun my/org-clip-image ()
 (interactive)
 (let* ((default-directory "~/org/images/")
        (image-file (format-time-string "%Y%m%d%H%M%S.png")))
     (shell-command (concat "xclip -selection clipboard -t image/png -o > " image-file))
     (if (= (shell-commmand-to-string (concat "file --mime-type -b " image-file)) "image/png")
         (progn
          (insert (concat "[[" default-directory image-file "]]"))
          (org-display-inline-images))
         (progn
          (message "Clipboard does not contain an image")
          (delete-file image-file)))))

(setq org-pretty-entities t)

(setq user-full-name "Alexander Gusev"
      user-mail-address "goose@soulbound.xyz")

(map! :leader
      :desc "Toggle Zen Mode"
      "z" #'+zen/toggle)
