;; Set Up Packages ;;
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

(use-package gnu-elpa-keyring-update)

;; UI Tweaks ;;
(setq inhibit-startup-screen  t
      initial-scratch-message nil)

(menu-bar-mode -1)
(tool-bar-mode -1)

(setq frame-title-format "%b - Emacs"
      icon-title-format t)

(setq-default cursor-type 'bar)

(scroll-bar-mode -1)
(setq-default scroll-conservatively 1
              scroll-margin 3)

;; Line Numbers ;;
(global-display-line-numbers-mode)
(add-hook 'display-line-numbers-mode-hook
          (lambda ()
            (when display-line-numbers
              (setq display-line-numbers 'relative))))
(column-number-mode)

;; Highlighting ;;
(global-hl-line-mode)
(show-paren-mode)
(electric-pair-mode)

;; Replace selection on input. ;;
(delete-selection-mode)

;; Indentation ;;
(setq-default indent-tabs-mode nil
              tab-width 2)
(defvaralias 'standard-indent 'tab-width)
(defvaralias 'smie-indent-basic 'tab-width)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'sh-basic-offset 'tab-width)
(defvaralias 'js-indent-level 'tab-width)
(defvaralias 'python-indent-offset 'tab-width)
(defvaralias 'js-switch-indent-offset 'tab-width)
(defvaralias 'css-indent-offset 'tab-width)
(add-hook 'c-mode-common-hook
          (lambda ()
            (c-set-style "k&r" t)
            (c-set-offset 'label '-)
            (c-set-offset 'case-label '+)))

;; Evil ;;
(use-package evil
  :init
  (defvaralias 'evil-shift-width 'tab-width)
  (setq evil-undo-system 'undo-redo
        evil-want-C-u-scroll t)
  :config
  (evil-mode)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))
  (global-set-key (kbd "C-l") 'universal-argument)
  (define-advice evil-scroll-line-up (:around (fun &optional count))
    (funcall fun (max count 2)))
  (define-advice evil-scroll-line-down (:around (fun &optional count))
    (funcall fun (max count 2))))

;; Frame-Oriented Buffer Management ;;
(defvar unframable-buffer-conditions nil
  "A list of buffer conditions to be passed to buffer-match-p.
   A buffers matching any one of the conditions here will not
   be displayed in its own frame.")

(setq display-buffer-alist
      '((display-buffer-pop-up-frame-p
         display-buffer-pop-up-or-reuse-frame)))

(defun xdotool (&rest args)
  (string-remove-suffix
   "\n"
   (with-output-to-string
     (let ((status (apply #'call-process
                          "xdotool" nil standard-output nil args)))
       (unless (= status 0)
         (error "xdotool exited with a nonzero status"))))))

(defun x-activate-frame (frame)
  (let ((x-window-id (frame-parameter frame 'outer-window-id)))
    (xdotool "windowactivate" x-window-id)))

(defun frame-desktop (&optional frame)
  (let ((x-window-id (frame-parameter frame 'outer-window-id)))
    (xdotool "get_desktop_for_window" x-window-id)))

(defun get-buffer-window-on-current-desktop (&optional buffer)
  (let ((current-desktop (frame-desktop)))
    (cl-find-if
     (lambda (window)
       (let ((frame (window-frame window)))
         (string= current-desktop (frame-desktop frame))))
     (get-buffer-window-list buffer :no t))))

(defun display-buffer-pop-up-or-reuse-frame (buffer alist)
  (if-let* ((window (get-buffer-window-on-current-desktop buffer)))
      (prog1 window (x-activate-frame (window-frame window)))
    (display-buffer-pop-up-frame buffer alist)))

(defun display-buffer-pop-up-frame-p (buffer action)
  (not (or (eq 'display-buffer-same-window (car action))
           (buffer-match-p `(or ,@unframable-buffer-conditions)
                           buffer))))

(global-set-key (kbd "C-x 2") 'make-frame-command)
(global-set-key (kbd "C-x 3") 'make-frame-command)

(add-to-list 'unframable-buffer-conditions
             '(or "^\\*Completions\\*$"
                  "^\\*Disabled Command\\*$"
                  "^\\*Backtrace\\*$"
                  "^ \\*Deletions\\*$"))

;; Kill Orphaned Buffers ;;
(defvar orphanable-buffer-conditions nil
  "A list of buffer conditions to be passed to buffer-match-p,
   a buffer is orphaned if it matches at least one of the
   conditions in this list and is not currently displayed in
   at least one window.")

(defun buffer-orphanable-p (buffer)
  (buffer-match-p `(or ,@orphanable-buffer-conditions) buffer))

(defun kill-orphaned-buffers--window-buffer-change-hook (frame)
  (when-let* ((window (frame-selected-window frame))
              (buffer (window-old-buffer window)))
    (when (and (buffer-orphanable-p buffer)
               (null (get-buffer-window-list buffer :no t)))
      (kill-buffer buffer))))

(add-hook 'window-buffer-change-functions
          'kill-orphaned-buffers--window-buffer-change-hook)

(defun kill-orphaned-buffers--delete-frame-hook (frame)
  (cl-dolist (window (window-list frame :dont))
    (when-let* ((buffer (window-buffer window)))
      (when (and (buffer-orphanable-p buffer)
                 (null (remove window (get-buffer-window-list buffer :no t))))
        (kill-buffer buffer)))))

(add-hook 'delete-frame-functions
          'kill-orphaned-buffers--delete-frame-hook)

(add-to-list 'orphanable-buffer-conditions
             (lambda (buffer)
               (with-current-buffer buffer
                 (or buffer-file-name
                     list-buffers-directory))))
(add-to-list 'orphanable-buffer-conditions
             '(or "^\\*Help\\*$"
                  "^\\*Buffer List\\*$"
                  "^\\*xref\\*$"))

;; Autocompletion ;;
(keymap-set minibuffer-mode-map "C-." 'minibuffer-complete)

(use-package company
  :hook (after-init . global-company-mode)
  :bind (:map company-mode-map
              ("C-." . company-complete))
  :config
  (define-key company-active-map [escape] 'company-abort))

(use-package which-key
  :config (which-key-mode)
  :bind (:map which-key-mode-map
              ("s-w" . 'which-key-show-major-mode)))

;; IDE Support ;;
(use-package lsp-mode
  :bind (:map lsp-mode-map
              ("M-p" . flymake-goto-prev-error)
              ("M-n" . flymake-goto-next-error)
              ("C-c C-k" . lsp-workspace-compile))
  :hook ((c-mode . lsp)
         (c++-mode . lsp)
         (haskell-mode . lsp)
         (python-mode . lsp)
         (javascript-mode . lsp)
         (typescript-ts-mode . lsp)
         (vala-mode . lsp))
  :init
  (setq lsp-keymap-prefix "C-c l"
        ;; workaround for https://github.com/emacs-lsp/lsp-mode/issues/4473
        lsp-rename-use-prepare nil
        lsp-enable-indentation nil))

(add-hook 'lsp-mode-hook
          (setq eldoc-documentation-strategy 'eldoc-documentation-compose))

(use-package haskell-mode)
(use-package lsp-haskell
  :init (setq lsp-haskell-server-path (executable-find "haskell-language-server-wrapper")))

(use-package vala-mode)
(add-hook 'vala-mode-hook
          (lambda ()
            ;; how about no
            (kill-local-variable 'indent-tabs-mode)
            (kill-local-variable 'tab-width)
            (kill-local-variable 'c-basic-offset)))
(remove-hook 'vala-mode-hook 'lsp)

(use-package cider
  :config
  (evil-define-key '(normal visual insert) cider-mode-map (kbd "M-?") 'cider-find-var))

(use-package evil-cleverparens
  :hook ((lisp-data-mode . evil-cleverparens-mode)
         (clojure-mode . evil-cleverparens-mode)
         (cider-repl-mode . evil-cleverparens-mode)))

;; Term Mode ;;
;;(use-package multi-vterm
;;  :hook ((vterm-mode . evil-insert-state))
;;  :config
;;  (evil-define-key 'normal vterm-mode-map (kbd "i") #'evil-insert-resume)
;;  (evil-define-key 'insert vterm-mode-map (kbd "ESC") nil)
;;  (evil-define-key 'insert vterm-mode-map (kbd "M-ESC") #'evil-normal-state))
;;
;;(add-to-list 'orphanable-buffer-conditions
;;             '(major-mode . vterm-mode))
;;
;;(add-hook 'vterm-mode-hook
;;          (lambda ()
;;            (let ((process (get-buffer-process (current-buffer))))
;;              (set-process-query-on-exit-flag process nil))))

;; Man Mode ;;
(use-package man
  :demand t
  :hook (man-common . (lambda () (display-line-numbers-mode -1))))

(add-to-list 'orphanable-buffer-conditions
             '(derived-mode . Man-mode))

;; Buffer Menu ;;
(evil-define-key 'motion Buffer-menu-mode-map (kbd "RET") 'Buffer-menu-this-window)
(add-hook 'Buffer-menu-mode-hook 'auto-revert-mode)

;; Backups
(setq backup-directory-alist `((".*" . "~/.emacs.d/backups/"))
      backup-by-copying t
      delete-old-versions t
      version-control t)

;; Lock Files ;;
(let ((lock-files-dir "~/.emacs.d/lock-files/"))
  (setq lock-file-name-transforms `((".*" ,lock-files-dir t)))
  (make-directory lock-files-dir t))

;; Auto-Saves ;;
(let ((auto-saves-dir "~/.emacs.d/auto-saves/"))
  (setq auto-save-file-name-transforms `((".*" ,auto-saves-dir t)))
  (make-directory auto-saves-dir t))

;; Miscellaneous ;;
;(use-package free-keys)

(add-hook 'org-mode-hook 'auto-fill-mode)
(add-hook 'markdown-mode-hook 'auto-fill-mode)

;; strip trailing whitespace on save
(add-hook 'write-file-hooks
          (lambda ()
            (unless (derived-mode-p 'diff-mode)
              (delete-trailing-whitespace))))

;; leaving this on causes emacs to hang on exit
(setq x-select-enable-clipboard-manager nil)

;; Conveniences ;;
(defun xdg-open (file)
  (call-process "xdg-open" nil 0 nil (expand-file-name file)))

(defun open-default-directory-externally ()
  (interactive)
  (xdg-open default-directory))
(global-set-key (kbd "C-x C-j") 'open-default-directory-externally)

(defun open-external-terminal ()
  (interactive)
  (call-process "x-terminal-emulator" nil 0))
(global-set-key (kbd "C-x t") 'open-external-terminal)

(defun launch (command)
  (interactive (list (read-shell-command "Launch: ")))
  (call-process-shell-command command nil 0 nil))

;; Custom ;;
;; TODO: read the colors from from Xresources ;;
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t nil)))
 '(header-line ((t (:background "#000" :foreground "#fff"))))
 '(highlight ((t (:background "#670033"))))
 '(hl-line ((t (:inherit highlight :extend t :background "#670033"))))
 '(mode-line ((t (:background "#000" :foreground "#aaa" :overline "#5a5a5a"))))
 '(mode-line-inactive ((t (:inherit header-line :foreground "#ccc" :overline "#5a5a5a"))))
 '(region ((t (:extend t :background "#c70264"))))
 '(vertical-border ((t (:foreground "#5a5a5a")))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages nil))
