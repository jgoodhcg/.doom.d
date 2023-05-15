;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
;;
;;(setq doom-font (font-spec :family "Fira Code" :size 24 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "Fira Sans" :size 26))

;; For some reason `doom-variable-pitch-font' breaks doom on mac
(setq doom-font     (font-spec :family "Fira Code" :size 24 :weight 'regular)
      doom-big-font (font-spec :family "Fira code" :size 48 :weight 'regular))


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
(setq display-line-numbers-type t)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
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

;; https://stackoverflow.com/a/2417617/5040125
(defun my-put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(use-package! flycheck-clj-kondo
  :after clojure-mode
  :ensure t)

;; ;; clay https://scicloj.github.io/clay/#Emacs%20CIDER
;; ;; (inspired by: https://github.com/clojure-emacs/cider/issues/3094)
;; (require 'cider-mode)

;; (defun cider-tap (&rest r) ; inspired by https://github.com/clojure-emacs/cider/issues/3094
;;   (cons (concat "(let [__value "
;;                 (caar r)
;;                 "] (tap> {:clay-tap? true :form (quote " (caar r) ") :value __value}) __value)")
;;         (cdar r)))

;; (advice-add 'cider-nrepl-request:eval
;; :filter-args #'cider-tap)

;; accept completion from copilot and fallback to company
(use-package! copilot
  :hook (prog-mode . copilot-mode)
  :bind (("C-TAB" . 'copilot-accept-completion-by-word)
         ("C-<tab>" . 'copilot-accept-completion-by-word)
         :map copilot-completion-map
         ("<tab>" . 'copilot-accept-completion)
         ("TAB" . 'copilot-accept-completion)))

(setq copilot-idle-delay 2)


;; from chatgpt4
(after! clojure-mode
  (map! :map clojure-mode-map
        :localleader
        :desc "Reset" "r" #'kit-reset))

(defun kit-reset ()
  (interactive)
  (save-buffer) ; Save the current buffer
  (cider-interactive-eval
   "(require 'integrant.repl) (if (resolve 'integrant.repl/reset) (integrant.repl/reset) (println \"No integrant.repl/reset found.\"))"))

;; sudo apt-get install librsvg2-bin
;; image-toggle-display to render the source
(add-to-list 'auto-mode-alist '("\\.svg\\'" . image-mode))

;; chatgpt
(defun insert-current-date ()
  "Insert the current date in the format 'YYYY-MM-DD'."
  (interactive)
  (let ((current-date (format-time-string "%Y-%m-%d Justin - ")))
    (insert current-date)))

(require 'gptai)
(require 'gptai-turbo)
;; set configurations
(setq gptai-model "text-davinci-003")
;; Load secrets file
(load-file (expand-file-name "~/.doom.d/gptai-secrets.el"))
;; set keybindings optionally
;; (global-set-key (kbd "C-c o") 'gptai-send-query)

;;;###autoload
(defun gptai-turbo-query-buffer ()
  "Sends a request to gpt-3.5-turbo using the selected buffer contents and appends the response in the same buffer under a new headline."
  (interactive)
  (let* ((source-buffer (read-buffer "Select a buffer for the API text prompt: " (buffer-name (current-buffer)) t))
         (gptai-prompt (with-current-buffer source-buffer (buffer-string)))
         (response (gptai-turbo-request gptai-prompt)))
    (with-current-buffer source-buffer
      (goto-char (point-max))
      (insert "\n")
      (insert response))))

(defvar chatgpt-org-dir-default "~/projects/chatgpt-org/"
  "Default directory for new ChatGPT org files.")

(defvar chatgpt-template-file-default "~/projects/chatgpt-org/template.org"
  "Default template file for new ChatGPT org files.")

(defun create-chatgpt-org-file ()
  (interactive)
  (let* ((title (read-string "Enter title: "))
         (date (format-time-string "%Y-%m-%d"))
         (filename (concat date "-" title ".org"))
         (directory (read-directory-name "Select a directory: " chatgpt-org-dir-default))
         (template-file (read-file-name "Select a template file: " chatgpt-template-file-default))
         (path (expand-file-name filename directory)))
    (find-file path)
    (if (file-exists-p template-file)
        (progn
          (insert-file-contents template-file)
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TITLE:.*$" nil t)
            (replace-match (concat "#+TITLE: " title))))
      (insert (concat "#+TITLE: " title "\n")))
    (save-buffer)))
