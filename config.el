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


;; org-static-blog
;; I've forked the repo and copied that into my doom.d
;; https://github.com/jgoodhcg/org-static-blog/blob/master/org-static-blog.el
(require 'org-static-blog "~/.doom.d/custom/org-static-blog.el")

(setq org-static-blog-publish-title "My Static Org Blog")
(setq org-static-blog-publish-url "https://jgoodhcg.github.io/org-blog")
(setq org-static-blog-publish-directory "~/projects/org-blog/")
(setq org-static-blog-posts-directory "~/projects/org-blog/posts/")
(setq org-static-blog-drafts-directory "~/projects/org-blog/drafts/")
(setq org-static-blog-enable-tags t)
(setq org-export-with-toc nil)
(setq org-export-with-section-numbers nil)

;;   (you will need to create the style sheet at
;;    ~/projects/blog/static/style.css
;;    and the favicon at
;;    ~/projects/blog/static/favicon.ico)

;; This header is inserted into the <head> section of every page:
(setq org-static-blog-page-header
      "<meta name=\"author\" content=\"Justin\">
<meta name=\"referrer\" content=\"no-referrer\">
<meta name=\"viewport\" content=\"width=device-width, initial-scale=1, shrink-to-fit=no\">
<link href=\"static/css/output.css\" rel=\"stylesheet\" type=\"text/css\" />
<link rel=\"icon\" href=\"static/favicon.ico\">")

;; This preamble is inserted at the beginning of the <body> of every page:
(setq org-static-blog-page-preamble
      "<div class=\"container mx-auto px-4 py-4\">
         <div class=\"header text-3xl font-semibold mb-8\">
           <a href=\"https://jgoodhcg.github.io/org-blog\" class=\"text-black no-underline hover:text-blue-500\">My Static Org Blog</a>
         </div>")

;; This postamble is inserted at the end of the <body> of every page:
(setq org-static-blog-page-postamble
      "<div id=\"archive\" class=\"mt-8\">
         <a href=\"https://jgoodhcg.github.io/org-blog/archive.html\" class=\"text-black no-underline hover:text-blue-500\">Other posts</a>
       </div>
       </div>")

;; This HTML code is inserted into the index page between the preamble and
;;   the blog posts
(setq org-static-blog-index-front-matter
      "<h1 class=\"text-4xl font-bold underline mb-6\"> Welcome to my blog </h1>\n")
