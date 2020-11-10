;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")

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
;; (setq doom-font (font-spec :family "monospace" :size 28 :weight 'semi-light)
;;      doom-big-font (font-spec :family "monospace" :size 56 :weight 'semi-light)
;;      doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)


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


(use-package! aggressive-indent
  :hook
  (clojure-mode . aggressive-indent-mode)
  (lisp-mode . aggressive-indent-mode))

(use-package! ivy-cider
  :after cider-mode)

(use-package! flycheck-clj-kondo
  :after clojure-mode
  :config
  ;; (dolist (checkers '((clj-kondo-clj . clojure-joker)
  ;;                   (clj-kondo-cljs . clojurescript-joker)
  ;;                   (clj-kondo-cljc . clojure-joker)
  ;;                   (clj-kondo-edn . edn-joker)))
  ;; (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers)))))
  )

(after! clojure-mode
  (define-clojure-indent
    (PUT 2)
    (POST 2)
    (GET 2)
    (PATCH 2)
    (DELETE 2)
    (context 2)
    (for-all 2)
    (checking 3)
    (>defn :defn)
    (>defn- :defn)
    (match 1)
    (cond 0)
    (case 1)
    (describe 1)
    (it 2)
    (fn-traced :defn)
    (defn-traced :defn)
    (assert-match 1))
  (add-to-list 'clojure-align-binding-forms "let-flow")
  (setq clojure-indent-style 'align-arguments)
  (setq cider-default-cljs-repl 'shadow)
  (put '>defn 'clojure-doc-string-elt 2)
  (put '>defn- 'clojure-doc-string-elt 2)
  (put 'defsys 'clojure-doc-string-elt 2)
  (put 'defn-traced 'clojure-doc-string-elt 2)



  (setq clojure-align-forms-automatically t)
  (setq cider-cljs-lein-repl
        "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")
  (setq cljr-magic-require-namespaces
        '(("io" . "clojure.java.io")
          ("sh" . "clojure.java.shell")
          ("jdbc" . "clojure.java.jdbc")
          ("set" . "clojure.set")
          ("time" . "java-time")
          ("str" . "cuerdas.core")
          ("path" . "pathetic.core")
          ("walk" . "clojure.walk")
          ("zip" . "clojure.zip")
          ("async" . "clojure.core.async")
          ("component" . "com.stuartsierra.component")
          ("http" . "clj-http.client")
          ("url" . "cemerick.url")
          ("sql" . "honeysql.core")
          ("csv" . "clojure.data.csv")
          ("json" . "cheshire.core")
          ("s" . "clojure.spec.alpha")
          ("fs" . "me.raynes.fs")
          ("ig" . "integrant.core")
          ("cp" . "com.climate.claypoole")
          ("re-frame" . "re-frame.core")
          ("rf" . "re-frame.core")
          ("rf.db" . "re-frame.db")
          ("re" . "reagent.core")
          ("reagent" . "reagent.core")
          ("w" . "wing.core")
          ("gen" . "clojure.spec.gen.alpha"))))
