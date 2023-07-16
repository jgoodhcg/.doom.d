#!/usr/bin/env bb

;; This is just a stub for  wip org mode ui over openai api
(defn print-args
  [arg1 arg2]
  (println "The first argument is: " arg1)
  (println "The second argument is: " arg2))

(def args (vec *command-line-args*))

#_#_(when (< (count args) 2)
  (println "Please provide at least two arguments.")
  (System/exit 1))

(print-args (nth args 0) (nth args 1))

(println "hello")
