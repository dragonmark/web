(ns dragonmark.web.mac
  (:require [environ.core :refer [env]]
            [clojure.java.io :as io])
  )

(defn dev? [] (env :dev))

(def template-dir (atom "/templates/"))

(def loading-text (atom "<div>loading</div>"))

(def server-template-path (atom "/_templates/"))

(defn load-template
  "Load a template from the /resources/templates directory"
  [name]
  (-> (.getResource (.getClass "") (str @template-dir name)) io/as-file slurp))

(defmacro set-template-dir
  "This is a macro that sets the template dir... it's
  called from cljs code, but changes the atom in Clojure/compile-time-land"
  [dir]
  (reset! template-dir dir)
  `[~dir])

(defmacro set-loading-text
  "This is a macro that sets the default loading text... it's
  called from cljs code, but changes the atom in Clojure/compile-time-land"
  [text]
  (reset! loading-text text)
  `[~text])

(defmacro set-server-template-path
  "This is a macro that sets the path prefix for server template loading... it's
  called from cljs code, but changes the atom in Clojure/compile-time-land"
  [path]
  (reset! server-template-path path)
  `[~path])

(defmacro set-template-dir
  "This is a macro that sets the template dir... it's
  called from cljs code, but changes the atom in Clojure/compile-time-land"
  [dir]
  (reset! template-dir dir)
  `[~dir])

(defmacro insert-template
  "This is used from ClojureScript to insert a template file
  In dev mode, the file is loaded via a synchronous Ajax call
  in non-dev mode, the file is loaded at compile time and inserted
  right into the code. Useful for templates"
  [name]
  (if (dev?)
    `(let [data# (reagent.core/atom ~(deref loading-text))
           cnt# (reagent.core/atom 0)]
       (letfn [(funcy#
                 []
                 (do
                   (when (and
                           (= 0 @cnt#)
                           (< 0 (count (.-watches data#))))
                     (let [req# (js/XMLHttpRequest.)]
                       (swap! cnt# inc)
                       (.open req# "GET" (str ~(deref server-template-path)
                                              ~name 
                                              "?param="
                                              (dragonmark.util.core/next-guid)))
                       (.send req# nil)
                       (set!
                         (.-onreadystatechange req#)
                         (fn []
                           (when (= 4 (.-readyState req#))
                             (swap! cnt# dec)
                             (let [text# (.-responseText req#)]
                               (when (not= text# @data#)
                                 (reset! data# text#))))

                           ))))
                   (js/setTimeout funcy# 3000))
                 )]
         (js/setTimeout funcy# 20)
         data#
         ))
    (load-template name)
    ))
