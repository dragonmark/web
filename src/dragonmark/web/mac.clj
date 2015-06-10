(ns dragonmark.web.mac
  (:require [environ.core :refer [env]]
            [clojure.java.io :as io])
  )

(defn dev? [] (env :dev))

(defn load-template
  "Load a template from the /resources/templates directory"
  [name]
  (-> (.getResource (.getClass "") (str "/templates/" name)) io/as-file slurp))

(defmacro insert-template
  "This is used from ClojureScript to insert a template file
  In dev mode, the file is loaded via a synchronous Ajax call
  in non-dev mode, the file is loaded at compile time and inserted
  right into the code. Useful for templates"
  [name]
  (if (dev?)
    `(let [data# (reagent.core/atom "<div>loading</div>")
           cnt# (reagent.core/atom 0)]
       (letfn [(funcy#
                 []
                 (do
                   (when (and
                           (= 0 @cnt#)
                           (< 0 (count (.-watches data#))))
                     (let [req# (js/XMLHttpRequest.)]
                       (swap! cnt# inc)
                       (.open req# "GET" (str "/_templates/" ~name "?param=" (dragonmark.util.core/next-guid)))
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
