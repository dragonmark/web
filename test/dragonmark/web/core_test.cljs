(ns  ^:figwheel-always dragonmark.web.core-test
  (:require
   [dragonmark.web.core :as dw :refer [xf xform]]
   [cljs.test :as t])
  (:require-macros [cljs.test :as t
                    :refer (is deftest run-tests testing)]
                   [dragonmark.web.mac :as mac]))

(enable-console-print!)

(def html [:div [:span {:id "dog"} "cat"]])

(deftest
  passthru
  "Tests a simple passthrough with no changes"
  (t/is (= html (xform html))))

(deftest
  replacement
  "Replace an element with a String"
  (t/is (= [:div "woof"]
           (xform html ["span" "woof"]))))

(deftest
  replacement-hiccup
  "replace the element with a hiccup template"
  (t/is (= [:div [:div "moo"]]

           (xform html ["span" [:div "moo"]]))))


(deftest
  replacement-hiccup2
  "replace the element with a hiccup template"
  (let [r (xform html ["span" [:div {:onclick (fn [x] (+ x 1))} "moo"]])
        func? (-> r second second :on-click)]
    (t/is (= 2 (func? 1)))))

(deftest
  many
  "replace one with many"
  (let [r (xform html ["span" :* ["foo" "bar" "baz"]])
        cnt (-> r count)]
    (t/is (= cnt 4))))

(deftest
  many-func
  "Update the span with new new ids"
  (let [data ["foo" "bar" "baz"]
        r (xform html ["span" (map #(xf "." {:id %}) data)])
        ids (->> r rest (map second) (map :id))]
    (t/is (= (vec ids) data))))

(deftest
  many-func2
  "Update the span with new new ids and new content"
  (let [data ["foo" "bar" "baz"]
        r (xform html ["span" (map #(comp (xf "." {:id %})
                                          (xf "." :*> %)) data)])
        ids (->> r rest (map second) (map :id))
        content (->> r rest (map last))]
    (println (pr-str r))
    (t/is (= 4 (-> r second count)))
    (t/is (= (vec content) data))
    (t/is (= (vec ids) data))))