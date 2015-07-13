(ns ^:figwheel-always dragonmark.web.core-test
  (:require
    [dragonmark.web.core :as dw :refer [xf xform]]
    [cljs.test :as t])
  (:require-macros [cljs.test :as t
                    :refer (is deftest run-tests testing)]
                   [dragonmark.web.mac :as mac]))

(enable-console-print!)

(def html [:div [:span {:id "dog"} "cat"]])

(def h2 "<div><!-- I'm a comment --><span id=\"dog\">cat</span></div>")

(def h3 (atom h2))

(def inputs [html h2 h3])

(deftest
  passthru
  "Tests a simple passthrough with no changes"
  (t/is (= html (xform html)))
  (t/is (= html (xform h2)))
  (t/is (= html (xform h3))))

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
  (doseq [html inputs]
    (let [r (xform html ["span" [:div {:on-click (fn [x] (+ x 1))} "moo"]])
          func? (-> r second second :on-click)]
      (t/is (= 2 (func? 1))))))

(deftest
  func-passed
  "Functions get passed"
  (doseq [html inputs]
    (let [r (xform html ["span" {:on-click (fn [x] (+ x 1))}])
          func? (-> r second second :on-click)]
      (t/is (= 2 (func? 1))))))

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
  (doseq [html inputs]
    (let [data ["foo" "bar" "baz"]
          r (xform html ["span" (map #(comp (xf "." {:id %})
                                            (xf "." :*> %)) data)])
          ids (->> r rest (map second) (map :id))
          content (->> r rest (map last))]
      (t/is (= 4 (-> r second count)))
      (t/is (= (vec content) data))
      (t/is (= (vec ids) data)))))

(defn funky-stuff
  []
  [:div "I'm a late bound funky thing"])

(deftest
  funky-kid
  "Insert a child with a function"

  (doseq [html inputs]
    (let [r (xform html ["span" [funky-stuff]])]
      (is (= funky-stuff (-> r second first)))
      ))
  )


(deftest
  extra
  "extra tests"

  (t/is (= ((xf "." :*> "moo") [:div "the cow sez"])
           [:div "the cow sez" "moo"]))
  (t/is (= ((xf "." :*> "moo") [:div [:span] "the cow sez"])
           [:div [:span] "the cow sez" "moo"]))
  (t/is (= ((comp (xf "." :*> "moo") (xf "span" "Span be gone")) [:div [:span] "the cow sez"])
           [:div "Span be gone" "the cow sez" "moo"]))
  (t/is (= ((comp (xf "." :*> "moo") (xf "span" {:class "foo"})) [:div [:span] "the cow sez"])
           [:div [:span {:class " foo "}] "the cow sez" "moo"]))
  (t/is (= ((comp (xf "." :*> "moo") (xf "span" {:class> "foo"})) [:div [:span {:class "bar"}] "the cow sez"])
           [:div [:span {:class "bar foo "}] "the cow sez" "moo"]))
  (t/is (= ((comp (xf "." :*> "moo") (xf "span" {:class-- "bar"})) [:div [:span {:class "bar"}] "the cow sez"])
           [:div [:span {:class ""}] "the cow sez" "moo"]))
  )
