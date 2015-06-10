(ns  ^:figwheel-always dragonmark.web.runner
  (:require [cljs.test :as t]))

(enable-console-print!)

(defn ^:export run
  []
  (t/run-tests 'dragonmark.web.core-test)
  )
