(defproject dragonmark/web "0.1.3"
  :description "Tools for Clojure web project"
  :url "https://github.com/dragonmark/web"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :jvm-opts ^:replace ["-Xms768m" "-Xmx768m" "-server"]

  :dependencies [[org.clojure/clojure "1.7.0-RC1"]
                 [dragonmark/util "0.1.1"  :exclusions [org.clojure/clojure]]
                 [org.clojure/clojurescript "0.0-3291"]
                 [domina "1.0.3"]
                 [environ "1.0.0"]]

  :plugins [[codox "0.8.10"  :exclusions [org.clojure/clojure]]
            [com.cemerick/austin "0.1.6"  :exclusions [org.clojure/clojure]]
            [lein-cljsbuild "1.0.6"]
            [lein-figwheel "0.3.3"]
            [com.cemerick/clojurescript.test "0.3.3"  :exclusions [org.clojure/clojure]]]


  :codox {:defaults {:doc/format :markdown}
          :sources ["target/generated/src"]
          :output-dir "doc/codox"
          :src-linenum-anchor-prefix "L"
          :src-uri-mapping {#"target/generated/src" #(str "src/" % "x")}}

  :jar-exclusions [#"\.cljx|\.swp|\.swo|\.DS_Store|\.props"]

  :source-paths ["src"]
  :test-paths   ["test"]

  :clean-targets ^{:protect false} ["resources/public/js/compiled" "target"
                                    "resources/test/compiled.js"]

  :cljsbuild 
  {

   :builds 
   [{:id "dev"
     :source-paths ["src" "test"]

     :figwheel true
     
     :compiler {:main dragonmark.web.core
                :asset-path "js/compiled/out"
                :output-to "resources/public/js/compiled/dm.js"
                :output-dir "resources/public/js/compiled/out"
                :source-map-timestamp true }}
    {:id "min"
     :source-paths ["src"]
     :compiler {:main dragonmark.web.core
                :output-to "resources/public/js/compiled/dm.js"
                :optimizations :advanced
                :pretty-print false}}

    {:id "test"
     :source-paths ["src" "test"]
     :compiler {:output-to "resources/test/compiled.js"
                :optimizations :whitespace
                :pretty-print true}}]
   :test-commands
   {"test" ["phantomjs"
            "resources/test/test.js"
            "resources/test/test.html"]}}
  
  :figwheel {
             :http-server-root "public" ;; default and assumes "resources"
             :server-port 3449 ;; default
             :css-dirs ["resources/public/css"] ;; watch and update CSS

             ;; Start an nREPL server into the running figwheel process
             :nrepl-port 7888

             ;; Server Ring Handler (optional)
             ;; if you want to embed a ring handler into the figwheel http-kit
             ;; server, this is for simple ring servers, if this
             ;; doesn't work for you just run your own server :)
             ;;:ring-handler dragonmark.figwheel-server/handler

             ;; To be able to open files in your editor from the heads up display
             ;; you will need to put a script on your path.
             ;; that script will have to take a file path and a line number
             ;; ie. in  ~/bin/myfile-opener
             ;; #! /bin/sh
             ;; emacsclient -n +$2 $1
             ;;
             ;; :open-file-command "myfile-opener"

             ;; if you want to disable the REPL
             ;; :repl false

             ;; to configure a different figwheel logfile path
             ;; :server-logfile "tmp/logs/figwheel-logfile.log" 
             }

  :aliases
  {"test-cljs" ["do" ["clean"] ["cljsbuild" "once"] ["cljsbuild" "test"]]
   }


)
