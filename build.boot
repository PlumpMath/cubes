(set-env!
 :source-paths    #{"src/cljs" "src/clj"}
 :resource-paths  #{"resources"}
 :dependencies '[[adzerk/boot-cljs          "1.7.170-3"   :scope "test"]
                 [adzerk/boot-cljs-repl     "0.2.0"      :scope "test"]
                 [adzerk/boot-reload        "0.4.1"      :scope "test"]
                 [pandeiro/boot-http        "0.6.3"      :scope "test"]
                 [crisptrutski/boot-cljs-test "0.2.1-SNAPSHOT" :scope "test"]
                 [cljsjs/react-with-addons "0.14.3-0"]
                 [cljs-react-test "0.1.3-SNAPSHOT" :scope "test"]
                 [org.clojure/clojurescript "1.7.170"]
                 [org.omcljs/om "1.0.0-alpha22" :exclusions [cljsjs/react]]
                 [datascript "0.13.3"]
                 [quil "2.3.0"]])

(require
 '[adzerk.boot-cljs      :refer [cljs]]
 '[adzerk.boot-cljs-repl :refer [cljs-repl start-repl]]
 '[adzerk.boot-reload    :refer [reload]]
 '[pandeiro.boot-http    :refer [serve]]
 '[crisptrutski.boot-cljs-test :refer [test-cljs]])

(deftask build []
  (comp (speak)

        (cljs)
        ))

(deftask run []
  (comp (serve)
        (watch)
        (cljs-repl)
        (reload)
        (build)))

(deftask testing []
  (set-env! :source-paths #(conj % "test/cljs"))
  identity)

;; remove warning
(ns-unmap 'boot.user 'test)

(deftask test []
  (comp (testing)
        (test-cljs :js-env :phantom
                   :exit? true)))

(deftask auto-test []
  (comp (testing)
        (watch)
        (test-cljs :js-env :phantom)))

(deftask production []
  (task-options! cljs {:optimizations :advanced})
  identity)

(deftask development []
  (task-options! cljs {:optimizations :none :source-map true}
                 reload {:on-jsload 'cubes.app/init})
  identity)

(deftask dev
  "Simple alias to run application in development mode"
  []
  (comp (development)
        (run)))
