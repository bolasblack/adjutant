(def +project+ 'c4605/utils)
(def +version+ "0.1.0")
(def +description+ "cljs normal utils")

(set-env!
 :resource-paths #{"src"}
 :source-paths #{}
 :dependencies '[[org.clojure/clojurescript "1.9.946" :scope "test"]
                 [org.clojure/clojure "1.8.0" :scope "test"]
                 [adzerk/boot-cljs "1.7.228-2" :scope "test"]
                 [org.clojure/core.async "0.3.442"]
                 [funcool/cats "2.1.0"]
                 [honeysql "0.9.1"]])

(require '[adzerk.boot-cljs :refer [cljs]])

(task-options!
 pom {:project +project+
      :version +version+
      :description +description+
      :license {"MIT" "http://opensource.org/licenses/MIT"}
      :scm {:url "https://github.com/bolasblack/cljs.utils"}})

(deftask build-cljs []
  (cljs :compiler-options {:target :nodejs
                           :optimizations :none
                           :source-map true}))

(deftask build-jar []
  (comp
   (pom)
   (jar)
   (install)
   (target)))

(deftask deploy []
  (set-env!
   :repositories #(-> %
                      (conj ["clojars" {:url "https://clojars.org/repo/"}])
                      (conj ["jfrog" {:url "https://jfrog.c4605.com/artifactory/clj/"
                                      :username (get-sys-env "JFROG_USER" :required)
                                      :password (get-sys-env "JFROG_PASS" :required)}])))
  (comp
   (build-jar)
   (push :repo "jfrog")))
