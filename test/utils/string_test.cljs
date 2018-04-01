(ns utils.string-test
  (:require
   [pjstadig.humane-test-output]
   [cljs.test :as ct :refer-macros [deftest testing is] :include-macros true]
   [utils.string :as us]))

(deftest kebab-case
  (is (= "hello-world" (us/kebab-case "!__hello_world__?")))
  (is (= "hello-world" (us/kebab-case "!__hello_world__!")))
  (is (= "hel-lo-world" (us/kebab-case "__hel!lo_world___")))
  (is (= "h-ello-world?" (us/kebab-case "__h!ello_world__?" :keep-pred? true)))
  (is (= "hello-world!" (us/kebab-case "!__hello_world__!" :keep-pred? true)))
  (is (= "hello-world" (us/kebab-case "__hello_world__!_" :keep-pred? true))))

(deftest start-case
  (is (= "Hello World" (us/start-case "!__hello_world__?")))
  (is (= "Hello World" (us/start-case "!__hello_world__!")))
  (is (= "Hel Lo World" (us/start-case "__hel!lo_world___")))
  (is (= "H Ello World?" (us/start-case "__h!ello_world__?" :keep-pred? true)))
  (is (= "Hello World!" (us/start-case "!__hello_world__!" :keep-pred? true)))
  (is (= "Hello World" (us/start-case "__hello_world__!_" :keep-pred? true))))

(deftest sentence-case
  (is (= "Hello world" (us/sentence-case "!__hello_world__?")))
  (is (= "Hello world" (us/sentence-case "!__hello_world__!")))
  (is (= "Hel lo world" (us/sentence-case "__hel!lo_world___")))
  (is (= "H ello world?" (us/sentence-case "__h!ello_world__?" :keep-pred? true)))
  (is (= "Hello world!" (us/sentence-case "!__hello_world__!" :keep-pred? true)))
  (is (= "Hello world" (us/sentence-case "__hello_world__!_" :keep-pred? true))))

(deftest walk-keys)
;; tested with kebab-case-keys and start-case-keys

(deftest kebab-case-keys
  (is (= {:hello-world? true
          :a-coll ["a" "b"]
          [] {:nested-map-key! 1
              "nested-map-key-2" true}}
         (us/kebab-case-keys
          {:hello--world-? true
           'a-coll- ["a" "b"]
           [] {:nested-map-key! 1
               "nested-map-key2" true}}
          :keep-pred? true))))

(deftest start-case-keys
  (is (= {(keyword "Hello World?") true
          (keyword "A Coll") ["a" "b"]
          [] {(keyword "Nested Map Key!") 1
              "Nested Map Key 2" true}}
         (us/start-case-keys
          {:hello--world-? true
           'a-coll- ["a" "b"]
           [] {:nested-map-key! 1
               "nested-map-key2" true}}
          :keep-pred? true))))

(deftest paramify
  (is (= {:helloWorld true
          :aColl ["a" "b"]
          [] {:nestedMapKey 1
              :nestedMapKey2 true}}
         (us/paramify
          {:hello--world-? true
           'a-coll- ["a" "b"]
           [] {:nested-map-key! 1
               "nested-map-key2" true}})))
  (is (= {:HelloWorld true
          :AColl ["a" "b"]
          [] {:NestedMapKey 1
              :NestedMapKey2 true}}
         (us/paramify
          {:hello--world-? true
           'a-coll- ["a" "b"]
           [] {:nested-map-key! 1
               "nested-map-key2" true}}
          :first-upper? true))))
