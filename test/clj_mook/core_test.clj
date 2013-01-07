(ns clj-mook.core-test
  (:use clojure.test
        clj-mook.core))

(deftest simple-test 
  (let [result (-> (session-for-root "http://www.google.com")
                   (session-get "/#q=clj-mook"))]
    (is (not (empty? (:body  (last-response result))))))
  )