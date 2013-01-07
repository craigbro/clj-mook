(ns clj-mook.core
  (:require [clj-http.client :as client])
  (:import [org.jsoup Jsoup]))

(defn ensoup
  "Parse an HTML document or fragment, returning a JSoup document"
  [body]
  (Jsoup/parse body))

(defn select-elems [soup selector]
  (.select soup selector))

(defn select-first [soup selector]
  (.first (select-elems soup selector)))

(defn get-attr [elem attr]
  (.attr elem attr))

(defn get-html [elem attr]
  (.text elem attr))

(defn normalize-url [session url]
  (str (java.net.URL. (:root-url session) url)))

;; a session map consists of
(defn session-for-root [rooturl]
  {:root-url (java.net.URL. rooturl)
   :current-url (java.net.URL. (java.net.URL. rooturl) "/")
   :last-response {}
   :last-request {}
   :request-defaults {}})

(defn last-request [session]
  (:last-request session))

(defn last-response [session]
  (:last-response session))

(defn preserve-cookies [session]
  (let [oldcookies (:cookies (:request-defaults session))]
    (assoc session :request-defaults
           (assoc (:request-defaults session)
             :cookies (merge (:cookies (:last-response session))
                             oldcookies)))))

(defn session-request  [session & [reqs]]
  (let [req  (merge (:request-defaults session)
                    reqs)]
    (assoc session
      :last-request req
      :last-response (client/request req))))
  
(defn session-get  [session url & [reqs]]
  (session-request session
                   (merge {:url (normalize-url session url)
                           :method :get}
                          reqs)))

(defn session-post  [session url & [reqs]]
  (session-request session
                   (merge {:url (normalize-url session url)
                           :method :post}
                          reqs)))

;; (defn is-success [session & msg]
;;   (is (= 200 (:status (:last-response session)))
;;       (or msg "The last response was not a success"))
;;   session)


(defmacro session-set [session k & body]
  `(let [s# ~session]
     (assoc s#
       ~k (-> s#
              ~@body))))

(defmacro session-set-fn [session k fn]
  `(let [s# ~session]
     (assoc s#
       ~k (~fn s#))))


;; Face specific login helper
(defn session-login [session login password]
  (-> session
      ;; hit the login page
      (session-get "/login")
      ;; save our cookie!
      (preserve-cookies)
      ;; capture the csrf token
      (session-set :csrf-token
          (last-response)
        :body
        (ensoup)
        (select-first "#_csrf-token")
        (get-attr "value"))
      ;; now post to login to authenticate that session
      (->>
       ((fn [s]
          (session-post s "/login"
                        {:multipart
                         {"_csrf-token"
                          (:csrf-token s)
                          "login" login
                          "password" password}
                         }))))))
