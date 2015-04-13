(ns markov-clj.twitter-facade
  (:require [environ.core :refer [env]]
            [twitter.api.restful :as twitter]
            [twitter.oauth :as  twitter-oauth]))

(def my-creds (twitter-oauth/make-oauth-creds (env :app-consumer-key)
                                               (env :app-consumer-secret)
                                               (env :user-access-token)
                                               (env :user-access-secret)))

(defn status-update [status]
    (when (not-empty status)
      (try (twitter/statuses-update :oauth-creds my-creds
                                    :params {:status status})
        (catch Exception e (println "Oh no! " (.getMessage e))))))