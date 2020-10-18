(ns ferossgp.coord
  (:gen-class)
  (:require [clojure.java.io :as io]
            [clojure.string :as cstr]
            [cognitect.transit :as transit]
            [org.httpkit.server :as server])
  (:import java.util.Arrays))

(def grid-size 700)
(def file (io/resource "input-data.txt"))
(def index-html (io/resource "public/index.html"))

(def state (atom {:ready false}))

;; NOTE: Not used in real code as it is mutable and could be a source of errors
;; but, also this allows us to keep low memory and work faster for big matrices
(def grid (make-array Boolean/TYPE grid-size grid-size))

(defn fill!
  "Assigns the specified byte value to each element
  of the specified array of bytes."
  ([input val]
   (Arrays/fill input (boolean val)))
  ([input start end val]
   (Arrays/fill input (int start) (int end) (boolean val))))

(defn count-array [array]
  (count (filter true? array)))

(defn activate [state from to]
  (doseq [r (range (first from) (inc (first to)))]
    (fill! (aget state r) (second from) (second to) true)))

(defn deactivate [state from to]
  (doseq [r (range (first from) (inc (first to)))]
    (fill! (aget state r) (second from) (second to) false)))

(defn toggle [state from to]
  (doseq [r (range (first from) (inc (first to)))
          c (range (second from) (inc (second to)))]
    (aset-boolean state r c (not (aget state r c)))))

(defn process-command! [state {:keys [command from to]}]
  (cond
    (= command "activate")   (activate state from to)
    (= command "deactivate") (deactivate state from to)
    (= command "toggle")     (toggle state from to)))

(defn parse-number [s]
  (when (re-find #"^-?\d+\.?\d*$" s) (read-string s)))

(defn ->coordinates [input]
  (mapv parse-number (cstr/split input #",")))

(defn line->command [line]
  (let [[command from _ to] (cstr/split line #" ")]
    {:command command
     :from    (->coordinates from)
     :to      (->coordinates to)}))

(defn process-lines!
  [file]
  (with-open [rdr (io/reader file)]
    (mapv line->command (line-seq rdr))))

(defn process-file! []
  (try
    (doseq [command (process-lines! file)]
      (process-command! grid command))
    (swap! state assoc :ready true)
    (catch Exception e
      (prn e))))

(defn task-1 []
  (try
    (process-file!)
    (reduce (fn [acc el]
              (+ acc (count-array el)))
            0
            grid)
    (catch Exception _ 0)))

(defn serialize-grid []
  (reduce (fn [acc row]
            (into acc (map #(if % 1 0) row)))
          '()
          grid))

(defn generate-transit [o]
  (let [bos (java.io.ByteArrayOutputStream. 1024)
        writer (transit/writer bos :json)]
    (transit/write writer o)
    (String. (.toByteArray bos) "UTF-8")))

(defn get-coordinates []
  (try
    (if-not (:ready @state)
      {:status  200
       :headers {"Content-Type" "application/transit+json"}
       :body    (generate-transit {:status "not ready"})}
      {:status  200
       :headers {"Content-Type" "application/transit+json"}
       :body    (generate-transit (serialize-grid))})
    (catch Exception e
      {:status  500
       :headers {"Content-Type" "application/transit+json"}
       :body    (generate-transit {:error (.getMessage e)})})))

(defn serve-index []
  {:status 200
   :header {"Content-Type" "text/html"}
   :body   (slurp index-html)})

(defn handler [{:keys [request-method uri]}]
  (cond
    (and (= request-method :get)
           (= uri "/coordinates"))
    (get-coordinates)

    (and (= request-method :get)
         (= uri "/"))
    (serve-index)

    :else
    {:status 404
     :body   "Not found"}))

(defn -main
  [& _args]
  (.start (Thread. process-file!))
  (server/run-server handler {:port 8090})
  (println "Server running on http://localhost:8090"))
