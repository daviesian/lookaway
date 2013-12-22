(ns lookaway.core
  (:use [overtone.live]
        [overtone.studio.scope]
        [clojure.pprint])
  (:require [clojure.java.io :as io]))

(demo (sin-osc))

(def wav-dir (io/file "D:/Reaper Media/Never Look Away/WAV"))

;; ***********************************************
;; * Load file buffers
;; ***********************************************

(def track-files (map #(.getPath %)
                      (sort (filter #(.endsWith (.getName %) ".wav")
                                    (file-seq wav-dir)))))

(def tracks (map load-sample (take 5 track-files)))

(def piano (nth tracks 0))
(def vox (nth tracks 1))


;; ***********************************************
;; * Initialise
;; ***********************************************

(def bpm 142)
(def sr 44100)



(def samples-per-bar
  (let [seconds-per-beat (/ 60 bpm)
        seconds-per-bar (* 4 seconds-per-beat)]
    (* 44100 seconds-per-bar)))


;; ***********************************************
;; * Synths
;; ***********************************************

(def cursor-bus (audio-bus 1 "cursor"))

(defsynth cursor []
  (out cursor-bus (sweep:ar 1 sr)))

(defsynth query-bar [trig-id 0]
  (send-trig:kr (impulse:kr 1) trig-id (a2k (/ (in cursor-bus) samples-per-bar)))
  (free-self (trig 1)))

(defsynth bar-player [buf 0 bar 0 bars 1 loop? 0]
  "Assumes 4/4 time."
  (let [env-shape (envelope [1 1] [(/ (* bars samples-per-bar) sr)])
        env (env-gen env-shape (- 1 loop?) :action FREE)
        p (phasor:ar 0 1 (* bar samples-per-bar) (* (+ bar bars) samples-per-bar) 0)
        s (* env (buf-rd:ar 2 buf p 0 1))]
    (out 0 s)))

(defsynth play-buffer-bars [buf 0 buffer-start-bar 0 bars 1 play-at-bar 0]
  (let [;;cursor-on-init (latch:ar (in cursor-bus) 1)
        play-at-t-samples (* samples-per-bar play-at-bar)
        end-at-t-samples (+ play-at-t-samples (* bars samples-per-bar))

        buffer-start-samples (* buffer-start-bar samples-per-bar)

        p (- (in:ar cursor-bus) play-at-t-samples)

        e-trig (trig:ar p (/ (* bars samples-per-bar) sr))
        env (env-gen:ar (envelope [0.1   1   0.1]
                                  [    1   1    ] :step 1)
                         e-trig
                         :action FREE)
        s (* env (buf-rd:ar 2 buf (+ buffer-start-samples p) 0 1))]
    (send-trig:ar (impulse:ar 5) 220 e-trig)
    ;;(send-trig:kr (- (a2k (in cursor-bus)) play-at-t-samples) 200 (a2k (in cursor-bus)))
    (out 0 s)))

;; ***********************************************
;; * Utils
;; ***********************************************

(defn get-current-bar []
  (let [b (promise)
        tid (trig-id)]
    (on-trigger tid (fn [val]
                      (deliver b val)
                      (remove-event-handler ::bar-number-trigger))
                ::bar-number-trigger)
    (query-bar tid)
    @b))


(defn get-next-bar []
  (int (Math/ceil (get-current-bar))))
;; ***********************************************
;; * Play
;; ***********************************************


(do
  (stop)
  (cursor)
  (play-buffer-bars vox 77 10 0)
  (play-buffer-bars piano 78 10 1)
  )

(stop)

(def phrase-a-bars [9 13 21 25 43 47])

(let [next (get-next-bar)]
  (doseq [lb phrase-a-bars]
    (play-buffer-bars vox lb 3.3 next)))

(play-buffer-bars vox 16.3 4.7 (+ (get-next-bar) 0.3))


(let [next (get-next-bar)]
  (stop)
  (cursor)
  (doseq [t tracks]
    (play-buffer-bars t 0 50 next)))

(on-event "/tr" (bound-fn [%] (println "trigger: " (:args %))) ::trigger-test)
(remove-event-handler ::trigger-test)
