(ns t-animations.core
  (:require
   [t-animations.generator :refer [generate-out-css]]
   [t-animations.utils :refer [print-colored-text]]
   [shadow.cljs.npm.util :as util]
   [clojure.string :refer [replace starts-with?]]))

(defonce my-set #{"anim-" "hover:anim-"})

(def fs (js/require "fs"))

(defn _starts-with? [s substr]
  (starts-with? s substr))

(defn any-starts-with? [s]
  (some #(_starts-with? s %) my-set))

(defn print-and-return-invalid-input-file [file-src]
  (print-colored-text :red "⚠️ ⚠️ ⚠️  Invalid :input argument!!")
  (print-colored-text :red (str "The file (" file-src ") specified for :input doesn't exist."))
  (print-colored-text :red "Try again with an existing input file. (e.g. public/index.html)")
  (prn)
  nil)

(defn parse-file-to-str [file-src]
  (if (.existsSync fs file-src)
    (util/slurp file-src)
    (print-and-return-invalid-input-file file-src)))

(defn spaces-tokenization [input-str]
  (if input-str
    (js->clj (.split input-str " "))
    nil))

(defn filter-valid-tokens [input-array]
  (filter #(any-starts-with? %) input-array))

(defn treat-raw-str [input-str]
  (if input-str
    (-> input-str
        (replace  #"\"" "")
        (replace  #"\n" " ")
        (replace  #">" " ")
        (replace  #"="  " "))
    nil))

(defn map-has-keys? [map keys-set]
  (let [map-keys (set (keys map))]
    (= map-keys keys-set)))

(defn exit-cli-args-count []
  (print-colored-text :red "⚠️ ⚠️ ⚠️  [ERROR] Invalid script call!! You should call it like:")
  (print-colored-text :red "$ npx t-animations :input public/index.html :output public/t-animations.css")
  (println)
  (js/process.exit [0]))

(defn validate-cli-args-count [my-args]
  (when (-> my-args count (not= 4))
    (exit-cli-args-count)))

(defn exit-argv-keys []
  (print-colored-text :red "⚠️ ⚠️ ⚠️  [ERROR] Wrong keys!!")
  (print-colored-text :red "Your script key arguments should have only one :input and :output arguments")
  (println)
  (js/process.exit [0]))

(defn validate-argv-keys [my-argv-map]
  (when-not (map-has-keys? my-argv-map #{":input" ":output"})
    (exit-argv-keys)))

(defn println-cwd []
  (print-colored-text :blue "Your current directory is:")
  (print-colored-text :blue (js/process.cwd))
  (println))

(defn init []
  (let [_               (println-cwd)
        argv            (-> js/process .-argv (.slice 2))
        _               (validate-cli-args-count argv)
        argv-map        (apply hash-map argv)
        _               (validate-argv-keys argv-map)
        file-src        (get argv-map ":input")
        file-dst        (get argv-map ":output")
        distinct-tokens (->> file-src
                             parse-file-to-str
                             treat-raw-str
                             spaces-tokenization
                             filter-valid-tokens
                             distinct)]
    (generate-out-css distinct-tokens file-dst)
    (if (.existsSync fs file-dst)
        (print-colored-text :green (str "Success! Created " file-dst " file."))
        (print-colored-text :red   "Done, but no valid T-Animation class was created."))
    (js/process.exit [0])))