(ns t-animations.generator
  (:require [clojure.set :as set]
            [clojure.string :refer [blank? join split starts-with? replace]]
            [t-animations.utils :refer [print-colored-text]]))

(defonce fs (js/require "fs"))

(defn match-percentage? [s]
  (not (nil? (re-matches #"^(?:100|[0-9]{1,2})%$" s))))

(defn vector-to-map [v]
  (->> (partition 2 v)
       (map (fn [[k v]] [k v]))
       (into {})))

(defn sanitize-kfp [raw-kfp]
  (let [without-dashes (split raw-kfp "-")
        without-prefix (rest without-dashes)
        with-perc (reduce
                   (fn [acc new-item]
                     (cond
                       (match-percentage? new-item) (conj acc (str "$" new-item "$"))
                       :else (conj acc new-item)))
                   []
                   without-prefix)
        one-str-again (join " " with-perc)
        final-split (split one-str-again "$")
        rmv-blank (remove blank? final-split)
        final-map (vector-to-map rmv-blank)]
    final-map))

(defn sanitize-anp [raw-anp]
  raw-anp)

(defonce possible-anim-props
  {"w"       "width:"
   "h"       "height:"
   "c"       "color:"
   "o"       "opacity:"
   "l"       "left:"
   "r"       "right:"
   "t"       "top:"
   "b"       "bottom:"
   "ml"      "margin-left:"
   "mr"      "margin-right:"
   "mt"      "margin-top:"
   "mb"      "margin-bottom:"
   "bgcolor" "background-color:"
   "scale"   "scale:"
   "rounded" "border-radius:"
   "transf"  "transform:"})

(defn println-invalid-prop-keys [input-key]
  (print-colored-text :red (str "⚠️ ⚠️ ⚠️  Key " input-key " is not valid!"))
  (print-colored-text :red (str "Possible options are: " (keys possible-anim-props))) 
  (prn)
  (js/process.exit [0]))

(defn are-keys-valid? [map-p]
  (doseq [prop map-p]
         (when (not (contains? possible-anim-props (key prop)))
           (println-invalid-prop-keys (key prop)))))

(defn sanitize-props [inp-props]
  (let [space-splitted (js->clj (.split inp-props " "))
        clean-empty    (remove blank? space-splitted)
        map-props      (vector-to-map clean-empty)]
    map-props))

(defn validate-props-keys [p-str] 
  (let [map-props (sanitize-props p-str)
        _         (are-keys-valid? map-props)]
    map-props))

(defn rename-props [p-str]
  (let [map-props (sanitize-props p-str)]
    (set/rename-keys map-props possible-anim-props)))

(defn transf-values-to-str [tf]
  (reduce
   (fn [acc item]
     (str acc
          (cond
            (starts-with? item "rot")     (str "rotate(" (js->clj (.replace item "rot" "")) ")")
            (starts-with? item "translx") (str "translateX(" (js->clj (.replace item "translx" "")) ")")
            (starts-with? item "transly") (str "translateY(" (js->clj (.replace item "transly" "")) ")"))
          " "))
   ""
   tf))

(defn expand-transformations [k v] 
  (if (= k "transform:")
    (let [transf-values (split v ";")
          fixed-transforms (transf-values-to-str transf-values)] 
      fixed-transforms)
    v))

(defn props-map-to-final-str [p-map]
  (reduce
   (fn [acc [k v]]
     (let [v2 (expand-transformations k v)] 
       (str acc " " k " " v2 ";"))) 
   ""
   p-map))
  
(defn get-final-anim-props
  [an-props]
  (let [decimal-an-props (.replace an-props "_" ".")
        final-an-props   (.replace decimal-an-props "-" " ")] 
    final-an-props))

(defn exit-validate-dst-file [file-dst]
  (print-colored-text :red (str "⚠️ ⚠️ ⚠️  Invalid css file name!!"))
  (print-colored-text :red (str "- '" file-dst "' is not a valid css file path."))
  (print-colored-text :red (str "- It should contain only letters, '/' (not '//'), '-' and end with '.css'"))
  (print-colored-text :red (str "- Try something like: 'public/t-animations.css"))
  (prn)
  (js/process.exit [0]))

(defn validate-dst-file [_file-dst]
  (let [file-dst (.replace _file-dst "\"" "")
        is-valid-css-file? (re-matches #"^[a-zA-Z]+(/[a-zA-Z_-]+)?\.css$" file-dst)
        _ (print-colored-text :green (str "file-dst: " file-dst))
        _ (prn file-dst)
        _ (print-colored-text :green (str "is-valid-css-file?: " is-valid-css-file?))
        ]
    (if is-valid-css-file?
      file-dst
      (exit-validate-dst-file file-dst ))))

(defn get-keyframes-props-with-neg [keyframes-props]
  (let [replace-neg (fn [m] (into {} (for [[k v] m]
                                       [k (replace v "neg" "-")])))
        replaced-keyframes-props (replace-neg keyframes-props)]
    replaced-keyframes-props))

(defn get-keyframes-props-with-decimals [keyframes-props]
  (let [replace-decimal (fn [m] (into {} (for [[k v] m] [k (replace v "_" ".")])))
        replaced-dec-keyframes-props (replace-decimal keyframes-props)]
    replaced-dec-keyframes-props))

(defn exit-validate-double-dashes [input-str]
  (print-colored-text :red (str "⚠️ ⚠️ ⚠️  [ERROR] Multiple '--'!!"))
  (print-colored-text :red (str "- '" input-str "' is not a valid T-Animation class."))
  (print-colored-text :red (str "- It should contain only one double dash '--'"))
  (print-colored-text :red (str "- Try something like: anim-0%-scale-1-100%-scale-1_5--5s-infinite"))
  (prn)
  (js/process.exit [0]))

(defn validate-double-dashes [input-str]
  (when (> (count (re-seq #"--" input-str)) 1) 
    (exit-validate-double-dashes input-str)))


(defn reset-atom-and-persist [final-css-str token opt-hover 
                              final-anim-props final-keyframes file-dst]
  (reset! final-css-str
                (str @final-css-str "\n"
                     "." token opt-hover "{\n"
                     "    animation: " token " " final-anim-props "\n"
                     "}\n"
                     "@keyframes " token " {\n"
                     final-keyframes 
                     "}\n"))
  (.writeFileSync fs (validate-dst-file file-dst) (str @final-css-str)))
      

(defn generate-individual-animation [token final-css-str file-dst]
  (let [clean-token (-> token
                        (.replace "\n"       "")
                        (.replace "class=\"" "")
                        (.replace "from"     "0%")
                        (.replace "to"       "100%"))
        _           (validate-double-dashes clean-token)
        [raw-keyframes-props raw-anim-props] (-> clean-token (.split "--"))
        keyframes-props              (sanitize-kfp raw-keyframes-props)
        anim-props                   (sanitize-anp raw-anim-props)
        get-keyframes-str-from-props (fn [k-props] 
                                       (let [k-atom (atom "")]
                                         (doseq [prop k-props]
                                           (let [percentage-key (first prop)
                                                 props-str (second prop)]
                                             (validate-props-keys   props-str)
                                             (let [renamed-keys (rename-props props-str)
                                                   final-prop-str (props-map-to-final-str renamed-keys)]
                                               (reset! k-atom
                                                       (str @k-atom
                                                            percentage-key " {\n"
                                                            final-prop-str  "\n"
                                                            "}\n")))))
                                         @k-atom))
        keyframes-props-with-neg      (get-keyframes-props-with-neg keyframes-props)
        keyframes-props-with-decimals (get-keyframes-props-with-decimals keyframes-props-with-neg)
        final-keyframes               (get-keyframes-str-from-props keyframes-props-with-decimals)
        final-anim-props              (get-final-anim-props anim-props)
        opt-hover                     (if (starts-with? token "hover:anim-") ":hover" "")
        escape-special-chars          #(-> %
                                           (replace "%" "\\%")
                                           (replace "#" "\\#")
                                           (replace ";" "\\;")
                                           (replace ":" "\\:"))
        token                         (escape-special-chars token)]
    
    (when (pos? (count final-keyframes))
      (reset-atom-and-persist final-css-str token opt-hover final-anim-props final-keyframes file-dst))))

(defn generate-out-css [valid-tokens-list file-dst]
  (let [final-css-str (atom "")]
    (doseq [token valid-tokens-list]
           (cond
             (or (starts-with? token "anim-")
                 (starts-with? token "hover:anim-"))
               (generate-individual-animation token final-css-str file-dst)))))
