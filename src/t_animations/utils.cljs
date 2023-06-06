(ns t-animations.utils)

(defn keyword-in-map? [keyword map]
  (contains? (set (keys map)) keyword))

(defn print-colored-text [color text]
  (let [_print-colored-text (fn [c t] (println (str "\u001b[" c "m" t "\u001b[0m")))
        colors              {:red     "31"
                             :green   "32"
                             :yellow  "33"
                             :blue    "34"
                             :magenta "35"
                             :cyan    "36"
                             :white   "37"}]
    (if (keyword-in-map? color colors)
      (_print-colored-text (color colors) text)
      (_print-colored-text "31" (str "Error! color " color " is out of options " (keys colors))))))