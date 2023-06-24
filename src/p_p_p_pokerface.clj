(ns p-p-p-pokerface)

(defn rank [card]
  (let [[fst _] card
        sp_vals {\T 10 \J 11 \Q 12 \K 13 \A 14}]
    
    (if (Character/isDigit fst)
      (Integer/valueOf (str fst))
      (sp_vals fst))))

(defn suit [card]
  (let [[_ snd] card]
    (str snd)))

(def f-check #(some #{%} 
                    (vals (frequencies
                                (map rank %2)))))

(defn pair? [hand]
  (if (some #{2} 
            (vals (frequencies 
                   (map rank hand))))
    true
    false))

(defn three-of-a-kind? [hand]
  (if (f-check 3 hand)
    true
    false))

(defn four-of-a-kind? [hand]
  (if (f-check 4 hand)
    true
    false))

(defn flush? [hand]
  (apply = (map suit hand)))

;(def high-seven                   ["2H" "3S" "4C" "5C" "7D"])
;(def pair-hand                    ["2H" "2S" "4C" "5C" "7D"])
;(def two-pairs-hand               ["2H" "2S" "4C" "4D" "7D"])
;(def three-of-a-kind-hand         ["2H" "2S" "2C" "4D" "7D"])
;(def four-of-a-kind-hand          ["2H" "2S" "2C" "2D" "7D"])
;(def straight-hand                ["2H" "3S" "6C" "5D" "4D"])
;(def low-ace-straight-hand        ["2H" "3S" "4C" "5D" "AD"])
;(def high-ace-straight-hand       ["TH" "AS" "QC" "KD" "JD"])
;(def flush-hand                   ["2H" "4H" "5H" "9H" "7H"])
;(def full-house-hand              ["2H" "5D" "2D" "2C" "5S"])
;(def straight-flush-hand          ["2H" "3H" "6H" "5H" "4H"])
;(def low-ace-straight-flush-hand  ["2D" "3D" "4D" "5D" "AD"])
;(def high-ace-straight-flush-hand ["TS" "AS" "QS" "KS" "JS"])

(defn full-house? [hand]
  (if (and (f-check 3 hand)
           (f-check 2 hand))
    true
    false))

(defn two-pairs? [hand]
  (let [twos (filter #(= % 2)
                     (vals (frequencies 
                            (map rank hand))))]
    (if (or (= (count twos) 2)
            (four-of-a-kind? hand))
            true
            false)))

(defn straight? [hand]
  (let [ranked-hand (map rank hand)
        sorted-ranks (sort ranked-hand)
        comp-ranks (range (first ranked-hand)
                          (+ (first ranked-hand) (count hand)))]
    (or (= comp-ranks sorted-ranks)
        (= [2 3 4 5 14] sorted-ranks))))

(defn straight-flush? [hand]
  (and (straight? hand)
       (flush? hand)))

(defn high-card? [hand]
  true)

(defn value [hand]
  (let [checkers #{[high-card? 0]  [pair? 1]
                 [two-pairs? 2]  [three-of-a-kind? 3]
                 [straight? 4]   [flush? 5]
                 [full-house? 6] [four-of-a-kind? 7]
                 [straight-flush? 8]}]
    (apply max (map second 
                    (filter #(= true (first %)) 
                            (map #(vector ((first %) hand) 
                                          (second %)) 
                                 checkers))))))
