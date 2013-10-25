(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [base exp current]
                 (if (== exp 0)
                    current
                    (recur base (dec exp) (* current base))))]
    (helper base exp 1)))



(defn last-element [a-seq]
  (if (nil? (second a-seq)) (first a-seq)
    (recur (rest a-seq))))



(defn seq= [seq1 seq2]
  (if (not (== (count seq1) (count seq2)))
    false
    (cond
      (empty? seq1) true
      (== (first seq1) (first seq2)) (recur (rest seq1) (rest seq2))
     :else false)))

(defn find-first-index [pred a-seq]
  (loop [a-seq a-seq
         pred pred
         index 0]
    (cond
     (empty? a-seq) nil
     (pred (first a-seq)) index
     :else (recur (rest a-seq) pred (inc index)))))


(defn avg [a-seq]
  (loop [sum (first a-seq)
         a-seq (rest a-seq)
         elements 1]
    (cond
     (empty? a-seq) (/ sum elements)
     :else (recur
            (+ sum (first a-seq))
            (rest a-seq)
            (inc elements)))))


(defn toggle [a-set elem]
  (if
    (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))

(defn parity [a-seq]
  (loop [odds #{}
         a-seq a-seq]
    (cond
     (empty? a-seq) odds
     :else (recur (toggle odds (first a-seq))
            (rest a-seq)))))


(defn fast-fibo [n]
    (loop [f1 0
         f2 1
         n n]
    (cond
     (== n 0) f1
     :else (recur f2 (+ f1 f2) (dec n)))))




(defn cut-at-repetition [a-seq]
  (loop [used #{}
         a-seq a-seq
         b-seq []]
    (cond
     (nil? (first a-seq)) b-seq
     (contains? used (first a-seq)) b-seq
     :else (recur (conj used (first a-seq))
                  (rest a-seq)
                  (conj b-seq (first a-seq))))))






