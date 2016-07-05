(ns looping-is-recursion)

(defn power [base exp]
  (let [helper (fn [acc base exp]
                 (if (zero? exp)
                   acc
                   (recur (* acc base) base (dec exp))))]
    (helper 1 base exp)))

(defn last-element [a-seq]
  (let [helper (fn [a-seq]
                 (cond
                   (empty? a-seq) nil
                   (= 1 (count a-seq)) (first a-seq)
                   :else (recur (rest a-seq))))]
    (helper a-seq)))

(defn seq= [seq1 seq2]
  (let [helper (fn [seq1 seq2]
                 (cond
                   (and (empty? seq1) (empty? seq2)) true
                   (or (empty? seq1) (empty? seq2)) false
                   (not (= (first seq1) (first seq2))) false
                   :else (recur (rest seq1) (rest seq2))))]
    (helper seq1 seq2)))

(defn find-first-index [pred a-seq]
  (loop [pred pred
         n 0
         b-seq a-seq]
    (cond
      (empty? b-seq) nil
      (pred (first b-seq)) n
      :else (recur pred (inc n) (rest b-seq)))))


(defn avg [a-seq]
  (loop [s 0
         c 0
         b-seq a-seq]
    (if (empty? b-seq)
      (/ s c)
      (recur (+ s (first b-seq)) (inc c) (rest b-seq)))))

(defn parity [a-seq]
  (loop [a-set '#{}
         b-seq a-seq]
    (if (empty? b-seq)
      a-set
      (let [toggle (fn [d-set x]
            (if (some #(= x %) a-set)
              (disj d-set x)
              (conj d-set x)))]
      (recur (toggle a-set (first b-seq)) (rest b-seq))))))

(defn fast-fibo [n]
  (loop [l1 1
         l2 0
         c 2]
    (cond
      (= n 0) l2
      (= n 1) l1
      (= c n) (+ l1 l2)
      :else (recur (+ l1 l2) l1 (inc c) ))))

(defn cut-at-repetition [a-seq]
  (loop [a-set '#{}
         b-seq '()
         c-seq a-seq]
    (cond
      (empty? c-seq) b-seq
      (some #(= (first c-seq) %) b-seq) b-seq
      :else (recur (conj a-set (first c-seq)) (reverse (conj (reverse b-seq) (first c-seq))) (rest c-seq)))))
