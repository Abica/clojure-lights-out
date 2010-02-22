(ns test-lights
  (:use clojure.test lights))

(deftest test-on?
  (is (on? :on))
  (is (not
        (on? :false))))

(deftest test-on
  (is (= (on :off) :on)))

(deftest test-flip
  (is (= (flip :off) :on))
  (is (= (flip :on) :off))
  (is (= (flip "anything else") :on)))

(deftest test-flip-at
  (is (= (flip-at 2 [:on :off :off :on])
          [:on :off :on :on]))
  (is (thrown? IndexOutOfBoundsException
               (flip-at 10000 [:on :off :off :on]))))

(deftest test-to-int
  (is (= (to-int "3") 3))
  (is (= (to-int 5)   5))
  (is (thrown? NumberFormatException (to-int "sdafsd"))))

(deftest test-colorize-light
  (is (= (colorize-light :on)  "\033[33m[=]\033[0m"))
  (is (= (colorize-light :off) "\033[34m[=]\033[0m")))

(deftest test-draw-rooms
  (is (= (draw-rooms [:o :o :o :o :on])
          "   0  1  2  3  4\n0 \033[34m[=]\033[0m\033[34m[=]\033[0m\033[34m[=]\033[0m\033[34m[=]\033[0m\033[33m[=]\033[0m")))

(deftest test-random-room-state
  (let [xs (distinct
              (map #(%)
                   (repeat 100 random-room-state)))]
    (is (= (count xs) 2))
    (is (not= (first xs)
              (last xs)))))

(deftest test-seed-states
  (let [xs (create-grid 25)]
    (is (not= (seed-states xs)
               xs))))

(deftest test-create-grid
  (let [xs (create-grid 25)]
    (is (= (count xs)) 25)))

(deftest test-number-of-rooms-on
  (is (= (number-of-rooms-on [:on :off :on :off :on :on])
          4)))

(deftest test-check-for-win
  (is (not (check-for-win [:on :off :on :off])))
  (is      (check-for-win [:on :on :on :on]))
  (is      (check-for-win [:off :off :off :off])))

(deftest test-locate-room
  (is (= (locate-room "4,3")  [4 3 19]))
  (is (= (locate-room "sdsd") [nil nil nil]))
  (is (= (locate-room 4 3)     19)))

(deftest test-in-bounds?
  (is (in-bounds? 4 3))
  (is (in-bounds? 0 0))
  (is (in-bounds? 4 4))
  (is (not (in-bounds? 23 44))))

(deftest test-locate-neighbors
  (let [xs (vec (range 25))]
    (is (= (locate-neighbors 40 -2 xs) []))
    (is (= (locate-neighbors 0 0 xs)   [1 5]))
    (is (= (locate-neighbors 2 2 xs)   [11 7 13 17]))
    (is (= (locate-neighbors 4 4 xs)   [23 19]))))

(deftest test-new-game
  (let [solved (repeat 25 :on)]
    (is (not (= (new-game :args solved)
                 solved)))))

(deftest test-solve-game
  (let [solved (repeat 25 :on)
        unsolved (create-grid 25)]
    (is (= (solve-game :args unsolved)
            solved))))
