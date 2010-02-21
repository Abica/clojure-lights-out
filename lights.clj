(ns lights)
(def valid-room-states [:on :off])
(def num-cols 5)
(def num-rows 5)
(def grid-size (* num-cols num-rows))
(def prompt "> ")
(def ansi-yellow "\033[33m")
(def ansi-blue "\033[34m")
(def ansi-terminator "\033[0m")

(defn on? "is x on?"
  [x]
  (= :on x))

(defn on "return the on state" [x] :on)

(defn flip
  "flips the value of x"
  [x]
  (if (= :on x) :off :on))

(defn flip-at
  "returns a copy of xs with the state of the cell at x toggled"
  [x xs]
  (assoc xs
         x
         (flip (xs x))))

(defn zip [& colls]
  (apply map (fn [& xs] xs) colls))

; why do I have to do this?
; there's an int and num function that should work (based on docs) but don't
; not on strings anyway.. int does work on chars but returns ascii values
; if you know, hit me up on twitter: @abica
(defn to-int
  "coerce (str)i into an integer"
  [i]
  (Integer. i))

(defn colorize-light
  "return an appropriately colored version of x"
  [x]
  (str (if (on? x)
          ansi-yellow
          ansi-blue)
        "[=]"
        ansi-terminator))

(defn clear-screen
  "clear the screen"
  []
  (println "\033[H\033[2J"))

(defn display-banner
  "take an array of lines, draw them on the screen and then wait for input"
  [& lines]
  (clear-screen)
  (println
    (apply str
           (interpose "\n" lines)))
  (println "\n\n[Press enter to continue]")
  (read-line))

(defn draw-rooms
  "compose the grid of lights"
  [xs]
  (let [left-gutter (concat ["  "] (map (fn [x] [x " "]) (range 5)))]
    (apply str (interpose "\n"
      (map (fn [row] (apply str (apply concat row)))
           (zip left-gutter (concat [" 0  1  2  3  4"]
                                    (partition num-cols (map colorize-light xs)))))))))

(defn random-room-state
  "return a random valid room state"
  []
  (first
    (sort-by (fn [x] (rand))
             valid-room-states)))

(defn seed-states
  "randomizes the states of every room in the list"
  [xs]
  (map (fn [x] (random-room-state))
        xs))

(defn create-grid
  "generate a randomized grid of a given size"
  [n]
  (seed-states
    (range n)))

(defn number-of-rooms-on
  "count the number of rooms with the light on"
  [xs]
  (count
    (filter on? xs)))

(defn check-for-win
  "determine whether the game has been won"
  [xs]
  (let [size (number-of-rooms-on xs)]
    (or
      (= (count xs)
          size)
      (zero? size))))

;;;;;;;;;;;;;;
; SCREENS
;;;;;;;;;;;;;;
(defn help-screen
  "display a list of available options"
  [args xs]
  (display-banner
    "Welcome to lights out! The game is simple. The to get all of the lights either on or off."
    "Toggling a light on or off also toggles it's adjacent neighbors."
    "\nHere are some options:"
    "\th\tShow this help page"
    "\tt x,y\tToggle the light at cell x, y"
    "\tn\tNew game"
    "\ts\tSolve current game"
    "\tq\tQuit the game")
  xs)

(defn quit-game
  "allow player to quit the game"
  [args xs]
  (clear-screen)
  (println "I miss you already.. Play again!")
  (. System exit 0))

(defn locate-room
  "calculates the address of a room given it's x and y or a comma separated string of it's x and y"
  ([address]
    (let [[x y]
          (map to-int
               (rest (re-find #"\s?(\d+),\s?(\d+)" address)))]
      [x y (locate-room x y)]))
  ([x y]
    (+ (* x num-cols) y)))

(defn locate-neighbors
  "returns all existing adjacent neighbors"
  [x y xs]
  (filter (partial get xs)
          (map (partial apply locate-room)
               [[(- x 1) y]
                [x (- y 1)]
                [(+ x 1) y]
                [x (+ y 1)]])))

(defn toggle-room
  "toggle a room and it's neighbors"
  [args xs]
  (let [[x y address] (locate-room args) ys (vec xs)]
      (println x y address)
    (if (get ys address)
      (reduce (fn [zs z] (flip-at z zs))
              ys
              (conj (locate-neighbors x y ys)
                    address))
      (do
        (display-banner "That room doesn't exist! Try again.")
        ys))))

(defn new-game
  "generates a new board with random light states"
  [args xs]
  (seed-states xs))

(defn solve-game
  "turn all of the lights on to quickly beat the game"
  [args xs]
  (map on xs))

; menu options that players are able to choose
(def options {\q quit-game,
              \h help-screen,
              \t toggle-room,
              \s solve-game,
              \n new-game})

(defn handle-choice
  "if the user supplies a valid menu option then run it"
  [input xs]
  (let [option (first input)
        args (apply str (rest input))]
    (if (options option)
       ((options option) args xs)
       xs)))

(defn blit-screen
  "draw the current board and prompts to a fresh screen"
  [xs]
  (clear-screen)
  (println (draw-rooms xs))
  (println "\nWhat do you want to do? (press h for help)\n")
  (if (check-for-win xs)
    (do (println "YOU'VE WON!!!")
        (println "Play again! Press n to restart.")))
  (print prompt) (flush))

(defn play
  "run main game loop"
  [xs]
  (loop [ys xs]
    (blit-screen ys)
    (recur (handle-choice (read-line) ys))))

; initialize grid
(def grid (create-grid grid-size))

; play game
(play grid)