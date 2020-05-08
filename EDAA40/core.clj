(ns amoebas.TOKYODIFT.core
(:use amoebas.defs amoebas.lib amoebas.run)

  (defn most-energy-and-fuel-target-selector
      "picks a target with the highest sum of stored energy and energy in the cell it is in"
      [hs species env]

      (let
          [energy-and-fuel
              (fn [cell]
                  (if (:occupant cell)
                      (+ (:fuel cell) (:energy (:occupant cell)))
                      (:fuel cell)
                  )
              )
          ]

          (last (sort-by #(energy-and-fuel (env %)) hs))
      )
  )


(defn mindless-divider

    [energy health species env data]

    (if (< energy (+ MinDivideEnergy (/ (- MaxAmoebaEnergy MinDivideEnergy) 2)))
        {:cmd :rest}
        (if (< (rand) 0.3)                      ;; divide with probability 0.3
            {:cmd :divide, :dir (rand-int 8)}
            {:cmd :move, :dir (rand-int 8)}
        )
    )
)

(defn create-argamoeba
    [
        low-energy
        divide-energy
        select-target
        mutation-rate
        mutation-range
        edge-limit
    ]


    (fn [energy health species env data]
        (let
            [
                fs      (friendlies species Environment env)             ;; all friendlies
                hs      (hostiles species Environment env)               ;; all friendlies
                hss     (map #(hostiles species % env) Env-Sections)     ;; all hostiles, by section
                opens   (filter #(region-empty? (Env-Sections %)) Dirs)  ;; direction of all open sections, i.e. without hostiles or friendlies

                edge-min        (apply min                                                          ;; minimal edge distance, obtained from friendlies
                    (cons WorldSize
                          (map #(+ (:edge-distance (:data (env %)) 0) (distance Here %)) fs)
                    ))
                hostile-dist    (apply min (cons WorldSize (map #(distance Here %) hs)))            ;; minimal distance of visible hostiles






                do-move (fn []
                            (let                                        ;; otherwise we gotta move...
                                [
                                    empty-nb     (empty-neighbors env)              ;; these are the empty neighbors
                                    by-fuel      (sections-by-fuel empty-nb env)    ;; this sorts them by the amount of fuel in the corresponding sections
                                ]

                                (if (empty? empty-nb)       ;; no empty neighbors?
                                    (let
                                        [hs  (hostiles species Neighbors env)]      ;; hostile neighbors

                                        (if (empty? hs)                             ;; nobody to hit?
                                            {:cmd :rest}                              ;; eat
                                            {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                        )
                                    )            ;; hunker down, we can't move --- FIXME: perhaps we should hit someone?
                                    {:cmd :move :dir (last by-fuel)}    ;; move toward the most fuel
                                )
                            )
                        )
                do-fuel (fn []
                            (if (< MaxFuelingEnergy (:fuel (env Here)))     ;; are we *at* a McDonald's?
                                {:cmd :rest}                                ;; chomp chomp
                                (do-move)                                   ;; otherwise, keep looking
                            )
                        )

                do-hit  (fn []
                            (let
                                [hs  (hostiles species Neighbors env)]      ;; hostile neighbors

                                (if (empty? hs)                             ;; nobody to hit?
                                    (do-fuel)                               ;; eat
                                    {:cmd :hit :dir (Neighbor-To-Dir (select-target hs species env))}   ;; KAPOW!
                                )
                            )
                        )
                do-div  (fn [empty-nb]
                  (if (<= (rand) mutation-rate)


                      {:cmd :divide :dir (rand-nth empty-nb)
                       :function
                          (create-argamoeba
                              (bound MoveEnergy (+ low-energy (rand-int (inc (* 2 mutation-range))) (- mutation-range)) MaxAmoebaEnergy)
                              (bound MinDivideEnergy (if (< divide-energy 85) (+ 10 divide-energy) divide-energy) MaxAmoebaEnergy)
                              select-target
                              mutation-rate
                              mutation-range
                              edge-limit) }
                      {:cmd :divide :dir (first (sections-by-friendlies Dirs env species )) }
                  )
                  )


                            ;;{:cmd :divide :dir (rand-nth empty-nb)}         ;; amoeba parenting: drop the child wherever...

            ]

            (cond
              (< energy 5)
                {:cmd :rest} ;;suicide function
              (< divide-energy energy)               ;; parenthood!
                  (let
                        [empty-nb   (empty-neighbors env)]

                        (if (empty? empty-nb)       ;; nowhere to put that crib?
                            (do-hit)                ;; then screw parenthood, hit someone
                            (do-div empty-nb)       ;; oooh, look, it's... an amoeba :-(
                        )
                    )
                (< energy low-energy)           ;; need some chow?
                (if (= 1 (rand-int 2))
                    (do-fuel)
                    (do-move)
                    )
                (< energy 50)

                  (let
                      [empty-nb   (empty-neighbors env)]

                      (if (empty? empty-nb)       ;; nowhere to put that crib?
                          (do-move)                ;; then screw parenthood, run away
                          (do-div empty-nb)
                                ;; oooh, look, it's... an amoeba :-(
                      )
                  )

                (hostiles species Neighbors env)            ;; someone looking at us funny?
                    (do-hit)                    ;; whack 'em

                :else
                    (do-fuel)                   ;; let's eat some more
            )
        )
    )
)

(def Evam (create-argamoeba 10 15 most-energy-and-fuel-target-selector 0.3 1 0.5))
