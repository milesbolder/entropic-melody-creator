extensions [ sound ]

breed [ notes note ]
breed [ markovs markov ]

markovs-own [
  markov-list
  fitness
  markov-entropy
  markov-redundancy
  differential
]

globals [
  winner
  best-markov
  avg-markov

  scale-tones
  minor-pent
  major
  chromatic

   note-one
   mel
   markov-next-index
   markov-next
  note-next
  asc-markov-next
    asc-eval
       sum-list
       sum-prob
       prob-pick
       ?
]

to initialize
  set target-redundancy 0.5
  set population-size 50
  set crossover-rate 0.3
  set mutation-rate 0.05
  set scale "minor pentatonic"

  set update-rate 20
end

to setup
  clear-all
  ;set target-redundancy 0.5
  ;set population-size 50
  ;set crossover-rate 0.3
  ;set mutation-rate 0.05
  ;set scale "minor pentatonic"

  ;set update-rate 20
  setup-scales
  create-notes 16 [
    set shape "circle 2"
    set xcor who
    set color white
  ]

  create-markovs population-size [
    set markov-list (list ) ;;n-values (scale-tones * scale-tones) [random-float 1]
    ifelse order = "first" [
      repeat (scale-tones) [populate-transitions] ] [
      ifelse order = "second" [
       repeat (scale-tones ^ 2) [populate-transitions] ] [
        ifelse order = "third" [
          repeat (scale-tones ^ 3) [populate-transitions] ] [
          repeat (scale-tones ^ 4) [populate-transitions] ]
    ]
    ]

    hide-turtle  ;; the markovs' locations are not used, so hide them
    calculate-entropy
  ]
  setup-winner

  update-display
  reset-ticks



end

to setup-scales
  set minor-pent (list 1 4 6 8 11)
  set major (list 1 3 5 6 8 10 12)
  set chromatic (list 1 2 3 4 5 6 7 8 9 10 11 12)

  if scale = "minor pentatonic" [ set scale-tones 5 ]
  if scale = "major" [ set scale-tones 7 ]
  if scale = "chromatic" [ set scale-tones 12 ]

end

to setup-winner

  set winner min-one-of markovs [differential]
  set best-markov [markov-redundancy] of winner
end

to populate-transitions


let value-one (random-float 1)
let probs (list value-one)
repeat (scale-tones - 2) [
    set probs lput (random-float (1 - (reduce + probs ))) probs
  ]
 set probs lput (1 - (reduce + probs)) probs

  repeat scale-tones [
    let index1 one-of range length probs
    set markov-list lput (item index1 probs) markov-list ;; Add value to end of Markov transition list
    set probs remove-item index1 probs  ;; Remove the indexed value from probs
  ]

end


to go

  if ticks mod update-rate = 0
    [ update-display ]
  create-next-generation
  set winner min-one-of markovs [differential]
  if [differential] of winner <= 0.001  ;;stop simulation when the best markov table is close enough to the target
    [ stop ]
  set best-markov [markov-redundancy] of winner
  tick

end

;; calculate entropy of each markov matrix, the corresponding redundancy, and the differential between markov redundancy and target redundancy

to calculate-entropy
  set markov-entropy (sum (map [ i -> (-1 ) * i * (log i 2)] markov-list))

   ifelse order = "first" [
      set markov-redundancy (1 - markov-entropy / (scale-tones * log scale-tones 2 )) ] [
      ifelse order = "second" [
       set markov-redundancy (1 - markov-entropy / ((scale-tones ^ 2) * log scale-tones 2 )) ] [
        ifelse order = "third" [
          set markov-redundancy (1 - markov-entropy / ((scale-tones ^ 3) * log scale-tones 2 )) ] [
          set markov-redundancy (1 - markov-entropy / ((scale-tones ^ 4) * log scale-tones 2 )) ]
    ]
    ]

  set differential abs ( target-redundancy - markov-redundancy ) ;; calculate difference between target and current redundancy

end

to create-next-generation

  let old-generation markovs with [true]

  let crossover-count (floor (population-size * crossover-rate / 2))

  repeat crossover-count [
    let p1 min-one-of (n-of tournament-num old-generation) [differential]
    let p2 min-one-of (n-of tournament-num old-generation) [differential]

    let child-markov crossover ( [markov-list] of p1)([markov-list] of p2)

    ask p1 [ hatch 1
      [  set markov-list item 0 child-markov ]
    ]
    ask p2 [ hatch 1
      [ set markov-list item 1 child-markov ]
    ]
  ]

  repeat (population-size - crossover-count * 2)
  [ ask min-one-of (n-of tournament-num old-generation) [differential]
    [ hatch 1 ]
  ]

  ask old-generation  [die]

  ask markovs [
    mutate
   calculate-entropy
  ]
end

to update-display ;; display best solution contour
 let scale-in-use chromatic
  ifelse (scale = "minor pentatonic") [ set scale-in-use minor-pent ]
    [ifelse scale = "chromatic" [ set scale-in-use chromatic ]
      [ set scale-in-use major ]
  ]

  set note-one (random length scale-in-use)
  set mel (list (item note-one scale-in-use))
  set markov-next-index (note-one * scale-tones)
  set markov-next (list )
  set note-next 0

  repeat 15 [
      ;let markov-next (list ) ;;create an empty list to contain the probabilities of all possible successive notes

      repeat scale-tones [ ;;populate next note probabilities for whole scale
        set markov-next lput (item markov-next-index [markov-list] of winner) markov-next
        set markov-next-index markov-next-index + 1  ;; select the index of the probability of the next scale tone
      ]

      set asc-markov-next (sort markov-next) ;;put probabilities of next note in order from sm to lg
      set asc-eval random-float 1
      set sum-list (list 0)
      set sum-prob 0
      set prob-pick 0
      set ? 0


      repeat scale-tones [ ;;which two items random float is between on the ordered list
        set sum-list lput (item ? asc-markov-next) sum-list ;; a list of each item in ascending list tested so far, including current item
        set sum-prob (sum sum-list) ;;sum of all tested items including current item
        ifelse asc-eval < sum-prob and asc-eval >= (sum-prob - item ? asc-markov-next) ; if random float is less than the sum of all items tested so far (inclusive), minus sum of items so far EXCLUSIVE of current, choose current item as corresponding prob of next note and exit
         [ set prob-pick item ? asc-markov-next ]
        [ set ? ? + 1 ]
      ]


      let index2 0

      repeat length markov-next [
        ifelse ((item index2 markov-next) = prob-pick)
        [ set  note-next index2 ]
        [ set index2 index2 + 1 ]
      ]

      set mel lput (item note-next scale-in-use) mel


    set markov-next (list )

    set markov-next-index ( note-next * scale-tones )
   set note-next 0
    ]

      let index 0
  repeat 16 [
      ask turtle index [
        set ycor item index mel
        sound:play-note "music box" ((item index mel) + 59) 64 (30 / tempo)

    ]
  wait (30 / tempo)
      set index index + 1
    ]



end

to play-melody
  let index 0
  repeat 16 [
      ask turtle index [
        sound:play-note "music box" ((item index mel) + 60) 64 (30 / tempo)
    ]
  wait (30 / tempo)
      set index index + 1
    ]
end

to-report crossover [tprobs1 tprobs2]

  ;let split-point scaletones + random (length tprobs1 - 1)
  let split-point one-of (range scale-tones (scale-tones ^ 2) scale-tones )
  report list (sentence (sublist tprobs1 0 split-point)
                        (sublist tprobs2 split-point length tprobs2))
              (sentence (sublist tprobs2 0 split-point)
                        (sublist tprobs1 split-point length tprobs1))
end

to mutate
if random-float 1 <= mutation-rate [
  let in-point one-of (range 0 (length markov-list) scale-tones)
  let mutate-range (range in-point (in-point + scale-tones))

  let value-one (random-float 1)
  let probs (list value-one)
  repeat (scale-tones - 2) [
    set probs lput (random-float (1 - (reduce + probs ))) probs
  ]
 set probs lput (1 - (reduce + probs)) probs

  repeat scale-tones [
    let index1 one-of range length probs
    set markov-list replace-item (item 0 mutate-range) markov-list (item index1 probs) ;; Add value to end of Markov transition list
    set probs remove-item index1 probs  ;; Remove the indexed value from probs
    set mutate-range remove-item 0 mutate-range
  ]
]
end

to-report avg
  report (sum [markov-redundancy] of markovs) / population-size
end
@#$#@#$#@
GRAPHICS-WINDOW
210
10
778
474
-1
-1
35.0
1
10
1
1
1
0
0
0
1
0
15
0
12
1
1
1
ticks
30.0

SLIDER
8
275
203
308
population-size
population-size
0
100
50.0
1
1
NIL
HORIZONTAL

BUTTON
20
19
178
52
NIL
setup\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
959
431
1154
464
Target-Redundancy
Target-Redundancy
0
1
0.91
0.01
1
NIL
HORIZONTAL

SLIDER
5
359
201
392
crossover-rate
crossover-rate
0
1
0.28
0.01
1
NIL
HORIZONTAL

SLIDER
4
400
198
433
mutation-rate
mutation-rate
0
1.00
0.25
.01
1
NIL
HORIZONTAL

BUTTON
115
92
178
125
NIL
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
21
93
106
126
go-once
go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

PLOT
793
10
1248
412
Winner
ticks
Redundancy
0.0
10.0
0.0
1.0
true
true
"" ""
PENS
"winner" 1.0 0 -16777216 true "" "plot best-markov"
"avg" 1.0 0 -14454117 true "" "set avg-markov avg\nplot avg-markov"

SLIDER
3
440
197
473
update-rate
update-rate
1
100
1.0
1
1
NIL
HORIZONTAL

CHOOSER
22
131
177
176
scale
scale
"minor pentatonic" "major" "chromatic"
2

BUTTON
21
55
177
88
NIL
play-melody
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

MONITOR
795
427
946
472
Redundancy of winner
[markov-redundancy] of winner
3
1
11

SLIDER
13
183
198
216
tempo
tempo
1
300
300.0
1
1
NIL
HORIZONTAL

BUTTON
1164
430
1246
463
NIL
initialize\n
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

SLIDER
10
318
199
351
tournament-num
tournament-num
1
10
6.0
1
1
NIL
HORIZONTAL

CHOOSER
12
222
150
267
order
order
"first" "second" "third" "fourth"
0

@#$#@#$#@
## WHAT IS IT?

(a general understanding of what the model is trying to show or explain)

## HOW IT WORKS

(what rules the agents use to create the overall behavior of the model)

## HOW TO USE IT

(how to use the model, including a description of each of the items in the Interface tab)

## THINGS TO NOTICE

Tempo assumes notes are eighth notes, with two to the beat.

## THINGS TO TRY

(suggested things for the user to try to do (move sliders, switches, etc.) with the model)

## EXTENDING THE MODEL

(suggested things to add or change in the Code tab to make the model more complicated, detailed, accurate, etc.)

## NETLOGO FEATURES

(interesting or unusual features of NetLogo that the model uses, particularly in the Code tab; or where workarounds were needed for missing features)

## RELATED MODELS

(models in the NetLogo Models Library and elsewhere which are of related interest)

## CREDITS AND REFERENCES

(a reference to the model's URL on the web if it has one, as well as any other necessary credits, citations, and links)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.0.4
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
