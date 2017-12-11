import "/futlib/math"

type point = (i32,i32)
type line = (point,point)
type points = []point

-- Exercise 2.1
-- Translates points
let transl_point (x: i32, y: i32) ((tx, ty):point): point =
  (x + tx, y + ty)

let transl_line ((p1, p2):line) (t:point):line =
  (transl_point p1 t, transl_point p2 t)

-- Translate all lines in an image with the offset given in ``offset``
let transl_lines [n] (lines:[n]line) (offset:point) : [n]line =
  map (\line -> transl_line line offset) lines
