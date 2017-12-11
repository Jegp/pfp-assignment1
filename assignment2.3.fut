import "/futlib/math"

type point = (i32,i32)
type line = (point,point)
type points = []point

let compare (v1:i32) (v2:i32) : i32 =
  if v2 > v1 then 1 else if v1 > v2 then -1 else 0

let slo ((x1,y1):point) ((x2,y2):point) : f32 =
  if x2==x1 then if y2>y1 then 1f32 else -1f32
                 else r32(y2-y1) / f32.abs(r32(x2-x1))

let linepoints ((x1,y1):point, (x2,y2):point) : points =
  let len = 1 + i32.max (i32.abs(x2-x1)) (i32.abs(y2-y1))
  let xmax = i32.abs(x2-x1) > i32.abs(y2-y1)
  let (dir,slop) =
    if xmax then (compare x1 x2, slo (x1,y1) (x2,y2))
    else (compare y1 y2, slo (y1,x1) (y2,x2))
  in map (\i -> if xmax then (x1+i*dir, y1+t32(slop*r32(i)))
                else (x1+t32(slop*r32(i)), y1+i*dir)) (iota len)

-- Write to grid
let update [h] [w] [n] (grid:*[h][w]i32)(xs:[n]i32)(ys:[n]i32):*[h][w]i32 =
  let is = map (\x y -> w*y+x) xs ys
  let flatgrid = reshape (h*w) grid
  let ones = map (\ _ -> 1) is
  in reshape (h,w) (scatter flatgrid is ones)

-- Sequential algorithm for drawing multiple lines
let drawlines [h] [w] [n] (grid: *[h][w]i32) (lines:[n]line) : [h][w]i32 =
  loop (grid) for i < n do -- find points for line i
    let (xs,ys) = unzip (linepoints (lines[i]))
in update grid xs ys

-- Assignment 2.3

-- Translates points
let transl_point (x: i32, y: i32) ((tx, ty):point): point =
  (x + tx, y + ty)

let transl_line ((p1, p2):line) (t:point):line =
  (transl_point p1 t, transl_point p2 t)

-- Formula: (x′,y′) = (x cos f − y sin f , y cos f + x sin f)
let rotate_point ((x, y):point) (r:f64): point =
  let cosx = f64.cos(r)
  let siny = f64.sin(r)
  let fx = f64.i32(x)
  let fy = f64.i32(y)
  in (i32.f64(fx * cosx - fy * siny), i32.f64(fy * cosx + fx * siny))

let rotate_line ((p1, p2):line) (r:f64): line =
  (rotate_point p1 r, rotate_point p2 r)

let rotate_lines_center [n] (lines:[n]line) (center:point) (r:f64): []line =
  let neg_center = (-center.1, -center.2)
  let centered = map (\line -> transl_line line neg_center) lines
  let rotated = map (\line -> rotate_line line r) centered
  in map (\line -> transl_line line center) rotated

-- Test on image
let main (r:f64) : [][]i32 =
  let height:i32 = 30
  let width:i32 = 70
  let grid : *[][]i32 = replicate height (replicate width 0)
  let lines = [((58,20),(2,3)),((27,3),(2,28)),((5,20),(20,20)),
               ((4,10),(6,25)),((26,25),(26,2))]
  let rotated = rotate_lines_center lines (width / 2, height / 2) r
in drawlines grid rotated
