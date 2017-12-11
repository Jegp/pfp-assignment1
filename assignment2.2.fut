import "/futlib/math"

type point = (i32,i32)
type line = (point,point)
type points = []point

-- Formula: (x′,y′) = (x cos f − y sin f , y cos f + x sin f)
let rotate_point ((x, y):point) (r:f32) =
  let cosx = i32.f32(f32.cos(r))
  let siny = i32.f32(f32.sin(r))
  in (x * cosx - y * siny, y * cosx + x * siny)
