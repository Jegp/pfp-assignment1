import "/futlib/math"

type dart = (f32,f32)

let inside((x, y): dart): bool =
  ((x - f32.i32(1))**f32.i32(2) + (y - f32.i32(1))**f32.i32(2)) <= f32.i32(1)

let estimate_pi [n] (xs:[n]f32) (ys:[]f32): f64 =
  let length = loop length = 0 for i in iota n do
   let is_inside = inside (xs[i], ys[i])
   in if is_inside then length + 1 else length
  in (f64.i32(length) / f64.i32(n)) * 4.0

let main (xs:[]f32) (ys:[]f32): f64 = estimate_pi xs ys
