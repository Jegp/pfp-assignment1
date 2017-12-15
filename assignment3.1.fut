import "/futlib/math"

type dart = (f32,f32)

let inside(x:f32) (y:f32): bool =
  ((x - 1.0f32)**2.0f32 + (y - 1.0f32)**2.0f32) <= 1f32

let estimate_pi [n] (xs:[n]f32) (ys:[n]f32): f64 =
  let is_inside = map (\i -> i32.bool (inside xs[i] ys[i])) (iota n)
  let length = reduce (\s l -> s + l) 0 is_inside
  in (f64.i32(length) / f64.i32(n)) * 4.0

let main (xs:[]f32) (ys:[]f32): f64 = estimate_pi xs ys
