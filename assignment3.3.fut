import "/futlib/math"

let f(x:f32) (y:f32) : f32 =
  2.0f32*x*x*x*x*x*x*y*y - x*x*x*x*x*x*y
  + 3.0f32*x*x*x*y*y*y - x*x*y*y*y +
  x*x*x*y - 3.0f32*x*y*y + x*y -
  5.0f32*y + 2.0f32*x*x*x*x*x*y*y*y*y -
  2.0f32*x*x*x*x*x*y*y*y*y*y + 250.0f32

let calc_bin (x:f32) (y:f32) (width:f32) =
  (f x y) * width * width -- expand in two dimensions

let estimate_area [n] (xs:[n]f32) (ys:[n]f32): f32 =
  let bin_width: f32 = 2f32 / f32.i32(n)
  let areas = map (\i -> calc_bin xs[i] ys[i] bin_width) (iota n)
  in reduce (+) 0.0f32 areas

let main [n] (xs:[n]f32) (ys:[]f32): f32 = (estimate_area xs ys) * 4.0f32 / f32.i32(n)
