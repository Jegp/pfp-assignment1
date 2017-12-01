let process (xs: []i32) (ys: []i32): i32 =
  let diff (x: i32, y: i32) : i32 = i32.abs (x - y)
  in reduce i32.max 0 (map diff (zip xs ys))

let main(s1:[]i32, s2:[]i32): i32 = process s1 s2
