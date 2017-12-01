let process_idx [n] (xs: [n]i32) (ys: [n]i32): (i32, i32) =
  let diff (x: i32, y: i32, i:i32) : (i32, i32) = (i32.abs (x - y), i)
  let t_max (t1:(i32, i32)) (t2:(i32, i32)): (i32, i32) =
    if t1.1 > t2.1 then t1 else t2
  let index_list : [n](i32, i32) = map diff (zip xs ys (iota n))
  in reduce t_max (0, 0) index_list

let main(s1:[]i32, s2:[]i32): (i32, i32) = process_idx s1 s2
