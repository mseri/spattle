let find_hash input difficulty =
  let alphabet = Array.init 26 (fun i -> char_of_int (97+i)) in
  let make_string n =
    let buf = Buffer.create 7 in
    let rec go n =
      let c = alphabet.(Int64.(rem n 26L |> to_int)) in
      match Int64.div n 26L with
      | 0L -> Buffer.contents buf
      | r -> Buffer.add_char buf c; go r
    in go n
  in
  (* 40 == len SHA1 hex hash *)
  let check_tail n hash =
    let rec go acc k =
      if k = 0 then acc
      else if not acc then acc
      else go (String.unsafe_get hash (40-k) = '0') (k-1)
    in
      (* We need to find exactly n zeroes *)
      String.unsafe_get hash (40-n-1) != '0'
      && go true n
  in
  let test suffix =
    Digestif.SHA1.digestv_string [input; suffix]
    |> Digestif.SHA1.to_hex
    |> check_tail difficulty
  in        
  let rec loop n =
    let suffix = make_string n in
    if test suffix then suffix else loop Int64.(add 1L n)
  in loop 0L