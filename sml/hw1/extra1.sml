fun alternate(xs: int list) =
  let
    fun sum_with_sign(xs: int list, sign: int) =
      if null xs
      then 0
      else sign * (hd xs) + sum_with_sign(tl xs, ~sign)
  in
    sum_with_sign(xs, 1)
  end

fun min_max(xs : int list) =
  if null (tl xs)
  then (hd xs, hd xs)
  else
    let
      val tail_min_max = min_max(tl xs)
      fun get_max(a : int, b : int) = 
        if a > b then a else b
      fun get_min(a : int, b : int) = 
        if a < b then a else b
    in
      (get_min(#1 tail_min_max, hd xs), get_max(#2 tail_min_max, hd xs))
    end

fun cumsum(xs : int list) =
  let
    fun internal(acc : int, xs : int list) =  
      let
        val new_acc = acc + hd xs
      in
        if null (tl xs)
        then [new_acc]
        else new_acc :: internal(new_acc, tl xs)
      end
  in
    internal(0, xs)
  end

fun greeting(name : string option) =
  if isSome name
  then "Hello there, " ^ valOf(name)
  else "Hello there, you"

fun repeat(xs: int list, amount: int list) =
  let
    fun repeat_one(x : int, amount : int) =
      if amount <= 0
      then []
      else x :: repeat_one(x, amount - 1)
  in
    if null xs
    then []
    else repeat_one(hd xs, hd amount) @ repeat(tl xs, tl amount)
  end

fun addOpt(a : int option, b : int option) = 
  if (isSome(a) andalso isSome(b))
  then
    SOME(valOf(a) + valOf(b))
  else
    NONE

fun addAllOpt(xs : int option list) = 
  let
    fun filterSome(xs : int option list) = 
      if null xs
      then []
      else
        if isSome(hd xs)
        then valOf(hd xs) :: filterSome(tl xs)
        else filterSome(tl xs)

    fun sum(xs : int list) = 
      if null xs
      then 0
      else hd xs + sum(tl xs)

    val filtered = filterSome(xs)
  in
    if null filtered
    then NONE
    else SOME(sum(filtered))
  end

fun any(xs : bool list) = 
  if null xs
  then false
  else hd xs orelse any(tl xs)

fun all(xs : bool list) = 
  if null xs
  then true
  else hd xs andalso all(tl xs)

fun zip(xs : int list, ys : int list) = 
  if null xs orelse null ys
  then []
  else (hd xs, hd ys) :: zip(tl xs, tl ys)

fun len (xs : int list) =
  if null xs
  then 0
  else 1 + len(tl xs)

fun zipRecycle (xs : int list, ys : int list) =
  if null xs orelse null ys
  then []
  else
    let
      fun zip (xs : int list, cur_ys : int list, full_ys : int list) = 
        if null xs
        then []
        else
          if null cur_ys
          then (hd xs, hd full_ys) :: zip(tl xs, tl full_ys, full_ys)
          else (hd xs, hd cur_ys) :: zip(tl xs, tl cur_ys, full_ys)
      
      fun swap (pairs: (int * int) list) = 
        if null pairs
        then []
        else (#2 (hd pairs), #1 (hd pairs)) :: swap(tl pairs)
    in
      if len(xs) > len(ys)
      then zip(xs, ys, ys)
      else swap(zip(ys, xs, xs))
    end

fun lookup (strings : (string * int) list, target : string) =
  if null strings
  then NONE
  else
    let
      val pair = hd strings
    in
      if #1 pair = target
      then SOME(#2 pair)
      else lookup(tl strings, target)
    end

fun splitAt (xs : int list, threshold : int) =
  if null xs
  then ([], [])
  else
    let
      val tail_result = splitAt(tl xs, threshold)
      val head = hd xs
    in
      if head >= threshold
      then (head :: (#1 tail_result), #2 tail_result)
      else (#1 tail_result, head :: (#2 tail_result))
    end

fun splitup (xs : int list) = splitAt(xs, 0)

fun isSorted (xs : int list) = null xs orelse null (tl xs) orelse ((hd xs <= hd (tl xs)) andalso isSorted(tl xs))

fun isAnySorted (xs : int list) =
  let
    fun sign (x : int) = if x >= 0 then 1 else ~1
    fun isSorted(xs : int list, dir : int) =
      if null xs orelse null (tl xs)
      then true
      else
        let
          val first = hd xs
          val second = hd (tl xs)
          val diff = second - first
        in
          (diff = 0 orelse sign(diff) = dir) andalso isSorted(tl xs, dir)
        end
  in
    isSorted(xs, 1) orelse isSorted(xs, ~1)
  end

fun sortedMerge (xs : int list, ys : int list) =
  if null xs andalso null ys
  then []
  else if null xs
  then ys
  else if null ys
  then xs
  else
    if hd xs <= hd ys
    then hd xs :: sortedMerge(tl xs, ys)
    else hd ys :: sortedMerge(xs, tl ys)

fun qsort (xs : int list) =
  if null xs
  then []
  else if null (tl xs)
  then xs
  else
    let
      val splited = splitAt(tl xs, hd xs)
    in
      qsort(#2 splited) @ [hd xs] @ qsort(#1 splited)
    end

fun divide (xs : int list) =
  if null xs
  then ([], [])
  else
    let
      fun internal_divide(xs : int list, add_in_left : bool) = 
        if null xs
        then ([], [])
        else
          let
            val tail_result = internal_divide(tl xs, not add_in_left)
          in
            if add_in_left
            then (hd xs :: #1 tail_result, #2 tail_result)
            else (#1 tail_result, hd xs :: #2 tail_result)
          end
    in
      internal_divide(xs, true)
    end

fun not_so_quick_sort (xs : int list) =
  if null xs
  then []
  else if null (tl xs)
  then xs
  else
    let
      val divided = divide(xs)
    in
      sortedMerge(not_so_quick_sort(#1 divided), not_so_quick_sort(#2 divided))
    end

fun fullDivide (n : int, k : int) = 
  let
    fun internal(n : int, k : int, count : int) =
      if (k mod n <> 0)
      then (count, k)
      else internal(n, k div n, count + 1)
  in
    internal(n, k, 0)
  end

fun factorize (x : int) =
  if x = 1
  then []
  else 
    let
      fun internal(k : int, n : int) = 
        if k = 1
        then []
        else if n * n > k 
        then [(k, 1)]
        else
          let 
            val res = fullDivide(n, k)
          in
            if #1 res <> 0
            then (n, #1 res) :: internal(#2 res, n + 1)
            else internal(#2 res, n + 1)
          end
    in
      internal(x, 2)
    end

fun multiply (factors : (int * int) list) = 
  if null factors
  then 1
  else
    let
      fun power (x : int, y : int) =
        if y = 0
        then 1
        else x * power(x, y - 1)

      val cur_factor = hd factors
    in
      power(#1 cur_factor, #2 cur_factor) * multiply(tl factors)
    end
