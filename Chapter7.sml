(*Chapter 7 problem 2*)
fun member (e,nil) = false
  | member(e,L) =
  let
    fun headEq(e, nil) = false
      | headEq(e, hd::tail) =
          if e = hd then true
          else headEq(e,tail)
  in
    headEq(e,L)
  end;

(*Chapter 7 problem 3*)
fun less(e,nil) = []
  | less(e,L) =
  let
    fun findLess(e,nil) = nil
      | findLess(e,hd::tail) =
      if e > hd then hd::findLess(e,tail)
      else findLess(e,tail)
  in
    findLess(e,L)
  end;

(*Chapter 7 problem 4*)
fun repeats nil = false
  | repeats [_] = false
  | repeats (L) =
  let
    fun findDupl nil = false
      | findDupl [_] = false
      | findDupl (first::next::tail) =
      if first = next then true
      else findDupl(next::tail)
  in
    findDupl(L)
  end;

(*Chapter 7 problem 5*)
fun eval (nil, x: real) = 0.0
  | eval ([i], x: real) = i
  | eval (hd::tail, x: real) =
  hd + (x * eval(tail, x))

(*Chapter 7 problem 7*)
fun quicksort (nil, P) = nil
  | quicksort (pivot::rest, P) =
  let
    fun split(nil) = (nil,nil)
      | split(x::xs) =
      let
        val (below,above) = split(xs)
      in
        if P(x, pivot) = true then (x::below, above)
        else (below, x::above)
      end;
    val (below, above) = split(rest)
  in
    quicksort(below,P) @ [pivot] @ quicksort(above,P)
  end;

(*Chapter 7 additions for quicksort*)
fun icmp (a,b) = a < b
fun rcmp (a:real, b) = a < b
fun ircmp(a,b) = a > b
fun rrcmp(a:real, b) = a > b
