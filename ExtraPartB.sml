fun f2n(f,0,x) = x
  | f2n(f,n,x) =
  f2n(f,n-1,f(x))
