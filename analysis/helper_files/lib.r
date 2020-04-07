trace = FALSE

fatal  = function (s) { 
  cat ('enter fatal\n')
  traceback ()
  stop ("+++ FATAL ERROR: ", s, ". Stopping...\n") 
  stop
}

equal.within.tol = function (r1, r2) { abs (r1 - r2) < 1e-12 }
zero.within.tol  = function (x)      { equal.within.tol (x, 0) }
odds             = function (p)      { p / (1 - p) }   
#normalize        = function (p)      { p / sum (p) }

colname.from.to = function  (d, from, to) { 
  names = colnames (d)
  names [which (names (d) == from)] = to
  names 
}

# Allows NAs, which are represented with "x".
interaction.w.nas = function (d) {
  apply (d, 1, function (r) { paste (ifelse (is.na(r), "x", r), collapse="")})
}

standard.err = function (x, na.rm = FALSE) {
  if (is.matrix (x)) 
    apply (x, 2, se, na.rm = na.rm)
  else if (is.vector (x)) 
    sqrt (var (x, na.rm = na.rm)) / sqrt (length (x))
  else if (is.data.frame (x)) 
    sapply (x, se, na.rm = na.rm)
  else 
    sqrt (var(as.vector(x), na.rm = na.rm)) / sqrt (length (as.vector (x)))
}

dmultinom.wo.round = function (x, size = NULL, prob, log = FALSE) 
{
  K <- length(prob)
  if (length(x) != K) 
    stop("x[] and prob[] must be equal length vectors.")
  if (any(!is.finite(prob)) || any(prob < 0) || (s <- sum(prob)) == 
      0) 
    stop("probabilities must be finite, non-negative and not all 0")
  prob <- prob/s
  #x <- as.integer(x + 0.5)
  if (any(x < 0)) 
    stop("'x' must be non-negative")
  N <- sum(x)
  if (is.null(size)) 
    size <- N
  else if (size != N) 
    stop("size != sum(x), i.e. one is wrong")
  i0 <- prob == 0
  if (any(i0)) {
    if (any(x[i0] != 0)) 
      return(if (log) -Inf else 0)
    if (all(i0)) 
      return(if (log) 0 else 1)
    x <- x[!i0]
    prob <- prob[!i0]
  }
  r <- lgamma(size + 1) + sum(x * log(prob) - lgamma(x + 1))
  if (log) 
    r
  else exp(r)
}