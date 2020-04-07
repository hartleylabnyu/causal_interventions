#############################################################################
# Utility functions for creating and computing properties of joint probability distributions.
# A joint is represented by a data frame, one row per each unique combination of its variables.
# Colnames are the variable names. The 'p' column also appears last and is the probability of that entry.
# Rownames are strings uniquely associated with each system state (formed using the interaction function).
# Some, but not all, routines handle variables with an arbitrary number of states (i.e., non binary variables).

# Naming conventions:
#  j  - a joint distribution
#  p  - a probabilitiy
#  x  - a named vector representing a system state (values for some or all the variables in j)
#  xi - a numeric index into an x
#  xs - a data frame of multiple x's
#  v  - a variable name
#  vs - a vector of variable names
#  vi - a numeric index into j's variables (i.e., a column number)
#  states - a vector of the values that can be assumed by a variable 
#############################################################################
library (utils)
library (plyr)
library (ggm)
library (psych)

binary.states = c (0, 1)
trinary.states = c (0, 1, 2)

assert.names = function (j, vs) {
  if (is.null (vs)) stop ('assert.names: Unnamed state.')
  if (!all (vs %in% joint.vs (j))) {
    stop ('assert.names: Names (', vs, '), not all in joint (', joint.vs (j), ').')
  }
}   

# Return an index vector indicating which rows of the joint match state x.
# X can be a vector or list. If list, it can be a (one row) data frame.
joint.idx = function (j, x) {
  assert.names (j, names (x))
  if (is.list (x)) { 
    stopifnot (all (lengths (x) == 1))
    if (class (x) == 'data.frame') x = unlist (x) # Must unlist for recycling to work.
  } 
  else stopifnot (is.vector (x))
  if      (length (x) == 1) j[, names(x)] == x 
  else if (length (x) == 0) rep (TRUE, nrow (j)) 
  else    apply (sapply (names (x), function (v) j[, v] == x[v]), 1, all)
}   

# The (unnormalizedd) subset of j that matches x.
joint.subset = function (j, x) {
  j [joint.idx (j, x),]
}   

# Normalized joint
joint.normalized = function (j) { 
  j$p = j$p / sum (j$p)
  if (any (is.nan (j$p))) { print (j); stop ('joint.normalized: NaNs in normalized joint.\n') }
  j
}   

# Joint conditionalized on x
joint.conditionalized = function (j, x) {
  j$p [!joint.idx (j, x)] = 0
  joint.normalized (j)
}

# Marginalize joint by integregating out variables not in vs.
# If out is present, then those variables are marginalized out instead.
joint.marginalized = function (j, vs = NULL, out = NULL) { 
  if (is.null (out)) {
    if (!is.null (vs)) {
      assert.names (j, vs)
      j = aggregate (j$p, by = j[vs], sum); names (j)[ncol (j)] = 'p'
      j = joint.w.rownames (j)
    }
  }
  else {
    stopifnot (is.null (vs), all (out %in% joint.vs (j)))
    j = joint.marginalized (j, setdiff (joint.vs (j), out))
  }
  j 
}

# Marginal probability of v = 1 for each v in vs (suitable for binary variables).
# If vs is NULL, then marginals for all variables in j are returned.
# Joint is first conditionalized on a non-null gx.
v.p = function (j, vs = NULL, gx = NULL)  { 
  v.p.1 = function (v) sum (j$p[j[,v] == 1])
  if (is.null (vs)) vs = joint.vs (j) else assert.names (j, vs)
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  if (length (vs) == 1) v.p.1 (vs) else sapply (vs, v.p.1)
}   

# Probability of state x in j. X may be a partial state.
# Joint is first conditionalized on a non-null gx.
x.p = function (j, x, gx = NULL)  { 
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  sum (j$p[joint.idx (j, x)])
}   

# Entropy associated with with variables vs in j. If vs is null, returns entropy of entire joint.
# Joint is first conditionalized on a non-null gx.
v.entropy = function (j, vs = NULL, gx = NULL) {
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  j = joint.marginalized (j, vs)
  -sum (ifelse (j$p == 0, 0, j$p * log2 (j$p)))
}  

# Return names of vars in j.
joint.vs = function (j) { 
  setdiff (names (j), 'p') # All column names except "p"
}

# Return pairs of variables in j.
joint.v.pairs = function (j, simplify=F) { 
  vs = joint.vs (j)
  if (length (vs) >= 2) 
    combn (vs, 2, simplify=simplify)
  else 
    list ()
}

# Return triples of variables in j.
joint.v.triples = function (j, simplify = F) 
  if (length (joint.vs (j)) >= 3) combn (joint.vs (j), 3, simplify=simplify) else list ()

# Return a list with the states of each variable in j.
joint.vs.states = function (j) 
  lapply (j [,joint.vs (j)], function (col) unique (col) )

# Return joint entries in canonical sort order: left-to-right across columns, ascending within columns.
joint.sorted = function (j) 
  j[do.call (order, j[joint.vs (j)]), ]

# Return joint with variable columns in alphabetical order ("p" remains last).
joint.w.sorted.cols = function (j) { 
  j[, c (sort (joint.vs (j)), 'p')]
}

# Set row names as interaction of all the vars. (Allows joint entries to be indexed like an array.)  
joint.w.rownames = function (j) {
  rownames (j) = interaction (j[joint.vs (j)], sep='')
  j
}

# Put rows and column in order and add rownames.
canonicalized.joint = function (j) 
  joint.w.rownames (joint.sorted (joint.w.sorted.cols (j)))

# True if the two joints have the same rows and columns. 
joints.commensurable = function (j1, j2) 
  nrow (j1) == nrow (j2) & 
    ncol (j1) == ncol (j2) & 
    all (colnames (j1) == colnames (j2)) & all (rownames (j1) == rownames (j2))

# True if the two joints are equal (within a tolerance defined by equal.within.tol).
joints.equal = function (j1, j2) {
  j1 = canonicalized.joint (j1); j2 = canonicalized.joint (j2); stopifnot (joints.commensurable (j1, j2)) 
  all (mapply (equal.within.tol, j1$p, j2$p)) # Whether all p values match.
}   

# A measure of how similar two joints are.
joint.distance = function (j1, j2) {
  j1 = canonicalized.joint (j1); j2 = canonicalized.joint (j2)
  stopifnot (joints.commensurable (j1, j2)) 
  sum (abs (j1$p - j2$p)**2)
} 

# The KL divergence from joint q to joint p.
joint.kl.divergence = function (p, q) {
  p = canonicalized.joint (p); q = canonicalized.joint (q)
  stopifnot (joints.commensurable (p, q)) 
  sum (p$p * log (p$p / q$p))
} 

# Compute the log odds ratio (or "association") between selected feature pair.
# If v1 and v2 are null, lods between all variable pairs are returned.
# (Assumes variables are binary.)
lod = function (j, v1 = NULL, v2 = NULL, gx = NULL) {
  stopifnot (is.null (v1) == is.null (v2))
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  if (is.null (v1)) {
    pairs = joint.v.pairs (j, F); lod = NULL
    if (length (pairs) > 0) {
      lod = sapply (pairs, function (p) lod (j, p[1], p[2]))
      names (lod) = sapply (pairs, function (p) paste (p[1], p[2], sep = ':'))
    }
  }
  else {
    nms = c (v1, v2)
    lod = 
      log (x.p (j, setNames (c(1,1), nms))) + log (x.p (j, setNames (c(0,0), nms))) -
      log (x.p (j, setNames (c(0,1), nms))) - log (x.p (j, setNames (c(1,0), nms)))
    names (lod) = paste (v1, v2, sep = ':')
  }
  names (lod) = paste (names (lod), givens.str (gx), sep = '')
  lod
}   

vs.phi = function (j, v1 = NULL, v2 = NULL, gx = NULL) {
  stopifnot (is.null (v1) == is.null (v2))
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  if (is.null (v1)) {
    pairs = joint.v.pairs (j, F); lod = NULL
    if (length (pairs) > 0) {
      phis = sapply (pairs, function (p) vs.phi (j, p[1], p[2]))
      names (phis) = sapply (pairs, function (p) paste (p[1], p[2], sep = ':'))
    }
  }
  else {
    nms = c (v1, v2)
    cells = c (x.p (j, setNames (c(1,1), nms)), x.p (j, setNames (c(0,1), nms)), x.p (j, setNames (c(1,0), nms)), x.p (j, setNames (c(0,0), nms)))
    phis = phi (cells, digits=6)
    names (phis) = paste (v1, v2, sep = ':')
  }
  names (phis) = paste (names (phis), givens.str (gx), sep = '')
  phis
}   

# Compute the log odds ratio (or "association") between selected variable triplet.
# If v1, v2, and v3 are null, lods between all variable pairs are returned.
# (Assumes variables are binary.)
lod.3 = function (j, v1 = NULL, v2 = NULL, v3 = NULL, gx = NULL) {
  stopifnot (is.null (v1) == is.null (v2), is.null (v2) == is.null (v3))
  if (!is.null (gx)) j = joint.conditionalized (j, gx)
  if (is.null (v1)) {
    triples = joint.v.triples (j, F); lod = NULL
    if (length (triples) > 0) {
      lod = sapply (triples, function (t) lod.3 (j, t[1], t[2], t[3]))
      names (lod) = sapply (triples, function (t) paste (t[1], t[2], t[3], sep = ':'))
    }
  }
  else {
    # Compute three-way associations for selected variables (see Agresti, 2002, p. 322). 
    u111 = v.p (j, v1, setNames (c(1,1), c(v2,v3))); u211 = 1 - u111
    u112 = v.p (j, v1, setNames (c(1,0), c(v2,v3))); u212 = 1 - u112
    u121 = v.p (j, v1, setNames (c(0,1), c(v2,v3))); u221 = 1 - u121
    u122 = v.p (j, v1, setNames (c(0,0), c(v2,v3))); u222 = 1 - u122
    lod = 
      ((log (u111) + log (u221)) - (log (u121) + log (u211))) - 
      ((log (u112) + log (u222)) - (log (u122) + log (u212))) 
  }
  lod
}   

# Causal power (unconditional) between two variables.
vs.causal.power = function (j, c, e) {
  j = joint.marginalized (j, c (c, e))
  causal.power (j['11',]$p, j['10',]$p, j['01',]$p, j['00',]$p)
}  

# Causal power conditioned on all other variables = 0.
vs.causal.power.0 = function (j, c, e) {
  others = setdiff (joint.vs (j), c (c, e))
  j = joint.conditionalized (j, setNames (rep (0, length (others)), others))
  vs.causal.power (j, c, e)
}  

#############################################################################
# Query a joint.
# Returns all joint and marginal probabilities and a subset of conditional probabilities (cps).
#   All cps are returned where the number of conditioned on variables <= max.conditioners.
#   And, all cps specified in list cp.qs are returned. For each eleiment of cp.qs:
#     First element is a vector of targe variable names (vs).
#     Second is vector of variables names upon which each of the targets is to be conditioned on.
#     (those variables are instantiated with values in all possible ways).
# Assumes binary variables.
#############################################################################
joint.query = function (j, max.conditioners=1, cp.qs=NULL) {
  joint.query.driver (j, joint.query.skeleton (j, max.conditioners, cp.qs))
}

# Returns a structure that can then be combined with a joint in order to produce a query. 
# The structure pre-computes info needed for the query. By re-using it, repeated queries 
# are much faster.
joint.query.skeleton = function (j, max.conditioners=1, cp.qs=NULL) {
  # Returns list of variable/conditioner pairs that will need to be queried.
  standard.cp.qs = function (max.conditioners) {
    if (max.conditioners > 0) {
      # All possible variable subsets to condition on, filtered by max.conditioners.
      stopifnot (max.conditioners < length (joint.vs (j)))
      gvs.list = powerset (joint.vs (j))
      gvs.list = gvs.list [lengths (gvs.list) <= max.conditioners]
      # For each subset, generate a query for the cp of each variable not in the subset
      lapply (gvs.list, function (gvs) list (setdiff (joint.vs (j), gvs), gvs))      
    }
    else NULL
  }
  expand1.cp = function (cp.q) {
    antecedent1 = function (gx) {
      consequent1 = function (v) {
        num = c (setNames (1, v), gx)
        label = paste (v, gx.s, sep='|') # Unique label for the query.
        list (num=num, gx=gx, gx.s=gx.s, label=label)
      }
      # Form a query for each to-be-predicted variable (consequent).
      gx = unlist (gx); gx.s = givens.str.compact (gx)
      llply (vs, consequent1)
    }
    vs = cp.q[[1]]; gvs = cp.q[[2]]
    # Instantiate the conditioners (the antecedents) with all possible values.
    cp.q = unlist (dlply (xs.of.vs (gvs), gvs, antecedent1), recursive=F) 
    names (cp.q) = lapply (cp.q, function (q) q$label)
    cp.q
  }
  
  # Set up the three different types of queries.
  qu.j = data.frame (q = rownames (j)); qu.j$qtype = 'j' # All joint probabilities.
  qu.m = data.frame (q = joint.vs (j)); qu.m$qtype = 'm' # All marginal probabilities.
  qu = rbind (qu.j, qu.m)
  
  # Indices for all the (single variable) marginal probabilities.
  ms.idx = lapply (joint.vs (j), function (v) which (joint.idx (j, setNames (1, v))))
  
  # Combine standard cp queries (specified by max.conditioners) with any requested by the caller.
  cp.qs = c (standard.cp.qs (max.conditioners), cp.qs)
  if (!is.null (cp.qs)) {
    # Instantiate each cp query type with different values.
    cp.qs = unlist (lapply (cp.qs, expand1.cp), recursive=F)
    # Identify all the sets of "given" variables.
    gxs = lapply (cp.qs, function (cp.q) cp.q$gx)
    names (gxs) = lapply (gxs, givens.str.compact)
    # Delete duplicates, for efficiency.
    gxs = gxs[!duplicated (names (gxs))]
    gxs.idx = lapply (gxs, function (x) joint.idx (j, x))
    numr = lapply (cp.qs, function (cp.q) {
      idx = which (joint.idx (j, cp.q$num))
      gx.i = which (cp.q$gx.s == names (gxs.idx))
      list (idx=idx, gx.i=gx.i)
    })
    qu.c = data.frame (q=names (numr))
    qu.c$qtype = 'c' # The conditional probabilities in cp.qs
    qu = rbind (qu, qu.c)
  }
  else 
    numr = gxs.idx = NULL
  
  stopifnot (!any (duplicated (qu)))
  rownames (qu) = interaction (qu$qtype, qu$q)  
  list (qu=qu[,c ('qtype','q')], j.rownames=rownames (j), ms.idx=ms.idx, numr=unname (numr), gxs.idx=unname (gxs.idx))
} 

# Returns a query. (Qs is a structure produced by joint.query.skeleton.)  
joint.query.driver = function (j, qs) {
  stopifnot (all (rownames (j) == qs$j.rownames))
  # Compute the marginals.
  ms = sapply (qs$ms.idx, function (idx) sum (j$p[idx]))
  if (!is.null (qs$numr)) {
    # Conditionalize the joint for each set of "given" variables.
    ps.per.gxs = lapply (qs$gxs.idx, function (idx) { p = j$p; p[!idx] = 0; p / sum (p) } )
    # Compute the conditional probabilities themselves from the conditionalized joints.
    cps = sapply (qs$numr, function (n) sum (ps.per.gxs[[n$gx.i]][n$idx]))
  }
  else
    cps = NULL
  # Form the data frame representing the query.
  qu = qs$qu; qu$y.hat = c (j$p, ms, cps)
  if (any (is.na (qu$y.hat))) {
    print (qu)
    cat ('NAs detected in joint.query.driver\n')
    stopifnot (all (!is.na (qu$y.hat)))
  }
  qu
}   

# Returns a data frame in which all combinations of the values of the variables in vs have been instantiated.
# Variables are binary by default. If states is a list, then its elements represent the states for each variable in vs.
xs.of.vs = function (vs, states = binary.states) { 
  if (is.list (states))
    stopifnot (length (states) == length (vs))
  else 
    states = rep (list (states), length (vs))
  stopifnot (all (is.character (vs)), all (sapply (states, function (s) length (s) > 0)))
  # Create data frame representing joint (flip columns to put rows in canonical order).
  setNames (data.frame (do.call (expand.grid, rev (states))[,length(vs):1]), vs)
}

# Create the skeleton of a joint (columns for variables but no p column). 
joint.skeleton = function (vs, states = binary.states) {
  stopifnot (!'p' %in% vs) # Column 'p' represents the probability, so can't be a variable.
  joint.w.rownames (xs.of.vs (vs, states))
}   

# Create a joint in which every partial state is represented explicitly.
power.set.of.joint = function (j) {
  ej = expand.grid (lapply (joint.vs.states (j), function (s) c (s, NA)))
  ej$p = apply (ej, 1, function (x) { x.p (j, x[!is.na (x)]) } )
  rownames (ej) = interaction.w.nas (ej[,joint.vs (ej)])
  ej
}

# Merge (multiply) joints together, matching on common variable names.
merge.joints = function (js, normalize = TRUE) {
  if (length (js) == 1) j = js[[1]]
  else if (length (js) == 2) { 
    common.vs = intersect (joint.vs (js[[1]]), joint.vs (js[[2]]))
    # Join up by common variables. If none are present, computes Cartesian product.
    j = merge (js[[1]], js[[2]], by = common.vs) 
    j$p = j$p.x * j$p.y; j$p.x = j$p.y = NULL
  }
  else j = merge.joints (list (js[[1]], merge.joints (js[-1])), normalize) # Recurse
  if (normalize) j = joint.normalized (j)
  joint.sorted (joint.w.rownames (j))
}     

# Joint together a bunch of unrelated joints (joints with no variables in common).
merge.independent.joints = function (js) {
  merge.joints (js, normalize = F) # No need to normalize if independent.
}     

# Integrate joints together. Applies chain rule, back to front, according to order in js.
integrate.joints = function (js) {
  p = function (x) x.p (j.y, x[others.y], gx = x[vs.common]) * x['p']
  if (length (js) == 1) j = js[[1]]
  else if (length (js) == 2) { 
    j.x = js[[1]]; j.y = js[[2]]; vs.x = joint.vs (j.x); vs.y = joint.vs (j.y)
    vs.common = intersect (vs.x, vs.y); others.y = setdiff (vs.y, vs.common)
    stopifnot (length (vs.common) > 0)
    j = merge (j.x, j.y[vs.y], by = vs.common)[c (vs.x, others.y, 'p')] # Join up by common variables.
    j$p = apply (j, MARGIN = 1, p)
  }
  else j = integrate.joints (list (js[[1]], integrate.joints (js[-1]))) # Recurse
  joint.sorted (joint.w.rownames (j))
}     


# Returns a joint with independent variables.
# Pvs is a list of probability vectors, one for each variable, that encode the probability of each variable state.
# The names of the vector are the variable's values. If NULL, then values are numeric: 0:(length (pvs[[vi]]) - 1).
# If pvs is a numeric vector, then variables are assumed to be binary (0, 1) and pvs[vi] is the probability that vi is present (= 1).
# Variables names are names (pvs). If NULL, they are generated. 
independent.joint = function (pvs) {
  states = function (pv) { # Derive states of variable from its probability vector.
    stopifnot (length (pv) > 1, is.numeric (pv), all (pv >= 0), all (pv <= 1), sum (pv) == 1)
    if (is.null (names (pv))) 0:(length (pv) - 1) else names (pv) # Either numeric (starting at 0) or characters.
  }
  stopifnot (length (pvs) > 0)
  if (!is.list (pvs)) { stopifnot (is.vector (pvs)); pvs = lapply (as.list (pvs), function (p) c(1 - p, p)) } # For binary variables.
  if (is.null (names (pvs))) names (pvs) = rev (rev (letters) [1:length (pvs)]) # Generate variables names.
  # Create a joint for each variable and then merge them. 
  js = mapply (function (v, pv) { j = joint.skeleton (v, states (pv)); j$p = pv; j }, names (pvs), pvs, SIMPLIFY = FALSE)
  merge.independent.joints (js)
}

# Returns list of independence relations between v and its siblings in j.
# List elements reflect independence conditioned on every combination of the variables 
# excluding v and siblings. 
#   If all.indep is true, then independence holds for all values of the conditioned-on variables. 
#   Else the values of the conditioned-on variables for which independence holds is in indep.xs
# Conditioned-on variables that yield no independence relations are omitted from the list.
# (Variables are assumed to be binary.)
v.irels = function (j, v, siblings = NULL, max.conditioners = Inf) {
  irelations = function (sibling) {
    irelation = function (gs) {
      relation.indep = function (gx) zero.within.tol (lod (j, v, sibling, gx = gx))
      indep.xs = data.frame ()
      if (length (gs) == 0) all.indep = zero.within.tol (lod (j, v, sibling))
      else {
        gxs = xs.of.vs (gs)
        # Test if pairs are independent for all possible combination of values of g 
        xs.indep = apply (gxs, MARGIN = 1, relation.indep)
        all.indep = all (xs.indep); indep.xs = data.frame (gxs [xs.indep,])
      }
      list (conditioned.on = gs, all.indep = all.indep, indep.xs = indep.xs)
    }
    # All subsets of variables not in union (v, sibling). 
    others = powerset (setdiff (joint.vs (j), c(v, sibling)), nonempty = FALSE)
    others = others [sapply (others, function (o) length (o) <= max.conditioners)]
    # Get relations for this sibling.
    irels = llply (as.list (others), irelation, .parallel = FALSE) 
    # Filter out v/sibling/other combinations with no independence relations.
    irels = irels [sapply (irels, function (i) i$all.indep | nrow (i$indep.xs) > 0)]
    list (sibling = sibling, irels = irels)
  }
  if (is.null (siblings)) siblings = setdiff (joint.vs (j), v)
  # Get independence relations with each sibling
  irels = llply (as.list (siblings), irelations, .parallel = FALSE)
  irels [sapply (irels, function (i) length (i$irels) > 0)]
}

# Returns list of independence relations for each pair of variables in j.
joint.irels = function (j, max.conditioners = Inf) {
  pair.irels = function (p) {
    irels = v.irels (j, p[1], p[2], max.conditioners)
    list (p = p, irels = if (length (irels) > 0) irels[[1]]$irels else NULL)
  }
  irels = lapply (joint.v.pairs (j, F), pair.irels)
  irels [sapply (irels, function (i) !is.null (i$irels))] # Remove pairs w no relations.
}

###############################################
# Generic "information gain" routine.
# (Variables are assumed to be binary.)
###############################################
# Expected gain on variable v if variables vs are observed.
expected.gain = function (j, v, vs, gain.fun) {
  sum (apply (xs.of.vs (vs), 1, function (x) x.p (j, x) * do.call (gain.fun, list (j, v, x))))
}  

###############################################
# "Information gain" measures.
###############################################
# Information gain on v if state x is observed.
info.gain = function (j, v = NULL, x) {
  v.entropy (j, v) - v.entropy (joint.conditionalized (j, x), v)
}  
  
# Expected information gain on variable v if vs are observed.
expected.info.gain = function (j, v = NULL, vs) { expected.gain (j, v, vs, info.gain) }  

###############################################
# "Probability gain" measures.
###############################################
# "Probability gain" on v if state x is observed.
prob.gain = function (j, v, x) {
  # The probability of guessing v correctly.
  max.prob = function (j, v) { p = x.p (j, v); ifelse (p > .5, p, 1 - p ) }  
  max.prob (joint.conditionalized (j, x), v) - max.prob (j, v)
}  

# Expected probability gain on variable v if variables vs are observed.
expected.prob.gain = function (j, v, vs) { expected.gain (j, v, vs, prob.gain) }  
expected.prob.gain.biased = function (j, v, vs) { prob.gain (j, v, vs, 1) }  

###############################################
# "Logit gain" measures.
###############################################
# "Logit gain" on variable v if state x is observed.
logit.gain = function (j, v, x) {
  max.logit = function (j) {
    p = v.p (j, v); p = ifelse (p > 1, 1, ifelse (p < 0, 0, p))
    abs (logit (p))
  }
  max.logit (joint.conditionalized (j, x)) - max.logit (j)
}  

# Expected logit gain on variable v if variables vs are observed.
expected.logit.gain = function (j, v, vs) { expected.gain (j, v, vs, logit.gain) }  

###############################################
# "Power" measures.
###############################################
# "Power gain" on variable v if state x is observed.
power.gain = function (j, v, x, lambda) {
  max.power = function (j) {
    p = v.p (j, v); p = ifelse (p > 1, 1, ifelse (p < 0, 0, p))
    p.pow = 1 / (1 + ((1 - p)**lambda / p**lambda) )
    ifelse (p.pow > .5, p.pow, 1 - p.pow )
  }
  max.power (joint.conditionalized (j, x)) - max.power (j)
}  

# Expected probability gain on variable v if variables vs are observed.
expected.power.gain = function (j, v, vs, lambda) {
  p = v.p (j, vs)
  #cat ("  Gain on", v, "if", vs, "equals 1 =", v.prob.gain (j, v, vs, 1), ", gain if it equals 0 =", v.prob.gain (j, v, vs, 0),"\n")
  power.gain (j, v, vs, 1, lambda) * p + power.gain (j, v, vs, 0, lambda) * (1 - p)
}  

###############################################
# "Impact" measures.
###############################################
# "Impact" on variable v if state x is observed.
impact = function (j, v, x) {
  abs (v.p (joint.conditionalized (j, x), v) - v.p (j, v))
}  

# Expected impact on variable v if variables vs are observed.
expected.impact = function (j, v, vs) { expected.gain (j, v, vs, impact) }  
