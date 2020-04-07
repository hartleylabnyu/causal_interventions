givens.str = function (gx) {
  gx.s = ''
  if (!is.null (gx)) { 
    gx.s = paste ('|', do.call (paste, as.list (mapply (function (n, g) { paste (n, '=', g, sep='') }, names (gx), gx))), sep='') 
  }
  gx.s
}

givens.str.compact = function (gx) {
  paste (mapply (function (gv, gx) paste (gv, gx, sep = ''), names (gx), gx), collapse = '.')
}

screening.off = function (j, cc, e1, e2) {
  j = joint.conditionalized (j, setNames (1, cc))
  cps = c (v.p (j, e1, setNames (1, e2)), v.p (j, e1), v.p (j, e1, setNames (0, e2)))
  nms = paste ('p(', e1, '|', cc, '=1,', e2, '=', sep = '')
  names (cps) = paste (nms, c('1)', 'NA)', '0)'), sep = '')
  cps
}

explaining.away = function (j, ce, c1, c2) {
  j = joint.conditionalized (j, setNames (1, ce))
  cps = c (v.p (j, c1, setNames (1, c2)), v.p (j, c1), v.p (j, c1, setNames (0, c2)))
  nms = paste ('p(', c1, '|', ce, '=1,', c2, '=', sep = '')
  names (cps) = paste (nms, c('1)', 'NA)', '0)'), sep = '')
  cps
}

print.joint.stats = function (j, conditionals = T) {
  print.2.way.conds = function (t) { 
    print.2.way.cond = function (p) { 
      g = setdiff (t, p)
      c (lod (j, p[1], p[2], gx = setNames (1, g)), lod (j, p[1], p[2], gx = setNames (0, g)))
    }
    print (unlist (combn (t, 2, print.2.way.cond, simplify = F)))
  }
  vs = joint.vs (j)
  print (v.p (j))
  if (length (vs) > 1) {
    print (lod (j))
    if (length (vs) > 2) {
      print (lod.3 (j))
      if (conditionals) sapply (joint.v.triples (j, F), print.2.way.conds)
    }
  }
  return ()
}

print.joint.stats.verbose = function (j, level = 3, zero.assoc = TRUE) {
  print.marginal   = function (v)          { cat ('*** Marginal:', v, v.p (j, v), "\n")}
  print.2.way = function (v1, v2) { 
    log.odds = lod (j, v1, v2)
    if ((!equal.within.tol (log.odds, 0)) | zero.assoc) {
      cat ('*** 2-way association [marginal]:', v1, v2, log.odds, "[", exp (log.odds), "]\n")
    }
  }
  print.3.way = function (v1, v2, v3) { 
    log.odds = (lod.3 (j, v1, v2, v3))
    if (!equal.within.tol (log.odds, 0) | zero.assoc) {
      cat ('*** 3-way association:', v1, v2, v3, log.odds, "\n")
    }
  }
  print.2.way.cond.set = function (v1, v2, v3) { 
    print.2.way.cond = function (v1, v2, v3) { 
      print.cond = function (v3.val) { 
        log.odds = lod (j, v1, v2, gx = setNames (v3.val, v3))
        cat ('*** 2-way association [', v3, '=', v3.val, ']:', v1, v2, log.odds, "[", exp (log.odds), "]\n")
      }
      print.cond (1); print.cond (0)
    }
    print.2.way.cond (v1, v2, v3)
    print.2.way.cond (v1, v3, v2)
    print.2.way.cond (v2, v3, v1)
  }
  vs = joint.vs (j)
  sapply (vs, print.marginal)
  if ((level > 1) & (length (vs) > 1)) {
    pairs = joint.v.pairs (j, F)
    sapply (pairs, function (p) print.2.way (p[1], p[2]))
    if ((level > 2) & (length (vs) > 2)) {
      triples = joint.v.triples (j, F)
      sapply (triples, function (t) { print.3.way (t[1], t[2], t[3]); print.2.way.cond.set (t[1], t[2], t[3]) })
    }
  }
}

print.irel = function (irel, v1, v2) {
  cat (v1, 'indep', v2)
  if (length (irel$conditioned.on) > 0) {
    cat (' conditioned on', irel$conditioned.on, '')
    if (!irel$all.indep) { apply (irel$indep.xs, MARGIN = 1, function (xs) cat ('[', unlist (xs), '] ')) }
  }
  cat ("\n")
}

print.irels = function (j, v = NULL, siblings = NULL, max.conditioners = Inf) {
  print.sibling.irels = function (sibling) lapply (sibling$irel, print.irel, v, sibling$sibling)
  print.irels.of.pair = function (pair) lapply (pair$irel, print.irel, pair$p[1], pair$p[2])
  if (is.null (v)) lapply (joint.irels (j, max.conditioners), print.irels.of.pair)
  else lapply (v.irels (j, v, siblings, max.conditioners), print.sibling.irels)
  return ()
}

