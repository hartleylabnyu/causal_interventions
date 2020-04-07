#############################################################################
# Routines that return joint distributions for causal graphical models (cgms).
#
# Naming conventions:
#   c - Parameter representing the base rate of a cause.
#   m - Parameter representing the strength, or power, of a causal relationship.
#   b - Paremeter representing the strength of some unspecified alternative causes of a variable.
#
# Most cgms specify a noisy-or integration function (with some exceptions)
#
# Most are accompanied by query routines that return answers to different questions 
# about a joint:
#     c queries: conditional probability
#     m queries: marginal probability
#     j queries: joint probability (entries in the joint itself)
#############################################################################

if(0){
    setwd("/Users/zach/Google Drive/Code/R")
    source ("hw.r")
    source ("print.lib.r")
    source ("lib.r")
    source ("plib.r")
    source ("cgm.r")
    source ("quad.cgm.r")
    require(plyr)
}

# Properties of each cgm parameter. (Row name is parameter name.)
min.cgm.p = 1e-10; max.cgm.p = 1 - min.cgm.p
cgm.p.range = c (low = min.cgm.p, high = max.cgm.p)      
#cgm.params = list (c   = list (low = p.range$low, hi = p.range$high, init = c (.25, .50, .75), logit=TRUE),
#                   m   = list (low = p.range$low, hi = p.range$high, init = c (.25, .50, .75), logit=TRUE),
#                   b   = list (low = p.range$low, hi = p.range$high, init = c (.10, .30),      logit=TRUE),
#                   y   = list (low = p.range$low, hi = p.range$high, init = c (.10, .30),      logit=TRUE),
#                   w   = list (low = p.range$low, hi = 10,           init = c (1, 3, 5)),
#                   w0  = list (low = -10,         hi = 10,           init = c (-3, -2, -1)),
#                   u   = list (low = p.range$low, hi = p.range$high, init = c (.25, .50, .75), logit=TRUE),
#                   um  = list (low = p.range$low, hi = p.range$high, init = c (.50),           logit=TRUE))
#cgm.params.debug = list (c   = list (low = p.range$low, hi = p.range$high, init = c (.5)),
#                         m   = list (low = p.range$low, hi = p.range$high, init = c (.5)),
#                         b   = list (low = p.range$low, hi = p.range$high, init = c (.10)),
#                         i   = list (low = p.range$low, hi = p.range$high, init = c (.50)),
#                         y   = list (low = p.range$low, hi = p.range$high, init = c (.50)),
#                         u   = list (low = p.range$low, hi = p.range$high, init = c (.50)),
#                         um  = list (low = p.range$low, hi = p.range$high, init = c (.50)),
#                         st  = list (low = 1,           hi = 100,          init = c (2)))


#############################################################################
# Basic functions that return the probability of an effect given the state
# of its causes for several types of integration functions. Larger networks are
# constructed out of these basic units.
#############################################################################
var.p = function (v, p) ifelse (v == 1, p, 1 - p) 

# Probability of an effect given a single cause as a function of parameters m and b.
e0.given.c = function (m, b, c.state)    { (1 - m)**c.state * (1 - b)     }  # p(e=0|c.state)
e1.given.c = function (m, b, c.state)    { 1 - e0.given.c (m, b, c.state) }  # p(e=1|c.state)
e.given.c  = function (e, m, b, c.state) { ifelse (e == 1, e1.given.c (m, b, c.state), e0.given.c (m, b, c.state)) }

# Probability of an effect given a single cause as a function of parameters w and b.
e0.given.c.linear = function (w, b, c.state)    { 1 - e1.given.c.linear (w, b, c.state) }  # p(e=0|c.state)
e1.given.c.linear = function (w, b, c.state)    { 1 / (1 + exp (-(w*c.state + b))) }       # p(e=1|c.state)
e.given.c.linear  = function (e, w, b, c.state) { ifelse (e == 1, e1.given.c.linear (w, b, c.state), e0.given.c.linear (w, b, c.state)) }

# Probability of an effect given a single cause and a disabler.
e0.given.cd = function (m, d.m, b, c.state, d.state)    { 
  e0.wo.d = e0.given.c (m, b, c.state); e0.w.d = e0.given.c (0, b, c.state)
  ifelse (d.state == 1, d.m * e0.w.d + (1 - d.m) * e0.wo.d, e0.wo.d) 
}
e1.given.cd = function (m, d.m, b, c.state, d.state)    { 1 - e0.given.cd (m, d.m, b, c.state, d.state) }  
e.given.cd  = function (e, m, d.m, b, c.state, d.state) { ifelse (e == 1, e1.given.cd (m, d.m, b, c.state, d.state), e0.given.cd (m, d.m, b, c.state, d.state)) }

# Probability of an effect given two independent causes (ica and icb) as a function of parameters ma, mb, and b (fuzzy or integration rule).
e0.given.ic2 = function (ma, mb, b, ica, icb)    { (1 - ma)**ica * (1 - mb)**icb * (1 - b) }  # p(e=0|ica,icb)
e1.given.ic2 = function (ma, mb, b, ica, icb)    { 1 - e0.given.ic2 (ma, mb, b, ica, icb)  }  # p(e=1|ica,icb)
e.given.ic2  = function (e, ma, mb, b, ica, icb) { ifelse (e == 1, e1.given.ic2 (ma, mb, b, ica, icb), e0.given.ic2 (ma, mb, b, ica, icb)) }

# Probability of an effect given two independent causes (ica and icb) as a function of parameters ma, mb, and b (dual link interpretation).
e0.given.ic2.dual = function (ma, mb, b, ica, icb)    { ifelse (ica != icb, .50, ifelse (ica == 1, e0.given.ic2 (ma, mb, b, 1, 1), 1 - e0.given.ic2 (ma, mb, b, 1, 1))) }  # p(e=0|ica,icb)
e1.given.ic2.dual = function (ma, mb, b, ica, icb)    { 1 - e0.given.ic2.dual (ma, mb, b, ica, icb)  }  # p(e=1|ica,icb)
e.given.ic2.dual  = function (e, ma, mb, b, ica, icb) { ifelse (e == 1, e1.given.ic2.dual (ma, mb, 0, ica, icb), e0.given.ic2.dual (ma, mb, 0, ica, icb)) }

# Probability of an effect given two independent causes (ica and icb) as a function of parameters wa, wb, and b (linear combination rule).
e0.given.ic2.linear = function (wa, wb, b, ica, icb)    { 1 - e1.given.ic2.linear (wa, wb, b, ica, icb)  }  # p(e=0|ica,icb)
e1.given.ic2.linear = function (wa, wb, b, ica, icb)    { 1 / (1 + exp (-(wa*ica + wb*icb + b))) }      # p(e=1|ica,icb)
e.given.ic2.linear  = function (e, wa, wb, b, ica, icb) { ifelse (e == 1, e1.given.ic2.linear (wa, wb, b, ica, icb), e0.given.ic2.linear (wa, wb, b, ica, icb)) }

# Probability of an effect given three independent causes (ica, icb, and icc) as a function of parameters ma, mb, mc, and b.
e0.given.ic3 = function (ma, mb, mc, b, ica, icb, icc)    { (1 - ma)**ica * (1 - mb)**icb * (1 - mc)**icc * (1 - b) }  # p(e=0|ica,icb,icc)
e1.given.ic3 = function (ma, mb, mc, b, ica, icb, icc)    { 1 - e0.given.ic3 (ma, mb, mc, b, ica, icb, icc)  }         # p(e=1|ica,icb,icc)
e.given.ic3  = function (e, ma, mb, mc, b, ica, icb, icc) { ifelse (e == 1, e1.given.ic3 (ma, mb, mc, b, ica, icb, icc), e0.given.ic3 (ma, mb, mc, b, ica, icb, icc)) }

# Probability of an effect given three independent causes (ica, icb, and icc) as a function of parameters ma, mb, mc, and b.
e0.given.ic3.linear = function (wa, wb, wc, b, ica, icb, icc)    { 1 - e1.given.ic3.linear (wa, wb, wc, b, ica, icb, icc) }  # p(e=0|ica,icb,icc)
e1.given.ic3.linear = function (wa, wb, wc, b, ica, icb, icc)    { 1 / (1 + exp (-(wa*ica + wb*icb + wc*icc + b))) }  # p(e=1|ica,icb,icc)
e.given.ic3.linear  = function (e, wa, wb, wc, b, ica, icb, icc) { ifelse (e == 1, e1.given.ic3.linear (wa, wb, wc, b, ica, icb, icc), e0.given.ic3.linear (wa, wb, wc, b, ica, icb, icc)) }

# Probability of an effect given two independent causes (ica and icb) with a shared "enabler" as a function of parameters ma, mb, and b.
e0.given.ic2en1 = function (ma, mb, b, ica, icb, en)    { (1 - ma)**(ica*en) * (1 - mb)**(icb*en) * (1 - b) }  # p(e=0|ica,icb,en)
e1.given.ic2en1 = function (ma, mb, b, ica, icb, en)    { 1 - e0.given.ic2en1 (ma, mb, b, ica, icb, en)  }       # p(e=1|ica,icb,en)
e.given.ic2en1  = function (e, ma, mb, b, ica, icb, en) { ifelse (e == 1, e1.given.ic2en1 (ma, mb, b, ica, icb, en), e0.given.ic2en1 (ma, mb, b, ica, icb, en)) }

# Probability of an effect given four independent causes as a function of parameters.
e0.given.ic4 = function (ma, mb, mc, md, b, ica, icb, icc, icd)    { (1 - ma)**ica * (1 - mb)**icb * (1 - mc)**icc * (1 - md)**icd * (1 - b) }  # p(e=0|ica,icb,icc,icd)
e1.given.ic4 = function (ma, mb, mc, md,b, ica, icb, icc, icd)     { 1 - e0.given.ic4 (ma, mb, mc, md, b, ica, icb, icc, icd)  }                # p(e=1|ica,icb,icc,icd)
e.given.ic4  = function (e, ma, mb, mc, md, b, ica, icb, icc, icd) { ifelse (e == 1, e1.given.ic4 (ma, mb, mc, md,b, ica, icb, icc, icd), e0.given.ic4 (ma, mb, mc, md, b, ica, icb, icc, icd)) }

# Probability of an effect given n independent causes as a function of parameters.
e0.given.icn = function (ms, b, ics) { 
  if (length (ms) == 0) 1 - b
  else (1 - b) * prod (mapply (function (m, ic) {(1 - m)**ic}, ms, ics)) 
}  
e1.given.icn = function (ms, b, ics)    { 1 - e0.given.icn (ms, b, ics)  }                
e.given.icn  = function (e, ms, b, ics) { ifelse (e == 1, e1.given.icn (ms, b, ics), e0.given.icn (ms, b, ics)) }

# Probability of an effect given two conjunctive causes (cca and ccb) as a function of parameters m and b.
e0.given.cc2 = function (m, b, cca, ccb)    { ifelse (cca == 1 & ccb == 1, (1 - m), 1) * (1 - b) }  # p(e=0|cca,ccb)
e1.given.cc2 = function (m, b, cca, ccb)    { 1 - e0.given.cc2 (m, b, cca, ccb)                  }  # p(e=1|cca,ccb)
e.given.cc2  = function (e, m, b, cca, ccb) { ifelse (e == 1, e1.given.cc2 (m, b, cca, ccb), e0.given.cc2 (m, b, cca, ccb)) }

# Probability of an effect given two complex causes (xca and xcb) that have both an independent and conjuctive effect.
e0.given.xc2 = function (ma, mb, mcc, b, xca, xcb)    { (1 - ma)**xca * (1 - mb)**xcb * ifelse (xca == 1 & xcb == 1, (1 - mcc), 1) * (1 - b) }  
e1.given.xc2 = function (ma, mb, mcc, b, xca, xcb)    { 1 - e0.given.xc2 (ma, mb, mcc, b, xca, xcb)                  }  
e.given.xc2  = function (e, ma, mb, mcc, b, xca, xcb) { ifelse (e == 1, e1.given.xc2 (ma, mb, mcc, b, xca, xcb), e0.given.xc2 (ma, mb, mcc, b, xca, xcb)) }

# Probability of an effect given two conjunctive causes (cca and ccb) as a function of parameters w and b.
e0.given.cc2.linear = function (w, b, cca, ccb)    { 1 - e1.given.cc2.linear (w, b, cca, ccb)  }  # p(e=0|cca,ccb)
e1.given.cc2.linear = function (w, b, cca, ccb)    { 1 / (1 + exp (-(ifelse (cca == 1 & ccb == 1, w, 0) + b))) }     # p(e=1|cca,ccb)
e.given.cc2.linear  = function (e, w, b, cca, ccb) { ifelse (e == 1, e1.given.cc2.linear (w, b, cca, ccb), e0.given.cc2.linear (w, b, cca, ccb)) }

# An effect conditioned on three causes, where the last two (cca and ccb) are conjunctive (as a function of parameters mi, mc, and b).
e0.given.ic1cc2 = function (mi, mc, b, ic, cca, ccb)    { (1 - mi)**ic * ifelse (cca == 1 & ccb == 1, (1 - mc), 1) * (1 - b) }  # p(e=0|ic,cca,ccb)
e1.given.ic1cc2 = function (mi, mc, b, ic, cca, ccb)    { 1 - e0.given.ic1cc2 (mi, mc, b, ic, cca, ccb)                      }  # p(e=1|ic,cca,ccb)
e.given.ic1cc2  = function (e, mi, mc, b, ic, cca, ccb) { ifelse (e == 1, e1.given.ic1cc2 (mi, mc, b, ic, cca, ccb), e0.given.ic1cc2 (mi, mc, b, ic, cca, ccb)) }

# An effect conditioned on three causes, where the last two (cca and ccb) are conjunctive (as a function of parameters mi, mc, and b).
e0.given.ic1cc2.linear = function (wi, wc, b, ic, cca, ccb)    { 1 - e1.given.ic1cc2.linear (wi, wc, b, ic, cca, ccb) }                 # p(e=0|ic,cca
e1.given.ic1cc2.linear = function (wi, wc, b, ic, cca, ccb)    { 1 / (1 + exp (-(wi*ic + ifelse (cca == 1 & ccb == 1, wc, 0) + b))) }  # p(e=1|ic,cca,ccb)
e.given.ic1cc2.linear  = function (e, wi, wc, b, ic, cca, ccb) { ifelse (e == 1, e1.given.ic1cc2.linear (wi, wc, b, ic, cca, ccb), e0.given.ic1cc2.linear (wi, wc, b, ic, cca, ccb)) }

# An effect conditioned on four causes consisting of a pair of conjunctive causes (as a function of parameters ma, mb, and b).
e0.given.double.cc = function (ma, mb, b, cca1, cca2, ccb1, ccb2)    { ifelse (cca1 == 1 & cca2 == 1, (1 - ma), 1) * ifelse (ccb1 == 1 & ccb2 == 1, (1 - mb), 1) * (1 - b) }  # p(e=0|cca1,cca2,ccb1,ccb2)
e1.given.double.cc = function (ma, mb, b, cca1, cca2, ccb1, ccb2)    { 1 - e0.given.double.cc (ma, mb, b, cca1, cca2, ccb1, ccb2)                                          }  # p(e=1|cca1,cca2,ccb1,ccb2)
e.given.double.cc  = function (e, ma, mb, b, cca1, cca2, ccb1, ccb2) { ifelse (e == 1, e1.given.double.cc (ma, mb, b, cca1, cca2, ccb1, ccb2), e0.given.double.cc (ma, mb, b, cca1, cca2, ccb1, ccb2)) }

# An effect conditioned on two conjunctive pairs and one independent cause.
e.given.ic1cc2cc2  = function (e, mi, mca, mcb, b, ic, cca1, cca2, ccb1, ccb2) { 
  icb = ifelse (cca1 == 1 & cca2 == 1, 1, 0)
  icc = ifelse (ccb1 == 1 & ccb2 == 1, 1, 0)
  e.given.ic3 (e, mi, mca, mcb, b, ic, icb, icc)
}  

#############################################################################
# Generic routine for arbitrary network of (independent, generative) relations.
#   ms - square matrix of causal strengths (rows must be named, no cycles)
#   bs - background causes of each variable (appear in same order as in ms)
#############################################################################
joint.cgm.generic = function (ms, bs) {
  vs = rownames (ms); j = joint.skeleton (vs); j$p = 1
  joint.cgm.generic.driver (j, 1:length (vs), ms, bs)
}

# Same as joint.cgm.generic except that jr is the joint distribution of the root nodes.
joint.cgm.generic.correlated = function (jr, ms, bs) {
  vs = rownames (ms); stopifnot (all (joint.vs (jr) %in% vs))
  child.vis = which (!(vs %in% names (jr)))
  # Merge the joints for the children and the root nodes (must put columns back in caller's order).
  j = merge (jr, joint.skeleton (vs[child.vis]), by = NULL)[, c (vs, 'p')]
  joint.cgm.generic.driver (j, child.vis, ms, bs)
}

# Utility routine.
joint.cgm.generic.driver = function (j, child.vis, ms, bs) {
  ps.for.vi = function (i) {
    pi = ms[,i] > 0; ms = ms[,i][pi]; b = bs[i] # Parents of i, and the strengths of the links.
    apply (j[,joint.vs (j)], 1, function (x) e.given.icn (x[i], ms, b, x[pi]))      
  }
  stopifnot (nrow (ms) == length (bs), nrow (ms) == ncol (ms), ms >= 0, ms <= 1, all (diag (ms) == 0), bs >= 0, bs <= 1)
  # Compute ps for each child variable in each state.
  child.ps = laply (child.vis, ps.for.vi, .parallel = TRUE)
  # Multiply by the probability of the root nodes.
  j$p = j$p * apply (child.ps, 2, prod); stopifnot (equal.within.tol (sum (j$p), 1))
  joint.w.rownames (j)
}

query.cgm.generic = function (ms, bs, max.conditioners=1, cp.qs = NULL) {
  joint.query (joint.cgm.generic (ms, bs), max.conditioners, cp.qs)
}

#############################################################################
# One variable.
#############################################################################
joint.one.var = function (p) {
  j = data.frame (x = c (0, 1), p = c (1 - p, p))
  joint.w.rownames (j)
} 

#############################################################################
# One cause, one effect.
#############################################################################
j.cgm.c1e1 = joint.skeleton (c ("c", "e"))

joint.cgm.c1e1.ps = function (j, c, m, b) e.given.c (j$e, m, b, j$c) * var.p (j$c, c)

joint.cgm.c1e1 = function (c, m, b) {
  j.cgm.c1e1$p = joint.cgm.c1e1.ps (j.cgm.c1e1, c, m, b)
  j.cgm.c1e1
} 

joint.cgm.c1e1.linear = function (c, m, b) {
  j.cgm.c1e1$p = e.given.c.linear (j.cgm.c1e1$e, m, b, j.cgm.c1e1$c) * var.p (j.cgm.c1e1$c, c)
  j.cgm.c1e1
} 

q.cgm.c1e1 = joint.query.skeleton (j.cgm.c1e1)
query.cgm.c1e1.joint = function (j) joint.query.driver (j, q.cgm.c1e1)
query.cgm.c1e1       = function (c, m, b) query.cgm.c1e1.joint (joint.cgm.c1e2 (c, m, b))

#############################################################################
# GRAPHS WITH THREE VARIABLES.
#############################################################################

#############################################################################
# One cause with two effects.
#############################################################################
j.cgm.c1e2 = joint.skeleton (c ("c", "ea", "eb"))

joint.cgm.c1e2.full.ps = function (j, c, ma, mb, ba, bb) {
  e.given.c (j$ea, ma, ba, j$c) * e.given.c (j$eb, mb, bb, j$c) * var.p (j$c, c)
} 

joint.cgm.c1e2.full = function (c, ma, mb, ba, bb) {
  j.cgm.c1e2$p = joint.cgm.c1e2.full.ps (j.cgm.c1e2, c, ma, mb, ba, bb)
  j.cgm.c1e2
} 

joint.cgm.c1e2 = function (c, m, b) joint.cgm.c1e2.full (c, m, m, b, b)

q.cgm.c1e2 = joint.query.skeleton (j.cgm.c1e2, max.conditioners=2)
query.cgm.c1e2.joint = function (j) joint.query.driver (j, q.cgm.c1e2)
query.cgm.c1e2       = function (c, m, b)           query.cgm.c1e2.joint (joint.cgm.c1e2      (c, m, b))
query.cgm.c1e2.full  = function (c, ma, mb, ba, bb) query.cgm.c1e2.joint (joint.cgm.c1e2.full (c, ma, mb, ba, bb))

# A version with alternative variable names (cause = 'z', effects = 'x' and 'y').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.ic2e1).
joint.cgm.c1e2.xyz = function (c, m, b) { 
  j = joint.cgm.c1e2 (c, m, b)
  names (j) = c ('z','x','y','p')
  canonicalized.joint (j)
}

#############################################################################
# Same as c1e2 but with alternative variable names (cause = 'x', effects = 'ya' and 'yb').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.ic2e1).
#############################################################################
j.cgm.c1e2.xy = joint.skeleton (c ("x", "ya", "yb"))

# A version with alternative variable names (cause = 'x', effects = 'ya' and 'yb').
joint.cgm.c1e2.xy = function (c, m, b) { 
  j = joint.cgm.c1e2 (c, m, b)
  names (j) = c (names (j.cgm.c1e2.xy), 'p')
  j
}

q.cgm.c1e2.xy = joint.query.skeleton (j.cgm.c1e2.xy, max.conditioners=2)
query.cgm.c1e2.xy.joint = function (j) joint.query.driver (j, q.cgm.c1e2.xy)
query.cgm.c1e2.xy = function (c, m, b) query.cgm.c1e2.xy.joint (joint.cgm.c1e2.xy (c, m, b))

#############################################################################
# Generic structures and functions for networks involving two causes (ca and cb) and one effect.
# Used by ic2e1 and cc2e1 network routines. 
#############################################################################
j.cgm.c2e1 = joint.skeleton (c("ca", "cb", "e"))
q.cgm.c2e1 = joint.query.skeleton (j.cgm.c2e1, max.conditioners=2)
query.cgm.c2e1.joint = function (j) joint.query.driver (j, q.cgm.c2e1)

#############################################################################
# An effect (e) with two independent causes (ca and cb). Both noisy-or and linear integration functions are defined.
#############################################################################
joint.cgm.ic2e1.ps.generic = function (j, ca, cb, ma, mb, b, integrate.fun) {
  do.call (integrate.fun, list (j$e, ma, mb, b, j$ca, j$cb)) * var.p (j$ca, ca) * var.p (j$cb, cb)
} 

joint.cgm.ic2e1.ps             = function (j, c, m, b)           joint.cgm.ic2e1.ps.generic (j, c,  c,  m,  m,  b, e.given.ic2) 
joint.cgm.ic2e1.full.ps        = function (j, ca, cb, ma, mb, b) joint.cgm.ic2e1.ps.generic (j, ca, cb, ma, mb, b, e.given.ic2) 
joint.cgm.ic2e1.linear.ps      = function (j, c, m, b)           joint.cgm.ic2e1.ps.generic (j, c,  c,  m,  m,  b, e.given.ic2.linear)
joint.cgm.ic2e1.linear.full.ps = function (j, ca, cb, ma, mb, b) joint.cgm.ic2e1.ps.generic (j, ca, cb, ma, mb, b, e.given.ic2.linear)
joint.cgm.ic2e1.dual.ps        = function (j, c, m, b)           joint.cgm.ic2e1.ps.generic (j, c,  c,  m,  m,  b, e.given.ic2.dual)
joint.cgm.ic2e1.dual.full.ps   = function (j, ca, cb, ma, mb, b) joint.cgm.ic2e1.ps.generic (j, ca, cb, ma, mb, b, e.given.ic2.dual)

joint.cgm.ic2e1.generic = function (ca, cb, ma, mb, b, p.fun) {
  j.cgm.c2e1$p = do.call (p.fun, list (j.cgm.c2e1, ca, cb, ma, mb, b))
  j.cgm.c2e1
} 

joint.cgm.ic2e1.full         = function (ca, cb, ma, mb, b) joint.cgm.ic2e1.generic (ca, cb, ma, mb, b, joint.cgm.ic2e1.full.ps) 
joint.cgm.ic2e1.linear.full  = function (ca, cb, wa, wb, b) joint.cgm.ic2e1.generic (ca, cb, wa, wb, b, joint.cgm.ic2e1.linear.full.ps) 
joint.cgm.ic2e1.dual.full    = function (ca, cb, ma, mb, b) joint.cgm.ic2e1.generic (ca, cb, ma, mb, b, joint.cgm.ic2e1.dual.full.ps) 
joint.cgm.ic2e1              = function (c, m, b)           joint.cgm.ic2e1.full        (c,  c,  m,  m,  b) 
joint.cgm.ic2e1.linear       = function (c, w, b)           joint.cgm.ic2e1.linear.full (c,  c,  w,  w,  b) 
joint.cgm.ic2e1.dual         = function (c, m, b)           joint.cgm.ic2e1.dual.full   (c,  c,  m,  m,  b) 

query.cgm.ic2e1             = function (c, m, b)           { query.cgm.c2e1.joint (joint.cgm.ic2e1             (c, m, b)) }   
query.cgm.ic2e1.full        = function (ca, cb, ma, mb, b) { query.cgm.c2e1.joint (joint.cgm.ic2e1.full        (ca, cb, ma, mb, b)) }   
query.cgm.ic2e1.linear      = function (c, w, b)           { query.cgm.c2e1.joint (joint.cgm.ic2e1.linear      (c, w, b)) }   
query.cgm.ic2e1.linear.full = function (ca, cb, wa, wb, b) { query.cgm.c2e1.joint (joint.cgm.ic2e1.linear.full (ca, cb, wa, wb, b)) }   

# A version with alternative variable names (effect = 'z', causes = 'x' and 'y').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.c1e2).
joint.cgm.ic2e1.xyz = function (c, m, b) { 
  j = joint.cgm.ic2e1 (c, m, b)
  names (j) = c ('x','y','z','p')
  j
}

#############################################################################
# Same as ic2e1 but with alternative variable names (effect = 'x', causes = 'ya' and 'yb').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.ic2e1).
#############################################################################
j.cgm.c2e1.xy = joint.skeleton (c ('ya','yb','x'))

joint.cgm.ic2e1.xy = function (c, m, b) { 
  j = joint.cgm.ic2e1 (c, m, b)
  names (j) = c (names (j.cgm.c2e1.xy), 'p')
  j
}

q.cgm.ic2e1.xy = joint.query.skeleton (j.cgm.c2e1.xy, max.conditioners=2)
query.cgm.ic2e1.xy.joint = function (j) joint.query.driver (j, q.cgm.ic2e1.xy)
query.cgm.ic2e1.xy = function (c, m, b) query.cgm.ic2e1.xy.joint (joint.cgm.ic2e1.xy (c, m, b))

#############################################################################
# An effect (e) with two conjunctive causes (ca and cb). Both noisy-or and 
# linear intergration functions are defined.
#############################################################################
joint.cgm.cc2e1.generic = function (ca, cb, m, b, integrate.fun) {
  j.cgm.c2e1$p = do.call (integrate.fun, list (j.cgm.c2e1$e, m, b, j.cgm.c2e1$ca, j.cgm.c2e1$cb)) * var.p (j.cgm.c2e1$ca, ca) * var.p (j.cgm.c2e1$cb, cb)
  j.cgm.c2e1
} 

joint.cgm.cc2e1.full        = function (ca, cb, m, b) joint.cgm.cc2e1.generic (ca, cb, m, b, e.given.cc2)
joint.cgm.cc2e1.linear.full = function (ca, cb, w, b) joint.cgm.cc2e1.generic (ca, cb, w, b, e.given.cc2.linear)
joint.cgm.cc2e1             = function (c, m, b)      joint.cgm.cc2e1.full        (c, c, m, b) 
joint.cgm.cc2e1.linear      = function (c, w, b)      joint.cgm.cc2e1.linear.full (c, c, w, b)  

query.cgm.cc2e1.full        = function (ca, cb, m, b) query.cgm.c2e1.joint (joint.cgm.cc2e1.full        (ca, cb, m, b))      
query.cgm.cc2e1.linear.full = function (ca, cb, w, b) query.cgm.c2e1.joint (joint.cgm.cc2e1.linear.full (ca, cb, w, b))
query.cgm.cc2e1             = function (c, m, b)      query.cgm.c2e1.joint (joint.cgm.cc2e1        (c, m, b))
query.cgm.cc2e1.linear      = function (c, w, b)      query.cgm.c2e1.joint (joint.cgm.cc2e1.linear (c, w, b))  

#############################################################################
# Same as cc2e1 but with alternative variable names (cause = 'x', effects = 'ya' and 'yb').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.ic2e1).
#############################################################################
# A version with alternative variable names (cause = 'x', effects = 'ya' and 'yb').
joint.cgm.cc2e1.xy = function (c, m, b) { 
  j = joint.cgm.cc2e1 (c, m, b)
  names (j) = c (names (j.cgm.c2e1.xy), 'p')
  j
}

q.cgm.cc2e1.xy = joint.query.skeleton (j.cgm.c2e1.xy, max.conditioners=2)
query.cgm.cc2e1.xy.joint = function (j) joint.query.driver (j, q.cgm.cc2e1.xy)
query.cgm.cc2e1.xy = function (c, m, b) query.cgm.cc2e1.xy.joint (joint.cgm.cc2e1.xy (c, m, b))

#############################################################################
# An effect (e) with two complex causes (ca and cb) that have both an independent and conjunctive effect.
#############################################################################
joint.cgm.xc2e1.generic.full = function (ca, cb, ma, mb, mcc, b, integrate.fun) {
  j.cgm.c2e1$p = do.call (integrate.fun, list (j.cgm.c2e1$e, ma, mb, mcc, b, j$ca, j$cb)) * var.p (j.cgm.c2e1$ca, ca) * var.p (j.cgm.c2e1$cb, cb)
  j.cgm.c2e1
} 

joint.cgm.xc2e1.full = function (ca, cb, ma, mb, mcc, b) joint.cgm.xc2e1.generic.full (ca, cb, ma, mb, mcc, b, e.given.xc2)
joint.cgm.xc2e1      = function (c, mi, mcc, b)          joint.cgm.xc2e1.full         (c,  c,  mi, mi, mcc, b)

query.cgm.xc2e1.full = function (ca, cb, ma, mb, mcc, b) query.cgm.c2e1.joint (joint.cgm.xc2e1.full (ca, cb, ma, mb, mcc, b))      
query.cgm.xc2e1      = function (c, mi, mcc, b)          query.cgm.c2e1.joint (joint.cgm.xc2e1      (c, mi, mcc, b))     

#############################################################################
# Three element chain: root (r) -> intermediate (i) -> terminal (t). 
############################################################################# 
j.cgm.ch3 = joint.skeleton (c("r", "i", "t"))

joint.cgm.ch3.full.ps = function (j, c, m.r.i, b.i, m.i.t, b.t) {
  e.given.c (j$t, m.i.t, b.t, j$i) * e.given.c (j$i, m.r.i, b.i, j$r) * var.p (j$r, c)
} 

joint.cgm.ch3.full = function (c, m.r.i, b.i, m.i.t, b.t) {
  j.cgm.ch3$p = joint.cgm.ch3.full.ps (j.cgm.ch3, c, m.r.i, b.i, m.i.t, b.t)
  j.cgm.ch3
} 

joint.cgm.ch3 = function (c, m, b) joint.cgm.ch3.full (c, m, b, m, b)

q.cgm.ch3 = joint.query.skeleton (j.cgm.ch3, max.conditioners=2)
query.cgm.ch3.joint = function (j) joint.query.driver (j, q.cgm.ch3)
query.cgm.ch3       = function (c, m, b)                   query.cgm.ch3.joint (joint.cgm.ch3 (c, m, b))
query.cgm.ch3.full  = function (c, m.r.i, b.i, m.i.t, b.t) query.cgm.ch3.joint (joint.cgm.ch3.full (c, m.r.i, b.i, m.i.t, b.t))

# A version with alternative variable names (root = 'x', intermediate = 'z', terminal = 'y').
# Alignable with analogous versions of other 3 variable joints (e.g., joint.cgm.ic2e1).
joint.cgm.ch3.xyz = function (c, m, b) { 
  j = joint.cgm.ch3 (c, m, b)
  names (j) = c ('x','z','y','p')
  canonicalized.joint (j)
}

#############################################################################
# A three-element chain in which the root and terminal effect are directly related
############################################################################# 
joint.cgm.direct.ch.ps = function (j, c, m.r.i, b.i, m.r.t, m.i.t, b.t) {
  e.given.ic2 (j$t, m.r.t, m.i.t, b.t, j$r, j$i) * e.given.c (j$i, m.r.i, b.i, j$r) * var.p (j$r, c)
}

joint.cgm.direct.ch.full = function (c, m.r.i, b.i, m.r.t, m.i.t, b.t) {
  j.cgm.ch3$p = joint.cgm.direct.ch.ps (j.cgm.ch3, c, m.r.i, b.i, m.r.t, m.i.t, b.t)
  j.cgm.ch3
}

joint.cgm.direct.ch = function (c, m, b) joint.cgm.direct.ch.full (c, m, b, m, m, b)

#############################################################################
# GRAPHS WITH FOUR VARIABLES.
#############################################################################

#############################################################################
# One cause with three effects.
#############################################################################
j.cgm.c1e3 = joint.skeleton (c("c", "ea", "eb", "ec"))

joint.cgm.c1e3.full.ps = function (j, c, ma, mb, mc, ba, bb, bc) {
  e.given.c (j$ea, ma, ba, j$c) * e.given.c (j$eb, mb, bb, j$c) * e.given.c (j$ec, mc, bc, j$c) * var.p (j$c, c)
} 

joint.cgm.c1e3.full = function (c, ma, mb, mc, ba, bb, bc) {
  j.cgm.c1e3$p = joint.cgm.c1e3.full.ps (j.cgm.c1e3, c, ma, mb, mc, ba, bb, bc)
  j.cgm.c1e3
} 

joint.cgm.c1e3 = function (c, m, b) joint.cgm.c1e3.full (c, m, m, m, b, b, b)

q.cgm.c1e3 = joint.query.skeleton (j.cgm.c1e3, max.conditioners=2)
query.cgm.c1e3.joint = function (j) joint.query.driver (j, q.cgm.c1e3)
query.cgm.c1e3       = function (c, m, b)                   query.cgm.c1e3.joint (joint.cgm.c1e3 (c, m, b))
query.cgm.c1e3.full  = function (c, ma, mb, mc, ba, bb, bc) query.cgm.c1e3.joint (joint.cgm.c1e3.full (c, ma, mb, mc, ba, bb, bc))

#############################################################################
# An effect (e) with three independent causes (ca, cb, and cc).
#############################################################################
j.cgm.ic3e1 = joint.skeleton (c("ca", "cb", "cc", "e"))

joint.cgm.ic3e1.full.ps = function (j, ca, cb, cc, ma, mb, mc, b) {
  e.given.ic3 (j$e, ma, mb, mc, b, j$ca, j$cb, j$cc) * var.p (j$ca, ca) * var.p (j$cb, cb) * var.p (j$cc, cc)
} 

joint.cgm.ic3e1.full = function (ca, cb, cc, ma, mb, mc, b) {
  j.cgm.ic3e1$p = joint.cgm.ic3e1.full.ps (j.cgm.ic3e1, ca, cb, cc, ma, mb, mc, b)
  j.cgm.ic3e1
} 

joint.cgm.ic3e1 = function (c, m, b) joint.cgm.ic3e1.full (c, c, c, m, m, m, b)

q.cgm.c1e3 = joint.query.skeleton (j.cgm.ic3e1, max.conditioners=2)
query.cgm.ic3e1.joint = function (j) joint.query.driver (j, q.cgm.c1e3)
query.cgm.ic3e1       = function (c, m, b)                   query.cgm.ic3e1.joint (joint.cgm.ic3e1 (c, m, b))
query.cgm.ic3e1.full  = function (ca, cb, cc, ma, mb, mc, b) query.cgm.ic3e1.joint (joint.cgm.ic3e1.full (ca, cb, cc, ma, mb, mc, b))

#############################################################################
# An effect (e) with two independent causes with a shared "enabler."
#############################################################################
j.cgm.ic2en1 = joint.skeleton (c("ca", "cb", "en", "e"))

joint.cgm.ic2en1.full = function (ca, cb, ma, mb, b) {
  j = j.cgm.ic2en1
  j$p = e.given.ic2en1 (j$e, ma, mb, b, j$ca, j$cb, j$en) * var.p (j$ca, ca) * var.p (j$cb, cb) * var.p (j$en, en)
  j
} 

joint.ic2en1 = function (c, en, m, b) joint.ic2en1.full (c, c, en, m, m, b)

#############################################################################
# Four element chain: W -> X -> Y -> Z. 
############################################################################# 
j.cgm.ch4 = joint.skeleton (c ("w", "x", "y", "z"))

joint.cgm.ch4.ps = function (j, c, m, b) {
  e.given.c (j$z, m, b, j$y) * e.given.c (j$y, m, b, j$x) * e.given.c (j$x, m, b, j$w) * var.p (j$w, c)
} 

joint.cgm.ch4 = function (c, m, b) {
  j.cgm.ch4$p = joint.cgm.ch4.ps (j.cgm.ch4, c, m, b)
  j.cgm.ch4
} 

q.cgm.ch4 = joint.query.skeleton (j.cgm.ch4, max.conditioners=2)
query.cgm.ch4.joint = function (j) joint.query.driver (j, q.cgm.ch4)
query.cgm.ch4       = function (c, m, b) query.cgm.ch4.joint (joint.cgm.ch4 (c, m, b))

#############################################################################
# A diamond: A root cause (r), with two intermediate effects (i), which have a common effect (e)
#############################################################################
j.cgm.diamond = joint.skeleton (c ("cc", "ia", "ib", "e"))

joint.cgm.diamond.full.ps = function (j, c, m.c.ia, b.ia, m.c.ib, b.ib, m.ia.e, m.ib.e, b.e) {
  e.given.ic2 (j$e, m.ia.e, m.ib.e, b.e, j$ia, j$ib) * e.given.c (j$ia, m.c.ia, b.ia, j$cc) * e.given.c (j$ib, m.c.ib, b.ib, j$cc) * var.p (j$cc, c)
}  

joint.cgm.diamond.full = function (c, m.c.ia, b.ia, m.c.ib, b.ib, m.ia.e, m.ib.e, b.e) {
  j.cgm.diamond$p = joint.cgm.diamond.full.ps (j.cgm.diamond, c, m.c.ia, b.ia, m.c.ib, b.ib, m.ia.e, m.ib.e, b.e)
  j.cgm.diamond
}  

joint.cgm.diamond = function (c, m, b) joint.cgm.diamond.full (c, m, b, m, b, m, m, b)

q.cgm.diamond = joint.query.skeleton (j.cgm.diamond, max.conditioners=1)
query.cgm.diamond.joint = function (j) joint.query.driver (j, q.cgm.diamond)
query.cgm.diamond       = function (c, m, b) query.cgm.diamond.joint (joint.cgm.diamond (c, m, b))

#############################################################################
# A common cause with an extra cause (ac) of one of the effects (e1).
#############################################################################
j.cgm.cc2.ac = joint.skeleton (c ("c", "a", "ea", "eb"))

joint.cgm.cc2.ac.ps = function (j, c, m, b, c.a, m.a) 
  e.given.ic2 (j$ea, m, m.a, b, j$c, j$a) * e.given.c (j$eb, m, b, j$c) * var.p (j$c, c) * var.p (j$a, c.a)

joint.cgm.cc2.ac = function (c, m, b, c.a, m.a) {
  j.cgm.cc2.ac$p = joint.cgm.cc2.ac.ps (j.cgm.cc2.ac, c, m.c.ia, b.ia, m.c.ib, b.ib, m.ia.e, m.ib.e, b.e)
  j.cgm.cc2.ac
}

#############################################################################
# GRAPHS WITH FIVE VARIABLES.
#############################################################################

#############################################################################
# One cause with four effects.
#############################################################################
j.cgm.c1e4 = joint.skeleton (c("c", "ea", "eb", "ec", "ed"))

joint.cgm.c1e4.full.ps = function (j, c, ma, mb, mc, md, ba, bb, bc, bd) {
  e.given.c (j$ea, ma, ba, j$c) * e.given.c (j$eb, mb, bb, j$c) * e.given.c (j$ec, mc, bc, j$c) * e.given.c (j$ed, md, bd, j$c) * var.p (j$c, c)
} 

joint.cgm.c1e4.full = function (c, ma, mb, mc, md, ba, bb, bc, bd) {
  j.cgm.c1e4$p = joint.cgm.c1e4.full.ps (j.cgm.c1e4, c, ma, mb, mc, md, ba, bb, bc, bd)
  j.cgm.c1e4
} 

joint.cgm.c1e4 = function (c, m, b) joint.cgm.c1e4.full (c, m, m, m, m, b, b, b, b)  

q.cgm.c1e4 = joint.query.skeleton (j.cgm.c1e4, max.conditioners=1)
query.cgm.c1e4.joint = function (j) joint.query.driver (j, q.cgm.c1e4)
query.cgm.c1e4       = function (c, m, b) query.cgm.c1e4.joint (joint.cgm.c1e4 (c, m, b))

#############################################################################
# A 2-1-2 network
#############################################################################
j.cgm.212 = joint.skeleton (c("xa", "xb", "y", "za", "zb"))

joint.cgm.212.ps = function (j, c, m, b) {
  e.given.c (j$za, m, b, j$y) * e.given.c (j$zb, m, b, j$y) * e.given.ic2 (j$y, m, m, b, j$xa, j$xb) * var.p (j$xa, c) * var.p (j$xb, c)
} 

joint.cgm.212 = function (c, m, b) {
  j.cgm.212$p = joint.cgm.212.ps (j.cgm.212, c, m, b)
  j.cgm.212
} 

q.cgm.212 = joint.query.skeleton (j.cgm.212, max.conditioners=1)
query.cgm.212.joint = function (j) joint.query.driver (j, q.cgm.212)
query.cgm.212       = function (c, m, b) query.cgm.212.joint (joint.cgm.212 (c, m, b))

#############################################################################
# A 3-1-1 network
#############################################################################
j.cgm.311 = joint.skeleton (c("xa", "xb", "xc", "y", "z"))

joint.cgm.311.ps = function (j, c, m, b) {
  e.given.c (j$z, m, b, j$y) * e.given.ic3 (j$y, m, m, m, b, j$xa, j$xb, j$xc) *var.p (j$xa, c) * var.p (j$xb, c) * var.p (j$xc, c)
} 

joint.cgm.311 = function (c, m, b) {
  j.cgm.311$p = joint.cgm.311.ps (j.cgm.311, c, m, b)
  j.cgm.311
} 

q.cgm.311 = joint.query.skeleton (j.cgm.311, max.conditioners=1)
query.cgm.311.joint = function (j) joint.query.driver (j, q.cgm.311)
query.cgm.311       = function (c, m, b) query.cgm.311.joint (joint.cgm.311 (c, m, b))

#############################################################################
# A 1-1-3 network
#############################################################################
j.cgm.113 = joint.skeleton (c("x", "y", "za", "zb", "zc"))

joint.cgm.113.ps = function (j, c, m, b) {
  e.given.c (j$za, m, b, j$y) * e.given.c (j$zb, m, b, j$y) * e.given.c (j$zc, m, b, j$y) * e.given.c (j$y, m, b, j$x) * var.p (j$x, c)
}

joint.cgm.113 = function (c, m, b) {
  j.cgm.113$p = joint.cgm.113.ps (j.cgm.113, c, m, b)
  j.cgm.113
} 

q.cgm.113 = joint.query.skeleton (j.cgm.113, max.conditioners=1)
query.cgm.113.joint = function (j) joint.query.driver (j, q.cgm.113)
query.cgm.113       = function (c, m, b) query.cgm.113.joint (joint.cgm.113 (c, m, b))

#############################################################################
# A 1-2-2 network
#############################################################################
j.cgm.122 = joint.skeleton (c("x", "ya", "yb", "za", "zb"))

joint.cgm.122.ps = function (j, c, m, b) {
  e.given.c (j$za, m, b, j$ya) * e.given.c (j$ya, m, b, j$x) * e.given.c (j$zb, m, b, j$yb) * e.given.c (j$yb, m, b, j$x) * var.p (j$x, c) 
}

joint.cgm.122 = function (c, m, b) {
  j.cgm.122$p = joint.cgm.122.ps (j.cgm.122, c, m, b)
  j.cgm.122
} 

q.cgm.122 = joint.query.skeleton (j.cgm.122, max.conditioners=1)
query.cgm.122.joint = function (j) joint.query.driver (j, q.cgm.122)
query.cgm.122       = function (c, m, b) query.cgm.122.joint (joint.cgm.122 (c, m, b))

joint.cgm.122.ps.full = function (j, c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b.ya, b.yb, b.za, b.zb) {
  e.given.c (j$za, m.ya.za, b.za, j$ya) * e.given.c (j$ya, m.x.ya, b.ya, j$x) * e.given.c (j$zb, m.yb.zb, b.zb, j$yb) * e.given.c (j$yb, m.x.yb, b.yb, j$x) * var.p (j$x, c) 
}

joint.cgm.122.full = function (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b.ya, b.yb, b.za, b.zb) {
  j.cgm.122$p = joint.cgm.122.ps.full (j.cgm.122, c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b.ya, b.yb, b.za, b.zb)
  j.cgm.122
} 

query.cgm.122.full = function (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b.ya, b.yb, b.za, b.zb) 
  query.cgm.122.joint (joint.cgm.122.full (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b.ya, b.yb, b.za, b.zb))

#joint.cgm.122.ps.full = function (j, c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b) {
#  e.given.c (j$za, m.ya.za, b, j$ya) * e.given.c (j$ya, m.x.ya, b, j$x) * e.given.c (j$zb, m.yb.zb, b, j$yb) * e.given.c (j$yb, m.x.yb, b, j$x) * var.p (j$x, c) 
#}

#joint.cgm.122.full = function (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b) {
#  j.cgm.122$p = joint.cgm.122.ps.full (j.cgm.122, c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b)
#  j.cgm.122
#} 

#query.cgm.122.full = function (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b) 
#  query.cgm.122.joint (joint.cgm.122.full (c, m.x.ya, m.x.yb, m.ya.za, m.yb.zb, b))

#############################################################################
# A 2-2-1 network
#############################################################################
j.cgm.221 = joint.skeleton (c("za", "zb", "ya", "yb", "x"))

joint.cgm.221.ps = function (j, c, m, b, bx) {
  e.given.ic2 (j$x, m, m, bx, j$ya, j$yb) * e.given.c (j$ya, m, b, j$za) * e.given.c (j$yb, m, b, j$zb) * var.p (j$za, c) * var.p (j$zb, c) 
}

joint.cgm.221 = function (c, m, b) {
  j.cgm.221$p = joint.cgm.221.ps (j.cgm.221, c, m, b, b)
  j.cgm.221
} 

# Same as joint.cgm.221 but allows X to have its own b parameter.
joint.cgm.221.bx = function (c, m, b, bx) {
  j.cgm.221$p = joint.cgm.221.ps (j.cgm.221, c, m, b, bx)
  j.cgm.221
} 

q.cgm.221 = joint.query.skeleton (j.cgm.221, max.conditioners=1)
query.cgm.221.joint = function (j) joint.query.driver (j, q.cgm.221)
query.cgm.221       = function (c, m, b)     { query.cgm.221.joint (joint.cgm.221    (c, m, b)) }
query.cgm.221.bx    = function (c, m, b, bx) { query.cgm.221.joint (joint.cgm.221.bx (c, m, b, bx)) }

joint.cgm.221.ps.full = function (j, c.za, c.zb, m.za.ya, m.zb.yb, m.ya.x, m.yb.x, b.ya, b.yb, b.x) {
  e.given.ic2 (j$x, m.ya.x, m.yb.x, b.x, j$ya, j$yb) * e.given.c (j$ya, m.za.ya, b.ya, j$za) * e.given.c (j$yb, m.zb.yb, b.yb, j$zb) * var.p (j$za, c.za) * var.p (j$zb, c.zb) 
}

joint.cgm.221.full = function (c.za, c.zb, m.za.ya, m.zb.yb, m.ya.x, m.yb.x, b.ya, b.yb, b.x) {
  j.cgm.221$p = joint.cgm.221.ps.full (j.cgm.221, c.za, c.zb, m.za.ya, m.zb.yb, m.ya.x, m.yb.x, b.ya, b.yb, b.x)
  j.cgm.221
} 

query.cgm.221.full = function (c.za, c.zb, m.za.ya, m.zb.yb, m.ya.x, m.yb.x, b.ya, b.yb, b.x) 
  query.cgm.221.joint (joint.cgm.221.full (c.za, c.zb, m.za.ya, m.zb.yb, m.ya.x, m.yb.x, b.ya, b.yb, b.x))

#############################################################################
# Koller & Friedman's (2009) student network
#############################################################################
j.cgm.student = joint.skeleton (c("d", "i", "g", "s", "l"))

joint.cgm.student.ps = function (j, c, m, b)
  e.given.c (j$l, m, b, j$g) * e.given.ic2 (j$g, m, m, b, j$d, j$i) * e.given.c (j$s, m, b, j$i) * var.p (j$d, c) * var.p (j$i, c) 

joint.cgm.student = function (c, m, b) {
  j.cgm.student$p = joint.cgm.student.ps (j.cgm.student, c, m, b)
  j.cgm.student
} 

q.cgm.student = joint.query.skeleton (j.cgm.student, max.conditioners=1)
query.cgm.student.joint = function (j) joint.query.driver (j, q.cgm.student)
query.cgm.student       = function (c, m, b) query.cgm.student.joint (joint.cgm.student (c, m, b))

# Student network mutilated so that i=1 and g=1.
joint.cgm.student.mutilated.ps = function (j, c, m, b)
  e.given.c (j$l, m, b, j$g) * var.p (j$g, 1) * e.given.c (j$s, m, b, j$i) * var.p (j$d, c) * var.p (j$i, 1) 

joint.cgm.student.mutilated = function (c, m, b) {
  j.cgm.student$p = joint.cgm.student.mutilated.ps (j.cgm.student, c, m, b)
  j.cgm.student
} 

#############################################################################
# Ic2 and cc2 networks that share a cause.
#############################################################################
j.cgm.ic2.cc2.shared.cause = joint.skeleton (c("ic", "sc", "cc", "ie", "ce"))

joint.cgm.ic2.cc2.shared.cause.ps = function (j, c, mi, mc, b) {
  e.given.ic2 (j$ie, mi, mi, b, j$ic, j$sc) * e.given.cc2 (j$ce, mc, b, j$cc, j$sc) * var.p (j$ic, c) * var.p (j$sc, c) * var.p (j$cc, c)
}

joint.cgm.ic2.cc2.shared.cause = function (c, mi, mc, b) {
  j.cgm.ic2.cc2.shared.cause$p = joint.cgm.221.ps (j.cgm.ic2.cc2.shared.cause, c, m, b)
  j.cgm.ic2.cc2.shared.cause
} 

#############################################################################
# Five element chain: V -> W -> X -> Y -> Z. 
############################################################################# 
j.cgm.ch5 = joint.skeleton (c ("v", "w", "x", "y", "z"))

joint.cgm.ch5.ps = function (j, c, m, b) {
  e.given.c (j$z, m, b, j$y) * e.given.c (j$y, m, b, j$x) * e.given.c (j$x, m, b, j$w) * e.given.c (j$w, m, b, j$v) * var.p (j$v, c)
}

joint.cgm.ch5 = function (c, m, b) {
  j.cgm.ch5$p = joint.cgm.ch5.ps (j.cgm.ch5, c, m, b)
  j.cgm.ch5
} 

q.cgm.ch5 = joint.query.skeleton (j.cgm.ch5, max.conditioners=1)
query.cgm.ch5.joint = function (j) joint.query.driver (j, q.cgm.ch5)
query.cgm.ch5       = function (c, m, b) query.cgm.ch5.joint (joint.cgm.ch5 (c, m, b))

#############################################################################
# Two common cause structures that share an effect (eb)
############################################################################# 
j.cgm.c2e3 = joint.skeleton (c ("ca", "cb", "ea", "eb", "ec"))

joint.cgm.c2e3.full.ps = function (j, ca, cb, maa, ba, mab, mbb, bb, mbc, bc) {
  e.given.c (j$ea, maa, ba, j$ca) * e.given.ic2 (j$eb, mab, mbb, bb, j$ca, j$cb) * e.given.c (j$ec, mbc, bc, j$cb) * var.p (j$ca, ca) * var.p (j$cb, cb)
}

joint.cgm.c2e3.full = function (ca, cb, maa, ba, mab, mbb, bb, mbc, bc) {
  j.cgm.c2e3$p = joint.cgm.c2e3.full.ps (j.cgm.c2e3, c, m, b)
  j.cgm.c2e3
} 

joint.cgm.c2e3 = function (c, m, b) joint.cgm.c2e3.full (c, c, m, b, m, m, b, m, b) 

#############################################################################
# A common cause with alternative causes (ac1 and ac2) of each of the effects.
#############################################################################
j.cgm.cc2.ac2 = joint.skeleton (c ("c", "aa", "ab", "ea", "eb"))

joint.cgm.cc2.ac2.ps = function (j, c, m, b, c.a, m.a) {
  e.given.ic2 (j$ea, m, m.a, b, j$c, j$aa) * e.given.ic2 (j$eb, m, m.a, b, j$c, j$ab) * var.p (j$c, c) * var.p (j$aa, c.a) * var.p (j$ab, c.a)
}

joint.cgm.cc2.ac2 = function (c, m, b, c.a, m.a) {
  j.cgm.cc2.ac2$p = joint.cgm.cc2.ac2.ps (j.cgm.cc2.ac2, c, m, b)
  j.cgm.cc2.ac2
} 

#############################################################################
# GRAPHS WITH SIX VARIABLES.
#############################################################################

#############################################################################
# A double c2e1 network. Generic routines.
#############################################################################
j.cgm.c2e1.1 = joint.skeleton (c('wa','wb','x'))
j.cgm.c2e1.2 = joint.skeleton (c('ya','yb','z'))
j.cgm.c2e1.c2e1 = joint.skeleton (c (names (j.cgm.c2e1.1), names (j.cgm.c2e1.2)))

# Preallocate query structures for the subnetwok queries.
# Only within-subnetwork joint and conditonal probabilities are generated.
subjoint.skeleton = function (j, j.qu.lab1, j.qu.lab2) {
  qu = joint.query.skeleton (j, max.conditioners=2)
  qu$qu$q = ifelse (qu$qu$qtype == 'j', paste (j.qu.lab1, qu$qu$q, j.qu.lab2, sep=''), levels (qu$qu$q)[qu$qu$q])
  rownames (qu$qu) = interaction (qu$qu$qtype, qu$qu$q)
  qu
}
q.cgm.c2e1.1 = subjoint.skeleton (j.cgm.c2e1.1, '', 'xxx')
q.cgm.c2e1.2 = subjoint.skeleton (j.cgm.c2e1.2, 'xxx', '')

query.cgm.c2e1.c2e1.joint = function (j) {
  # Compute the queries by marginalizing over each subnetwork.
  j1 = joint.marginalized (j, names (j.cgm.c2e1.1))[rownames (j.cgm.c2e1.1),]
  j2 = joint.marginalized (j, names (j.cgm.c2e1.2))[rownames (j.cgm.c2e1.2),]
  rbind (joint.query.driver (j1, q.cgm.c2e1.1), joint.query.driver (j2, q.cgm.c2e1.2))
}

# Computes the same queries as query.cgm.c2e1.c2e1.joint but from the joints associated with the
# two subnetworks. Potentially faster because it avoids having to marginalize over the large joint.
query.cgm.c2e1.c2e1.subjoints = function (j1, j2) {
  names (j1) = c (names (j.cgm.c2e1.1), 'p')
  q1 = joint.query.driver (j1, q.cgm.c2e1.1)
  names (j2) = c (names (j.cgm.c2e1.2), 'p')
  q2 = joint.query.driver (j2, q.cgm.c2e1.2)
  rbind (q1, q2)
}

#############################################################################
# A ic2e1 + cc2e1 network.
#############################################################################
joint.cgm.ic2e1.cc2e1.ps = function (j, c1, m1, b1, c2, m2, b2) {
  e.given.ic2 (j$x, m1, m1, b1, j$wa, j$wb) * var.p (j$wa, c1) * var.p (j$wb, c1) *
    e.given.cc2 (j$z, m2, b2, j$ya, j$yb) * var.p (j$ya, c2) * var.p (j$yb, c2)
}

joint.cgm.ic2e1.cc2e1 = function (c1, m1, b1, c2, m2, b2) {
  j.cgm.c2e1.c2e1$p = joint.cgm.ic2e1.cc2e1.ps (j.cgm.c2e1.c2e1, c1, m1, b1, c2, m2, b2)
  j.cgm.c2e1.c2e1
} 

query.cgm.ic2e1.cc2e1 = function (c1, m1, b1, c2, m2, b2) 
  query.cgm.c2e1.c2e1.subjoints (joint.cgm.ic2e1 (c1, m1, b1), joint.cgm.cc2e1 (c2, m2, b2))

#############################################################################
# A double ic2e1 network.
#############################################################################
joint.cgm.ic2e1.ic2e1.ps = function (j, c1, m1, b1, c2, m2, b2) {
  e.given.ic2 (j$x, m1, m1, b1, j$wa, j$wb) * var.p (j$wa, c1) * var.p (j$wb, c1) *
    e.given.ic2 (j$z, m2, m2, b2, j$ya, j$yb) * var.p (j$ya, c2) * var.p (j$yb, c2)
}

joint.cgm.ic2e1.ic2e1 = function (c1, m1, b1, c2, m2, b2) {
  j.cgm.c2e1.c2e1$p = joint.cgm.ic2e1.ic2e1.ps (j.cgm.c2e1.c2e1, c1, m1, b1, c2, m2, b2)
  j.cgm.c2e1.c2e1
} 

query.cgm.ic2e1.ic2e1 = function (c1, m1, b1, c2, m2, b2) 
  query.cgm.c2e1.c2e1.subjoints (joint.cgm.ic2e1 (c1, m1, b1), joint.cgm.ic2e1 (c2, m2, b2))

#############################################################################
# A double cc2e1 network.
#############################################################################
joint.cgm.cc2e1.cc2e1.ps = function (j, c1, m1, b1, c2, m2, b2) {
  e.given.cc2 (j$x, m1, b1, j$wa, j$wb) * var.p (j$wa, c1) * var.p (j$wb, c1) *
    e.given.cc2 (j$z, m2, b2, j$ya, j$yb) * var.p (j$ya, c2) * var.p (j$yb, c2)
}

joint.cgm.cc2e1.cc2e1 = function (c1, m1, b1, c2, m2, b2) {
  j.cgm.c2e1.c2e1$p = joint.cgm.cc2e1.cc2e1.ps (j.cgm.c2e1.c2e1, c1, m1, b1, c2, m2, b2)
  j.cgm.c2e1.c2e1
} 

query.cgm.cc2e1.cc2e1 = function (c1, m1, b1, c2, m2, b2) 
  query.cgm.c2e1.c2e1.subjoints (joint.cgm.cc2e1 (c1, m1, b1), joint.cgm.cc2e1 (c2, m2, b2))

#############################################################################
# Same as joint.cc2.ac where the two effects (ea and eb) have a common effect (e).
#############################################################################
j.cgm.cc2.ac2.e = joint.skeleton (c ("c", "aa", "ab", "ea", "eb", "e"))

joint.cgm.cc2.ac2.e.ps = function (j, c, m, b, c.a, m.a, m.e, b.e) {
  e.given.ic2 (j$e, m.e, m.e, b.e, j$ea, j$eb) * e.given.ic2 (j$ea, m, m.a, b, j$c, j$aa) * e.given.ic2 (j$eb, m, m.a, b, j$c, j$ab) *
    var.p (j$c, c) * var.p (j$aa, c.a) * var.p (j$ab, c.a)
}

joint.cgm.cc2.ac2.e = function (c, m, b, c.a, m.a, m.e, b.e) {
  j.cgm.cc2.ac2.e$p = joint.cgm.cc2.ac2.e.ps (j.cgm.cc2.ac2.e, c, m, b, c.a, m.a, m.e, b.e)
  j.cgm.cc2.ac2.e
} 

#############################################################################
# One cause with four effects plus an enabler.
#############################################################################
j.cgm.c1e4en = joint.skeleton (c("c", "en", "ea", "eb", "ec", "ed"))

joint.cgm.c1e4en.full.ps = function (j, c, cen, ma, mb, mc, md, men, ba, bb, bc, bd, be) {
  e.given.ic2 (j$ea, ma, men, ba, j$c, j$en) * 
    e.given.ic2 (j$eb, mb, men, bb, j$c, j$en) * 
    e.given.ic2 (j$ec, mc, men, bc, j$c, j$en) * 
    e.given.ic2 (j$ed, md, men, bd, j$c, j$en) * 
    var.p (j$c, c) * var.p (j$en, cen)
} 

joint.cgm.c1e4en.full = function (c, cen, ma, mb, mc, md, men, ba, bb, bc, bd) {
  j.cgm.c1e4en$p = joint.cgm.c1e4en.full.ps (j.cgm.c1e4en, c, cen, ma, mb, mc, md, men, ba, bb, bc, bd)
  j.cgm.c1e4en
} 

joint.cgm.c1e4en = function (c, cen, m, men, b) joint.cgm.c1e4en.full (c, cen, m, m, m, m, men, b, b, b, b)  

q.cgm.c1e4en = joint.query.skeleton (j.cgm.c1e4en, max.conditioners=1)
query.cgm.c1e4en.joint = function (j) joint.query.driver (j, q.cgm.c1e4en)
query.cgm.c1e4en       = function (c, cen, m, men, b) query.cgm.c1e4en.joint (joint.cgm.c1e4en (c, cen, m, men, b))

#############################################################################
# A 2-2-1-1 network
#############################################################################
j.cgm.2211 = joint.skeleton (c("wa", "wb", "xa", "xb", "y", "z"))

joint.cgm.2211.ps = function (j, c, m, b) {
  e.given.c (j$z, m, b, j$y) * e.given.ic2 (j$y, m, j, b, j$xa, j$xb) * 
    e.given.c (j$xa, m, b, j$wa) * var.p (j$wa, c) * e.given.c (j$xb, m, b, j$wb) * var.p (j$wb, c) 
}

joint.cgm.2211 = function (c, m, b) {
  j.cgm.2211$p = joint.cgm.2211.ps (j.cgm.2211, c, m, b)
  j.cgm.2211
} 

#############################################################################
# GRAPHS WITH SEVEN VARIABLES.
#############################################################################

#############################################################################
# One cause with five effects plus a disabler
#############################################################################
j.cgm.c1e5en = joint.skeleton (c("c", "en", "ea", "eb", "ec", "ed", "ee"))

joint.cgm.c1e5en.full.ps = function (j, c, cen, ma, mb, mc, md, me, men, ba, bb, bc, bd, be) {
  e.given.ic2 (j$ea, ma, men, ba, j$c, j$en) * 
    e.given.ic2 (j$eb, mb, men, bb, j$c, j$en) * 
    e.given.ic2 (j$ec, mc, men, bc, j$c, j$en) * 
    e.given.ic2 (j$ed, md, men, bd, j$c, j$en) * 
    e.given.ic2 (j$ee, me, men, be, j$c, j$en) * 
    var.p (j$c, c) * var.p (j$en, cen)
} 

joint.cgm.c1e5en.full = function (c, cen, ma, mb, mc, md, me, men, ba, bb, bc, bd, be) {
  j.cgm.c1e5en$p = joint.cgm.c1e5en.full.ps (j.cgm.c1e5en, c, cen, ma, mb, mc, md, me, men, ba, bb, bc, bd, be)
  j.cgm.c1e5en
} 

joint.cgm.c1e5en = function (c, cen, m, men, b) joint.cgm.c1e5en.full (c, cen, m, m, m, m, m, men, b, b, b, b, b)  

q.cgm.c1e5en = joint.query.skeleton (j.cgm.c1e5en, max.conditioners=1)
query.cgm.c1e5en.joint = function (j) joint.query.driver (j, q.cgm.c1e5en)
query.cgm.c1e5en       = function (c, cen, m, men, b) query.cgm.c1e5en.joint (joint.cgm.c1e5en (c, cen, m, men, b))

#############################################################################
# A 1-2-2-2 network
#############################################################################
j.cgm.1222 = joint.skeleton (c("w", "xa", "xb", "ya", "yb", "za", "zb"))

joint.cgm.1222.ps = function (j, c, m, b) {
  e.given.c (j$za, m, b, j$ya) * e.given.c (j$ya, m, b, j$xa) * e.given.c (j$xa, m, b, j$w) * 
    e.given.c (j$zb, m, b, j$yb) * e.given.c (j$yb, m, b, j$xb) * e.given.c (j$xb, m, b, j$w) * var.p (j$w, c) 
}

joint.cgm.1222 = function (c, m, b) {
  j.cgm.1222$p = joint.cgm.1222.ps (j.cgm.1222, c, m, b)
  j.cgm.1222
} 

#############################################################################
# OBSOLETE
#############################################################################

#library(gRain)

#############################################################################
# Full conditional probability tables (cpts) that combine all the conditional   
# probabilities into a single vector. (Formatted to be usable by the pGrain package.)
#############################################################################

binary.levels = c("0","1")

# Probability table for a single binary variable (usually used for a root cause).
pt = function (p) { c (1 - p, p) }

# Returns full cpt for an effect conditioned on a single cause.
cpt.c1e1 = function (m, b) { 
  cpt = c (); 
  for (c in 0:1) { 
    for (e in 0:1) { cpt = c (cpt, e.given.c (e, m, b, c)) }}
  cpt
}

# Returns full cpt for an effect conditioned on two independent causes.
cpt.ic2e1 = function (ma, mb, b) { 
  cpt = c ()
  for (icb in 0:1) {
    for (ica in 0:1) {
      for (e in 0:1) { cpt = c (cpt, e.given.ic2 (e, ma, mb, b, ica, icb)) }}}
  cpt
}

# Returns full cpt for an effect conditioned on three independent causes.
cpt.ic3e1 = function (ma, mb, mc, b) { 
  cpt = c ()
  for (icc in 0:1) {
    for (icb in 0:1) {
      for (ica in 0:1) {
        for (e in 0:1) { cpt = c (cpt, e.given.ic3 (e, ma, mb, mc, b, ica, icb, icc)) }}}}
  cpt
}

# Returns full cpt for an effect conditioned on three independent causes.
cpt.ic4e1 = function (ma, mb, mc, md, b) { 
  cpt = c ()
  for (icd in 0:1) {
    for (icc in 0:1) {
      for (icb in 0:1) {
        for (ica in 0:1) {
          for (e in 0:1) { cpt = c (cpt, e.given.ic4 (e, ma, mb, mc, md, b, ica, icb, icc, icd)) }}}}}
  cpt
}

# Returns full cpt for an effect conditioned on two conjunctive causes.
cpt.cc2e1 = function (m, b) { 
  cpt = c ()
  for (ccb in 0:1) {
    for (cca in 0:1) { cpt = c (cpt, e0.given.cc2 (m, b, cca, ccb), e1.given.cc2 (m, b, cca, ccb)) }}
  cpt
}

# Returns full cpt for an effect conditioned on three causes, where the last two (cca and ccb) are conjunctive.
cpt.ic1cc2e1 = function (mi, mc, b) { 
  cpt = c ()
  for (ccb in 0:1) {
    for (cca in 0:1) {
      for (ic in 0:1) { cpt = c (cpt, e0.given.ic1cc2 (mi, mc, b, ic, cca, ccb), e1.given.ic1cc2 (mi, mc, b, ic, cca, ccb)) }}}
  cpt
}

# Returns full cpt for an effect conditioned on two independent causes plus a shared "enabler."
cpt.ic2en1 = function (ma, mb, b) { 
  cpt = c ()
  for (ica in 0:1) {
    for (icb in 0:1) {
      for (en in 0:1) { cpt = c (cpt, e.given.ic2en1 (ma, mb, b, ica, icb, en)) }}}
  cpt
}

# Returns full cpt for an effect conditioned on a pair of conjunctive causes.
cpt.double.cc2 = function (ma, mb, b) { 
  cpt = c ()
  for (cb2 in 0:1) {
    for (cb1 in 0:1) {
      for (ca2 in 0:1) {
        for (ca1 in 0:1) {
          for (e in 0:1) { cpt = c (cpt, e.given.double.cc (e, ma, mb, b, ca1, ca2, cb1, cb2)) }}}}}
  cpt
}

#############################################################################
# Converts a joint returned by pGrain (a multidimensional array)
# into one represented as a data frame (the canoncial form used here).
############################################################################

to.joint.as.data.frame = function (pGrain.Joint, vars) {
  
  recurse = function (j, vals, level, i, inc) {
    if (level == 0) {
      # All vars have been assigned a 0 or 1. Pull out probability from pGrain.Joint and add to joint j.
      rbind (j, c (vals, as.vector (pGrain.Joint) [i]))
    }  
    else {
      # Instantiate current variable as either a 0 or 1 and then recurse to the next one.
      level = level - 1; new.inc = 2**(level - 1)
      j = recurse (j, c (0, vals), level, i,       new.inc)
      j = recurse (j, c (1, vals), level, i + inc, new.inc)
    }  
  }
  
  j = data.frame ()
  level = length (vars)
  # Walk the list of vars as a binary tree, instantiating each with a value of 0 or 1.
  j = recurse (j, c(), level, 1, 2**(level - 1))
  colnames (j) = c (vars, "p")
  joint.sorted (joint.w.rownames (j))
}

#############################################################################
# Same as joint.cc2.ac where the other effect (e2) has its own effect (e3).
#############################################################################
joint.cgm.cc2.ac.ee = function (c, m, b) {
  cc = cptable (~cc,           binary.levels, pt (c))
  ac = cptable (~ac,           binary.levels, pt (c))
  e1 = cptable (~e1 + cc + ac, binary.levels, cpt.ic2e1 (m, m, b))
  e2 = cptable (~e2 + cc,      binary.levels, cpt.c1e1 (m, b))
  e3 = cptable (~e3 + e1,      binary.levels, cpt.c1e1 (m, b))
  cgm = grain (compileCPT (list (cc, ac, e1, e2, e3))) # Build CGM
  vars = c("cc", "ac", "e1", "e2", "e3")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
}

#############################################################################
# A merge with an extra effect (ae) of one of the root causes (ra).
#############################################################################
joint.merge.ae = function (c, m, b) {
  ra       = cptable (~ra,          binary.levels, pt (c))
  rb       = cptable (~rb,          binary.levels, pt (c))
  ae.ra    = cptable (~ae + ra,     binary.levels, cpt.c1e1 (m, b))
  i.ra.rb  = cptable (~i + ra + rb, binary.levels, cpt.ic2e1 (m, m, b))
  t.i      = cptable (~t + i,       binary.levels, cpt.c1e1 (m, b))
  cgm = grain (compileCPT (list (ra, rb, ae.ra, i.ra.rb, t.i))) # Build CGM
  vars = c("ra", "rb", "ae", "i", "t")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# A common effect structure in which one of the causes (z) is an effect in a common cause structure.
#############################################################################
joint.cc.ce = function (c, m, b) {
  cc     = cptable (~cc,         binary.levels, pt (c))
  c      = cptable (~c,          binary.levels, pt (c))
  e.cc   = cptable (~e + cc,     binary.levels, cpt.c1e1 (m, b))
  z.cc   = cptable (~z + cc,     binary.levels, cpt.c1e1 (m, b))
  ce.z.c = cptable (~ce + z + c, binary.levels, cpt.ic2e1 (m, m, b))
  cgm = grain (compileCPT (list (cc, c, e.cc, z.cc, ce.z.c))) # Build CGM
  vars = c("cc", "e", "z", "c", "ce")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# A fork with an extra cause (ac) of one of the effects (t1).
#############################################################################
joint.fork.ac = function (c, m, b) {
  r       = cptable (~r,           binary.levels, pt (c))
  ac      = cptable (~ac,          binary.levels, pt (c))
  i.r     = cptable (~i + r,       binary.levels, cpt.c1e1  (m, b))
  ta.i.ac = cptable (~ta + i + ac, binary.levels, cpt.ic2e1 (m, m, b))
  tb.i    = cptable (~tb + i,      binary.levels, cpt.c1e1  (m, b))
  cgm = grain (compileCPT (list (r, ac, i.r, ta.i.ac, tb.i))) # Build CGM
  vars = c("r", "ac", "i", "ta", "tb")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
}


#############################################################################
# ic2e1 where one of the causes has an additional effect
#############################################################################
joint.ic2.ae.full = function (c.a, c.b, m.a.e, m.b.e, m.a.ae, b.e, b.ae) {
  ca        = cptable (~ca,          binary.levels, pt (c.a))
  cb        = cptable (~cb,          binary.levels, pt (c.b))
  ae.ca     = cptable (~ae + ca,     binary.levels, cpt.c1e1 (m.a.ae, b.ae))
  e.ca.cb   = cptable (~e + ca + cb, binary.levels, cpt.ic2e1 (m.a.e, m.b.e, b.e))
  cgm = grain (compileCPT (list (ca, cb, ae.ca, e.ca.cb))) # Build CGM
  vars = c("ca", "cb", "ae", "e")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# cc2e1 where one of the causes has an additional effect
#############################################################################
joint.cc2.ae.full = function (c.a, c.b, m.e, m.a.ae, b.e, b.ae) {
  ca        = cptable (~ca,          binary.levels, pt (c.a))
  cb        = cptable (~cb,          binary.levels, pt (c.b))
  ae.ca     = cptable (~ae + ca,     binary.levels, cpt.c1e1 (m.a.ae, b.ae))
  e.ca.cb   = cptable (~e + ca + cb, binary.levels, cpt.cc2e1 (m.e, b.e))
  cgm = grain (compileCPT (list (ca, cb, ae.ca, e.ca.cb))) # Build CGM
  vars = c("ca", "cb", "ae", "e")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# An effect with one independent cause and two conjunctive causes.
#############################################################################
joint.ic1cc2e1 = function (c, m, b) {
  ic  = cptable (~ic,                 binary.levels, pt (c))
  cca = cptable (~cca,                binary.levels, pt (c))
  ccb = cptable (~ccb,                binary.levels, pt (c))
  e   = cptable (~e + ic + cca + ccb, binary.levels, cpt.ic1cc2e1 (m, m, b))
  cgm = grain (compileCPT (list (ic, cca, ccb, e))) # Build CGM
  vars = c("ic", "cca", "ccb", "e")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
}

joint.ic1cc2e1.full = function (c.ic, c.cca, c.ccb, m.i, m.cc, b) {
  ic  = cptable (~ic,                 binary.levels, pt (c.ic))
  cca = cptable (~cca,                binary.levels, pt (c.cca))
  ccb = cptable (~ccb,                binary.levels, pt (c.ccb))
  e   = cptable (~e + ic + cca + ccb, binary.levels, cpt.ic1cc2e1 (m.i, m.cc, b))
  cgm = grain (compileCPT (list (ic, cca, ccb, e))) # Build CGM
  vars = c("ic", "cca", "ccb", "e")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
}

#############################################################################
# A merge: r1->i<-r2, i->t.
#############################################################################
joint.merge = function (c, m, b) {
  ra       = cptable (~ra,          binary.levels, pt (c))
  rb       = cptable (~rb,          binary.levels, pt (c))
  i.ra.rb  = cptable (~i + ra + rb, binary.levels, cpt.ic2e1 (m, m, b))
  t.i      = cptable (~t + i,       binary.levels, cpt.c1e1 (m, b))
  cgm = grain (compileCPT (list (ra, rb, i.ra.rb, t.i))) # Build CGM
  vars = c("ra", "rb", "i", "t")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# A fork: r->i, t1<-i->t2.
#############################################################################
joint.fork = function (c, m, b) {
  r    = cptable (~r,      binary.levels, pt (c))
  i.r  = cptable (~i + r,  binary.levels, cpt.c1e1 (m, b))
  ta.i = cptable (~ta + i, binary.levels, cpt.c1e1 (m, b))
  tb.i = cptable (~tb + i, binary.levels, cpt.c1e1 (m, b))
  cgm = grain (compileCPT (list (r, i.r, ta.i, tb.i))) # Build CGM
  vars = c("r", "i", "ta", "tb")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# A pair of conjunctive causes with the same effect e.
#############################################################################
joint.double.cc2 = function (c, m, b) {
  ca1 = cptable (~ca1, binary.levels, pt (c))
  ca2 = cptable (~ca2, binary.levels, pt (c))
  cb1 = cptable (~cb1, binary.levels, pt (c))
  cb2 = cptable (~cb2, binary.levels, pt (c))
  e   = cptable (~e + ca1 + ca2 + cb1 + cb2, binary.levels, cpt.double.cc2 (m, m, b))
  cgm = grain (compileCPT (list (ca1, ca2, cb1, cb2, e))) # Build CGM
  vars = c("ca1", "ca2", "cb1", "cb2", "e")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# Three independent common effect networks linked together via their causes.
#############################################################################
joint.triple.ic2e1 = function (c, m, b) {
  c1 = cptable (~c1, binary.levels, pt (c))
  c2 = cptable (~c2, binary.levels, pt (c))
  c3 = cptable (~c3, binary.levels, pt (c))
  c4 = cptable (~c4, binary.levels, pt (c))
  e1 = cptable (~e1 + c1 + c2, binary.levels, cpt.ic2e1 (m, m, b))
  e2 = cptable (~e2 + c2 + c3, binary.levels, cpt.ic2e1 (m, m, b))
  e3 = cptable (~e3 + c3 + c4, binary.levels, cpt.ic2e1 (m, m, b))
  cgm = grain (compileCPT (list (c1, c2, c3, c4, e1, e2, e3))) # Build CGM
  vars = c("c1", "c2", "c3", "c4", "e1", "e2", "e3")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars);
} 

#############################################################################
# CC plus 1 or more intermediate causes plus targets pluse ce
#############################################################################  
joint.c1i1t4e1 = function (r.c, m.r.i, m.i.t, m.t.ce) {
  r    = cptable (~r,      binary.levels, pt (r.c))
  i    = cptable (~i + r,  binary.levels, cpt.c1e1 (m.r.i, 0))
  t1   = cptable (~t1 + i, binary.levels, cpt.c1e1 (m.i.t, 0))
  t2   = cptable (~t2 + i, binary.levels, cpt.c1e1 (m.i.t, 0))
  t3   = cptable (~t3 + i, binary.levels, cpt.c1e1 (m.i.t, 0))
  t4   = cptable (~t4 + i, binary.levels, cpt.c1e1 (m.i.t, 0))
  ce   = cptable (~ce + t1 + t2 + t3 + t4, binary.levels, cpt.ic4e1 (m.t.ce, m.t.ce, m.t.ce, m.t.ce, 0))
  
  cgm = grain (compileCPT (list (r, i, t1, t2, t3, t4, ce))) # Build CGM
  vars = c("r", "i", "t1", "t2", "t3", "t4", "ce")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars)
}

joint.c1i2t4e1 = function (r.c, m.r.i, m.i.t, m.t.ce) {
  r    = cptable (~r,      binary.levels, pt (r.c))
  i1   = cptable (~i1 + r,  binary.levels, cpt.c1e1 (m.r.i, 0))
  i2   = cptable (~i2 + r,  binary.levels, cpt.c1e1 (m.r.i, 0))
  t1   = cptable (~t1 + i1, binary.levels, cpt.c1e1 (m.i.t, 0))
  t2   = cptable (~t2 + i1, binary.levels, cpt.c1e1 (m.i.t, 0))
  t3   = cptable (~t3 + i2, binary.levels, cpt.c1e1 (m.i.t, 0))
  t4   = cptable (~t4 + i2, binary.levels, cpt.c1e1 (m.i.t, 0))
  ce   = cptable (~ce + t1 + t2 + t3 + t4, binary.levels, cpt.ic4e1 (m.t.ce, m.t.ce, m.t.ce, m.t.ce, 0))
  
  cgm = grain (compileCPT (list (r, i1, i2, t1, t2, t3, t4, ce))) # Build CGM
  vars = c("r", "i1", "i2", "t1", "t2", "t3", "t4", "ce")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars)
}

#############################################################################
# A tree: A root cause (r), with two intermediate effects (b1 and b2), which each have two effects (e's)
#############################################################################  
joint.tree = function (c, m, b) {
  r      = cptable (~r,        binary.levels, pt (c))
  ba.r   = cptable (~ba + r,   binary.levels, cpt.c1e1 (m, b))
  bb.r   = cptable (~bb + r,   binary.levels, cpt.c1e1 (m, b))
  ea1.ba = cptable (~ea1 + ba, binary.levels, cpt.c1e1 (m, b))
  ea2.ba = cptable (~ea2 + ba, binary.levels, cpt.c1e1 (m, b))
  eb1.bb = cptable (~eb1 + bb, binary.levels, cpt.c1e1 (m, b))
  eb2.bb = cptable (~eb2 + bb, binary.levels, cpt.c1e1 (m, b))
  cgm = grain (compileCPT (list (r, ba.r, bb.r, ea1.ba, ea2.ba, eb1.bb, eb2.bb))) # Build CGM
  vars = c("r", "ba", "bb", "ea1", "ea2", "eb1", "eb2")
  to.joint.as.data.frame (querygrain (cgm, nodes = vars, type = "joint"), vars)
}

#############################################################################
# Randomly generated causal structure (with random params)
#############################################################################
# TODO: add c parameter, check marginals using v.p() 
random.cgm.topology.joint =function (n, density, c, m, b) {
    # Matrix representing every possible causal link.
    # Each of the n**2 - n rows is an index into the square strength matrix.
    possible.links = t (combn (1:n, 2, simplify=T))
    # Number of links to have non-zero entries in the strength matrix.
    max.no.links = nrow (possible.links)
    no.links = round (density * max.no.links)
    # Randomly pick a subset of the possible links.
    links = possible.links[sample (1:max.no.links, no.links),]
    # Initialize stength matrix to all zeros.
    ms = matrix (0, nrow=n, ncol=n)
    # Set the subset of cells of stength matrix to non-zero values.
    ms [links] = m
    
    root.idx = apply(ms, 2, function(ms) all(ms==0))
    #browser()
    bs = ifelse(root.idx,c,b)
    rownames (ms) = sapply (1:n, function (i) paste ('x', i, sep=''))
    joint.cgm.generic (ms, bs)
}