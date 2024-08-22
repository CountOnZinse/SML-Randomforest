cforest <- function
(
  formula,
  data,   
  weights,
  subset, 
  offset, 
  cluster,
  strata,
  na.action = na.pass,
  control = ctree_control(
    teststat = "quad", testtype = "Univ", mincriterion = 0,
    saveinfo = FALSE, ...),
  ytrafo = NULL, 
  scores = NULL, 
  ntree = 500L, 
  perturb = list(replace = FALSE, fraction = 0.632),
  mtry = ceiling(sqrt(nvar)), 
  applyfun = NULL,
  cores = NULL, 
  trace = FALSE,
  ...
) {
  
  ### get the call and the calling environment for .urp_tree
  call <- match.call(expand.dots = TRUE)
  oweights <- NULL
  if (!missing(weights))
    oweights <- weights
  m <- match(c("formula", "data", "subset", "na.action", "offset", "cluster", 
               "scores", "ytrafo", "control", "converged"), names(call), 0L)
  ctreecall <- call[c(1L, m)]
  ctreecall$doFit <- FALSE
  if (!is.null(oweights))
    ctreecall$weights <- 1:NROW(oweights)
  ctreecall$control <- control ### put ... into ctree_control()
  ctreecall[[1L]] <- quote(partykit::ctree)
  tree <- eval(ctreecall, parent.frame())
  
  if (is.null(control$update))
    control$update <- is.function(ytrafo)
  
  d <- tree$d
  updatefun <- tree$update
  
  nvar <- sum(d$variables$z > 0)
  control$mtry <- mtry
  control$applyfun <- lapply
  
  strata <- d[["(strata)"]]
  if (!is.null(strata)) {
    if (!is.factor(strata)) stop("strata is not a single factor")
  }
  
  probw <- NULL
  iweights <- model.weights(model.frame(d))
  if (!is.null(oweights)) {
    if (is.matrix(oweights)) {
      weights <- oweights[iweights,,drop = FALSE]
    } else {
      weights <- oweights[iweights]
    }
  } else {
    weights <- NULL
  }
  rm(oweights)
  rm(iweights)
  N <- nrow(model.frame(d))
  rw <- NULL
  if (!is.null(weights)) {
    if (is.matrix(weights)) {
      if (ncol(weights) == ntree && nrow(weights) == N) {
        rw <- unclass(as.data.frame(weights))
        rw <- lapply(rw, function(w) 
          rep(1:length(w), w))
        weights <- integer(0)
      } else {
        stop(sQuote("weights"), "argument incorrect")
      }
    } else {
      probw <- weights / sum(weights)
    }
  } else {
    weights <- integer(0)
  }
  
  idx <- .start_subset(d)
  
  frctn <- pmin(1, sum(perturb$fraction))
  
  if (is.null(rw)) {
    ### for honesty testing purposes only 
    if (frctn == 1) {
      rw <- lapply(1:ntree, function(b) idx)
    } else {
      if (is.null(strata)) {
        size <- N
        if (!perturb$replace) size <- floor(size * frctn)
        rw <- replicate(ntree, 
                        sample(idx, size = size, 
                               replace = perturb$replace, prob = probw[idx]),
                        simplify = FALSE)
      } else {
        frac <- if (!perturb$replace) frctn else 1
        rw <- replicate(ntree, function() 
          do.call("c", tapply(idx, strata[idx], 
                              function(i) 
                                sample(i, size = length(i) * frac, 
                                       replace = perturb$replace, prob = probw[i]))))
      }
    }
  }
  
  ### honesty: fraction = c(p1, p2) with p1 + p2 <= 1
  ### p1 is the fraction of samples used for tree induction
  ### p2 is the fraction used for honest predictions (nearest neighbor
  ### weights)
  ### works for subsampling only
  if (!perturb$replace && length(perturb$fraction) == 2L) {
    frctn <- perturb$fraction[2L]
    if (is.null(strata)) {
      size <- N
      if (!perturb$replace) size <- floor(size * frctn)
      hn <- lapply(1:ntree, function(b)
        sample(rw[[b]], size = size, 
               replace = perturb$replace, prob = probw[rw[[b]]]))
    } else {
      frac <- if (!perturb$replace) frctn else 1
      hn <- lapply(1:ntree, function(b)
        do.call("c", tapply(rw[[b]], strata[rw[[b]]], 
                            function(i) 
                              sample(i, size = length(i) * frac, 
                                     replace = perturb$replace, prob = probw[i]))))
    }
    rw <- lapply(1:ntree, function(b) rw[[b]][!(rw[[b]] %in% hn[[b]])])
    tmp <- hn
    hn <- rw
    rw <- tmp
  } else {
    hn <- NULL
  }
  
  ## apply infrastructure for determining split points
  ## use RNGkind("L'Ecuyer-CMRG") to make this reproducible
  if (is.null(applyfun)) {
    applyfun <- if(is.null(cores)) {
      lapply  
    } else {
      function(X, FUN, ...)
        parallel::mclapply(X, FUN, ..., mc.set.seed = TRUE, mc.cores = cores)
    }
  }
  
  trafo <- updatefun(sort(rw[[1]]), integer(0), control, doFit = FALSE)
  if (trace) pb <- txtProgressBar(style = 3) 
  forest <- applyfun(1:ntree, function(b) {
    if (trace) setTxtProgressBar(pb, b/ntree)
    ret <- updatefun(sort(rw[[b]]), integer(0), control)
    ### honesty: prune-off empty nodes
    if (!is.null(hn)) {
      nid <- nodeids(ret$nodes, terminal = TRUE)
      nd <- unique(fitted_node(ret$nodes, data = d$data, obs = hn[[b]]))
      prn <- nid[!nid %in% nd]
      if (length(prn) > 0)
        ret <- list(nodes = nodeprune(ret$nodes, ids = prn), trafo = ret$trafo)
    }
    # trafo <<- ret$trafo
    ret$nodes
  })
  if (trace) close(pb)
  
  fitted <- data.frame(idx = 1:N)  
  mf <- model.frame(d)
  fitted[[2]] <- mf[, d$variables$y, drop = TRUE]
  names(fitted)[2] <- "(response)"
  if (length(weights) > 0)
    fitted[["(weights)"]] <- weights
  
  ### turn subsets in weights (maybe we can avoid this?)
  rw <- lapply(rw, function(x) as.integer(tabulate(x, nbins = length(idx))))
  
  control$applyfun <- applyfun
  
  ret <- constparties(nodes = forest, data = mf, weights = rw,
                      fitted = fitted, terms = d$terms$all,
                      info = list(call = match.call(), control = control))
  if (!is.null(hn))
    ret$honest_weights <- lapply(hn, function(x) 
      as.integer(tabulate(x, nbins = length(idx))))
  
  ret$trafo <- trafo
  ret$predictf <- d$terms$z
  class(ret) <- c("cforest", class(ret))
  
  return(ret)
}