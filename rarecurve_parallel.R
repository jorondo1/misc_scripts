rarecurve_parallel <- function (x, step = 1, sample, xlab = "Sample Size", ylab = "Species", 
                                label = TRUE, col, lty, tidy = FALSE, ncores = NULL, ...) 
{
  x <- as.matrix(x)
  if (!identical(all.equal(x, round(x)), TRUE)) 
    stop("function accepts only integers (counts)")
  minobs <- min(x[x > 0])
  if (minobs > 1) 
    warning(gettextf("most observed count data have counts 1, but smallest count is %d", 
                     minobs))
  if (missing(col)) 
    col <- par("col")
  if (missing(lty)) 
    lty <- par("lty")
  tot <- rowSums(x)
  S <- specnumber(x)
  if (any(S <= 0)) {
    message("empty rows removed")
    x <- x[S > 0, , drop = FALSE]
    tot <- tot[S > 0]
    S <- S[S > 0]
  }
  nr <- nrow(x)
  col <- rep(col, length.out = nr)
  lty <- rep(lty, length.out = nr)
  
  # Parallel processing setup
  if (is.null(ncores)) {
    ncores <- parallel::detectCores() - 1  # Leave one core free
  }
  ncores <- min(ncores, nr)  # Don't use more cores than rows
  
  if (ncores > 1) {
    # Use parallel processing
    cl <- parallel::makeCluster(ncores)
    on.exit(parallel::stopCluster(cl))
    
    # Export necessary functions to clusters
    parallel::clusterExport(cl, varlist = c("rarefy"), 
                            envir = environment())
    
    out <- parallel::parLapply(cl, seq_len(nr), function(i) {
      n <- seq(1, tot[i], by = step)
      if (n[length(n)] != tot[i]) {
        n <- c(n, tot[i], use.names = FALSE)
      }
      drop(suppressWarnings(rarefy(x[i, ], n)))
    })
  } else {
    # Use sequential processing (original code)
    out <- lapply(seq_len(nr), function(i) {
      n <- seq(1, tot[i], by = step)
      if (n[length(n)] != tot[i]) {
        n <- c(n, tot[i], use.names = FALSE)
      }
      drop(suppressWarnings(rarefy(x[i, ], n)))
    })
  }
  
  if (tidy) {
    len <- sapply(out, length)
    nm <- rownames(x)
    df <- data.frame(Site = factor(rep(nm, len), levels = nm), 
                     Sample = unlist(lapply(out, attr, which = "Subsample")), 
                     Species = unlist(out))
    return(df)
  }
  Nmax <- sapply(out, function(x) max(attr(x, "Subsample")))
  Smax <- sapply(out, max)
  plot(c(1, max(Nmax)), c(1, max(Smax)), xlab = xlab, ylab = ylab, 
       type = "n", ...)
  if (!missing(sample)) {
    abline(v = sample)
    rare <- sapply(out, function(z) approx(x = attr(z, "Subsample"), 
                                           y = z, xout = sample, rule = 1)$y)
    abline(h = rare, lwd = 0.5)
  }
  for (ln in seq_along(out)) {
    N <- attr(out[[ln]], "Subsample")
    lines(N, out[[ln]], col = col[ln], lty = lty[ln], ...)
  }
  if (label) {
    ordilabel(cbind(tot, S), labels = rownames(x), ...)
  }
  invisible(out)
}