# Shamelessly stolen from https://github.com/h3abionet/TADA/blob/dev/dsl1/bin/VariableLenMergePairs-Pooled.R

# This is a modified copy of the default mergePairs() method; this can
# retain unmerged reads in case they don't overlap (this happens with some
# amplicon like ITS). 

mergePairsRescue <- function(dadaF, derepF, dadaR, derepR,
                             minOverlap = 12, maxMismatch=0, returnRejects=FALSE,
                             propagateCol=character(0), justConcatenate=FALSE,
                             trimOverhang=FALSE, verbose=FALSE, rescueUnmerged=FALSE, ...) {
  if(is(dadaF, "dada")) dadaF <- list(dadaF)
  if(is(derepF, "derep")) derepF <- list(derepF)
  if(is(dadaR, "dada")) dadaR <- list(dadaR)
  if(is(derepR, "derep")) derepR <- list(derepR)
  if( !(dada2:::is.list.of(dadaF, "dada") && dada2:::is.list.of(dadaR, "dada")) ) {
    stop("dadaF and dadaR must be provided as dada-class objects or lists of dada-class objects.")
  }
  if( !( (dada2:::is.list.of(derepF, "derep") || is(derepF, "character")) &&
         (dada2:::is.list.of(derepR, "derep") || is(derepR, "character")) )) {
    stop("derepF and derepR must be provided as derep-class objects or as character vectors of filenames.")
  }
  nrecs <- c(length(dadaF), length(derepF), length(dadaR), length(derepR))
  if(length(unique(nrecs))>1) stop("The dadaF/derepF/dadaR/derepR arguments must be the same length.")
  
  rval <- lapply(seq_along(dadaF), function (i)  {
    mapF <- dada2:::getDerep(derepF[[i]])$map
    mapR <- dada2:::getDerep(derepR[[i]])$map
    if(!(is.integer(mapF) && is.integer(mapR))) stop("Incorrect format of $map in derep-class arguments.")
    #    if(any(is.na(rF)) || any(is.na(rR))) stop("Non-corresponding maps and dada-outputs.")
    if(!(length(mapF) == length(mapR) && max(mapF) == length(dadaF[[i]]$map) &&
         max(mapR) == length(dadaR[[i]]$map))) {
      stop("Non-corresponding derep-class and dada-class objects.")
    }
    rF <- dadaF[[i]]$map[mapF]
    rR <- dadaR[[i]]$map[mapR]
    
    pairdf <- data.frame(sequence = "", abundance=0, forward=rF, reverse=rR)
    ups <- unique(pairdf) # The unique forward/reverse pairs of denoised sequences
    keep <- !is.na(ups$forward) & !is.na(ups$reverse)
    ups <- ups[keep, ]
    if (nrow(ups)==0) {
      outnames <- c("sequence", "abundance", "forward", "reverse",
                    "nmatch", "nmismatch", "nindel", "prefer", "accept")
      ups <- data.frame(matrix(ncol = length(outnames), nrow = 0))
      names(ups) <- outnames
      if(verbose) {
        message("No paired-reads (in ZERO unique pairings) successfully merged out of ", nrow(pairdf), " pairings) input.")
      }
      return(ups)
    } else {
      Funqseq <- unname(as.character(dadaF[[i]]$clustering$sequence[ups$forward]))
      Runqseq <- rc(unname(as.character(dadaR[[i]]$clustering$sequence[ups$reverse])))
      if (justConcatenate == TRUE) {
        # Simply concatenate the sequences together
        ups$sequence <- mapply(function(x,y) paste0(x,"NNNNNNNNNN", y),
                               Funqseq, Runqseq, SIMPLIFY=FALSE);
        ups$nmatch <- 0
        ups$nmismatch <- 0
        ups$nindel <- 0
        ups$prefer <- NA
        ups$accept <- TRUE
      } else {
        # Align forward and reverse reads.
        # Use unbanded N-W align to compare forward/reverse
        # Adjusting align params to prioritize zero-mismatch merges
        tmp <- getDadaOpt(c("MATCH", "MISMATCH", "GAP_PENALTY"))
        if(maxMismatch==0) {
          setDadaOpt(MATCH=1L, MISMATCH=-64L, GAP_PENALTY=-64L)
        } else {
          setDadaOpt(MATCH=1L, MISMATCH=-8L, GAP_PENALTY=-8L)
        }
        alvecs <- mapply(function(x,y) nwalign(x,y,band=-1,...), Funqseq, Runqseq, SIMPLIFY=FALSE)
        setDadaOpt(tmp)
        outs <- t(sapply(alvecs, function(x) dada2:::C_eval_pair(x[1], x[2])))
        ups$nmatch <- outs[,1]
        ups$nmismatch <- outs[,2]
        ups$nindel <- outs[,3]
        ups$prefer <- 1 + (dadaR[[i]]$clustering$n0[ups$reverse] > dadaF[[i]]$clustering$n0[ups$forward])
        ups$accept <- (ups$nmatch >= minOverlap) & ((ups$nmismatch + ups$nindel) <= maxMismatch)
        # Make the sequence
        ups$sequence <- mapply(dada2:::C_pair_consensus, sapply(alvecs,`[`,1), sapply(alvecs,`[`,2), ups$prefer, trimOverhang);
        # Additional param to indicate whether 1:forward or 2:reverse takes precedence
        # Must also strip out any indels in the return
        # This function is only used here.
      }
      
      # Add abundance and sequence to the output data.frame
      tab <- table(pairdf$forward, pairdf$reverse)
      ups$abundance <- tab[cbind(ups$forward, ups$reverse)]
      if (rescueUnmerged == TRUE) {
        rescue <- which(!ups$accept)
        ups$sequence[rescue] <- mapply(function(x,y) paste0(x,"NNNNNNNNNN", y),
                                       Funqseq[rescue], Runqseq[rescue], SIMPLIFY=FALSE);
      } else {
        ups$sequence[!ups$accept] <- ""
      }
      # Add columns from forward/reverse clustering
      propagateCol <- propagateCol[propagateCol %in% colnames(dadaF[[i]]$clustering)]
      for(col in propagateCol) {
        ups[,paste0("F.",col)] <- dadaF[[i]]$clustering[ups$forward,col]
        ups[,paste0("R.",col)] <- dadaR[[i]]$clustering[ups$reverse,col]
      }
      # Sort output by abundance and name
      ups <- ups[order(ups$abundance, decreasing=TRUE),]
      rownames(ups) <- NULL
      if(verbose) {
        message(sum(ups$abundance[ups$accept]), " paired-reads (in ", sum(ups$accept), " unique pairings) successfully merged out of ", sum(ups$abundance), " (in ", nrow(ups), " pairings) input.")
      }
      if(!returnRejects) { ups <- ups[ups$accept,] }
      
      if(any(duplicated(ups$sequence))) {
        message("Duplicate sequences in merged output.")
      }
      return(ups)
    }
  })
  if(!is.null(names(dadaF))) names(rval) <- names(dadaF)
  if(length(rval) == 1) rval <- rval[[1]]
  
  return(rval)
}
