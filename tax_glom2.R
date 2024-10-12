# This modified version of tax_glom updates the index names to the 
# agglomerated taxrank, to avoid e.g. keeping an arbitrary ASV for
# a genus that encompasses multiple ASVs post agglomeration. 

tax_glom2 <- function (physeq, taxrank = rank_names(physeq)[1], NArm = TRUE, 
          bad_empty = c(NA, "", " ", "\t")) 
{
  if (is.null(access(physeq, "tax_table"))) {
    stop("The tax_glom() function requires that physeq contain a taxonomyTable")
  }
  if (!taxrank[1] %in% rank_names(physeq)) {
    stop("Bad taxrank argument. Must be among the values of rank_names(physeq)")
  }
  CN <- which(rank_names(physeq) %in% taxrank[1])
  tax <- as(access(physeq, "tax_table"), "matrix")[, CN]
  if (NArm) {
    keep_species <- names(tax)[!(tax %in% bad_empty)]
    physeq <- prune_taxa(keep_species, physeq)
  }
  tax <- as(access(physeq, "tax_table"), "matrix")[, 1:CN, 
                                                   drop = FALSE]
  tax <- apply(tax, 1, function(i) {
    paste(i, sep = ";_;", collapse = ";_;")
  })
  tax <- tax[!(tax %in% bad_empty)]
  spCliques <- tapply(names(tax), factor(tax), list)
  for (i in names(spCliques)) {
    physeq <- merge_taxa(physeq, spCliques[[i]])
  }
  if (CN < length(rank_names(physeq))) {
    badcolumns <- (CN + 1):length(rank_names(physeq))
    tax_table(physeq)[, badcolumns] <- NA_character_
  }
  # NEW: replace taxnames with taxrank names !
  taxa_names(physeq) <- as.character(tax_table(physeq))[, taxrank]
  
  return(physeq)
}
