### Parse Sourmash output
parse_SM <- function(gather_files) {
  Sys.glob(gather_files) %>% 
    map_dfr(read_csv, col_types="ddddddddcccddddcccd") %>%
    mutate(
      uniqueK = (unique_intersect_bp/scaled)*average_abund,
      genome = str_replace_all(name, c(" .*" = "", ".fa.sig" = "")), # remove _1 from MAG identifiers
      run = str_replace(query_name, "_clean", ""), 
      .keep="unused") %>% 
    dplyr::select(run, uniqueK, genome) %>% 
    pivot_wider(names_from = run,
                values_from = uniqueK) %>% 
    replace(is.na(.), 0) %>% 
    arrange(genome) %>% 
    column_to_rownames("genome") %>% 
    round(digits=0)
}

### Parse MetaPhlAn output
default_colnames <- c('Taxonomy', 'Abundance')
read_filename <- function(filepath, column_names = default_colnames) {
  readLines(filepath) %>% 
    grep("^[^#]", ., value = TRUE) %>% # discard header lines
    textConnection %>% # create connection to chr vector, enables file-reading fun for in-memory strings
    read.table(sep = '\t', header = FALSE, col.names=column_names, 
               quote = "" # otherwise EOF error
               ) %>%
    mutate(sample = basename(filepath) %>% str_replace('_.*', ""))
}

parse_MPA <- function(MPA_files, # path with wildcard to point to all files
                      column_names = default_colnames){ 
  Sys.glob(MPA_files) %>% 
    map(read_filename, column_names) %>% #compact %>% 
    list_rbind %>% # Keep only lines with species, remove duplicates at strain level
    dplyr::filter(str_detect(Taxonomy, "s__") & !str_detect(Taxonomy,"t__")) %>% 
    dplyr::select(sample, Taxonomy, Abundance) %>% 
    mutate(Abundance = as.double(Abundance)) %>% 
    group_by(Taxonomy, sample) %>% 
    summarise(Abundance = sum(Abundance)) %>% # sum strains into species if applicable
    ungroup %>% 
    pivot_wider(names_from = sample, values_from = Abundance, values_fill = 0) %>%
    mutate(Kingdom = str_extract(Taxonomy, "k__[^|]+") %>% str_remove("k__"),
           Phylum = str_extract(Taxonomy, "p__[^|]+") %>% str_remove("p__"),
           Class = str_extract(Taxonomy, "c__[^|]+") %>% str_remove("c__"),
           Order = str_extract(Taxonomy, "o__[^|]+") %>% str_remove("o__"),
           Family = str_extract(Taxonomy, "f__[^|]+") %>% str_remove("f__"),
           Genus = str_extract(Taxonomy, "g__[^|]+") %>% str_remove("g__"),
           Species = str_extract(Taxonomy, "s__[^|]+") %>% str_remove("s__")) %>%
    dplyr::select(-Taxonomy)
  }

### Build phyloseq object from MPA output
make_phylo_MPA <- function(abunTable, sampleData, 
                           # vector with a name for every raw data column, 
                           # MUST include at least "Taxonomy" and "Abundance" :                          
                           raw_data_colnames = default_colnames) {
  
  # Extract abundance table with Species as identifier
  MPA_abund <- abunTable %>% dplyr::select(where(is.double), Species) %>% 
    column_to_rownames('Species') 
  
  # Extract taxonomy
  MPA_tax <- abunTable %>% dplyr::select(where(is.character)) %>% 
    mutate(Species2 = Species) %>% column_to_rownames('Species2') %>% as.matrix
  
  # Build phyloseq
  phyloseq(otu_table(MPA_abund, taxa_are_rows = TRUE),
           sample_data(sampleData),
           tax_table(MPA_tax)
  )
}

# compute Hill numbers
estimate_Hill <- function(ps, q) {
  x <- ps@otu_table %>% as("matrix")
  if (taxa_are_rows(ps)) { 
    x <- t(x) 
  }
  total <- rowSums(x)
  x <- sweep(x, 1, total, "/")
  
  if (q == 0) {  # Species richness
    div <- rowSums(x > 0)
  } else if (q == 1) { # Shannon diversity (exponential of Shannon entropy)
    div <- exp(-rowSums(x * log(x, base = exp(1)), na.rm = TRUE))
  } else {  # Hill number formula for q ≠ 0 and q ≠ 1
    div <- rowSums(x^q)^(1 / (1 - q))
  }
  return(div)
}

# Esitmate diversity (Shannon, Simpson, Tail)
estimate_diversity <- function(ps, index = 'Shannon') {
  x <- ps@otu_table %>% as("matrix")
  if (taxa_are_rows(ps)) { 
    x <- t(x) 
  }
  total <- apply(x, 1, sum)
  x <- sweep(x, 1, total, "/")
  
  if(index == 'Tail') {
    tail_stat <- function(row) {
      values <- sort(row, decreasing = TRUE)
      sqrt(sum(values * ((seq_along(values)-1)^2)))
    }
    div <- apply(x, 1, tail_stat)
  }
  if(index == 'Shannon') {
    x <- -x * log(x, exp(1))
    div <- apply(x, 1, sum, na.rm = TRUE)
  }
  if(index == 'Simpson') {
    div <- 1 - apply((x * x), 1, sum, na.rm = TRUE) 
  }
  if(index == 'Richness') {
    div <- apply(x, 1, function(x) sum(x != 0))
  }
  return(div)
}
