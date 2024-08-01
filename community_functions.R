### Parse Sourmash output
parse_SM <- function(gather_files) {
  Sys.glob(gather_files) %>% 
    map_dfr(read_csv, col_types="ddddddddcccddddcccd") %>%
    mutate(
      uniqueK = (unique_intersect_bp/scaled)*average_abund,
      genome = str_replace_all(name, c(" .*" = "", ".fa.sig" = "")), # remove _1 from MAG identifiers
      run = str_replace(query_name, "_clean", ""), 
      .keep="unused") %>% 
    select(run, uniqueK, genome) %>% 
    pivot_wider(names_from = run,
                values_from = uniqueK) %>% 
    replace(is.na(.), 0) %>% 
    arrange(genome) %>% 
    column_to_rownames("genome") %>% 
    round(digits=0)
}

### Parse MetaPhlAn output
read_filename <- function(filepath) {
  readLines(filepath) %>% 
    grep("^[^#]", ., value = TRUE) %>% # discard header lines
    textConnection %>% # create connection to chr vector, enables file-reading fun for in-memory strings
    read.table(sep = '\t', header = FALSE, col.names=c('Taxonomy', 'NCBI','Abundance', 'Void')) %>%
    mutate(sample = basename(filepath) %>% str_remove('_profile.txt'))
}

parse_MPA <- function(MPA_files){
  Sys.glob(MPA_files) %>% 
    map(read_filename) %>% #compact %>% 
    list_rbind %>% # Keep only lines with species, remove duplicates at strain level
    dplyr::filter(str_detect(Taxonomy, "s__") & !str_detect(Taxonomy,"t__")) %>% 
    dplyr::select(sample, Taxonomy, Abundance) %>% 
    mutate(Abundance = as.double(Abundance)) %>% 
    pivot_wider(names_from = sample, values_from = Abundance, values_fill = 0) %>%
    mutate(Taxonomy = str_remove_all(Taxonomy, "\\w__\\|?")) %>%
    separate(Taxonomy, into = c('Kingdom', 'Phylum', 'Class', 'Order', 'Family', 'Genus', 'Species'), 
             sep = "\\|")
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
