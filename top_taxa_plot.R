# From a phyloseq object, find the top N abundant species, aggregate
# the others into an "Others" category and produce a bar_plot showing
# the average relative abundance of samples per metadata.

# This code uses relative abundances per sample, otherwise the aggregation
# of abundances is biased by the difference in the number of total sequences 
# per sample. 

library(dplyr, magrittr, phyloseq)

topN <- 20 # <<< how many top taxa
taxRank <- 'Genus' # <<< which taxonomic rank 

# Melt the phyloseq object, agglomerate taxonomy and standardize abundances
ps.melt <- moss.ps %>% # <<< !! your phyloseq object
  tax_glom(taxRank) %>% # aggregate at desired taxrank
  transform_sample_counts(function(x) x/sum(x)) %>%  # normalise counts to 1 by sample
  psmelt %>% # melt the object in a single (massive) table
  select(!!sym(taxRank), Abundance, # psmelt creates an Abundance variable
         Host, Compartment) # <<< !! sample data variables you will need

# Create a taxon -> taxon group conversion table
taxonomie_agregee <- ps.melt %>% 
  group_by(!!sym(taxRank)) %>% # mean relative abundance by taxRank:
  summarise(sum_relab = mean(Abundance)) %>% 
  arrange(desc(sum_relab)) %>%          # most abundant first 
  mutate(aggTaxo = as.factor(case_when( # new aggregated taxonomy variable
    row_number() < topN ~ !!sym(taxRank),        # N-1 first keep their taxonomy
    row_number() >= topN ~ 'Others'))) %>% # 'Others' for every other taxa
  select(-sum_relab) # no need for this

# Reorder the vector so that 'Others' is first in plots
taxLvl <- taxonomie_agregee %$% aggTaxo %>% 
  setdiff(., 'Others') %>% # remove Others
  c('Others',.) # put Others first 

# add the aggregated taxonomy variable to the raw data
ps.melt_agg <- left_join(ps.melt, taxonomie_agregee, 
                         by = taxRank) %>% 
  mutate(aggTaxo = factor(aggTaxo, levels = taxLvl)) # reorder factor levels

# simple stacked barplot :
ps.melt_agg %>% 
  ggplot(aes(x = Host, y = Abundance, fill = aggTaxo)) +
  geom_bar(stat = 'identity', 
           position = 'fill') # 
  
# mean abundance by taxa, by some metadata:
ps.melt %>% 
  group_by(aggTaxo, Host) %>% 
  summarise(mean = mean(Abundance),
            sd = sd(Abundance))