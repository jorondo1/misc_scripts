library(pacman)
p_load(ggplot2, dplyr, magrittr, MetBrewer)

readxl::read_xlsx("~/OneDrive - USherbrooke/ω-Maitrise/Gant_planning.xlsx") %>%
  mutate(
    across(ends_with("Date"), ~as.Date(.x, format = "%d-%m-%Y")),
    across(where(is.character), as.factor)) %>% 
  mutate(
    Name = factor(Name, levels = unique(.$Name[order(desc(.$StartDate))]))
    ) %>% 
  # Plot
ggplot(aes(x = Name, ymin = StartDate, ymax = EndDate, colour = Type)) +
  geom_linerange(linewidth = 10) + 
  geom_hline(yintercept = Sys.Date(), linetype = 10, colour = "black") + 
  coord_flip() +  theme_bw() +
  scale_color_manual(values = met.brewer('Austria', direction=-1)) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 14)) +
  xlab("") +
  ylab("") +
  ggtitle("Échéancier M.Sc. Jonathan Rondeau-Leclaire")
