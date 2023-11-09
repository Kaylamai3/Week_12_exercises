prot_significant <- drto_psu_density %>%
  group_by(SPECIES_CD) %>%
  nest() %>% #this makes a new data frame or table of selecetd data 
  mutate(exp_test = map(.x=data, ~kruskal.test(density~PROT, data=.) %>%
                          tidy())) %>%
  unnest(exp_test) %>%
  mutate(p.exp = p.adjust(p.value, method = "BH")) %>% #this is adjusting to the p-value because you can accumulate bias when running a lot of lines of data over and over again 
  select(SPECIES_CD, data, p.exp) %>%
  filter(p.exp < 0.05) # this will filter out all p-vlaues that are significat

prot_significant
View(prot_significant)

prot_pairwise_dens <- prot_significant %>%
  mutate(pairwise = map(.x=data,
                        ~pairwise.wilcox.test(x = .x$density,
                                              g = .x$PROT,
                                              p.adjust.methods = "BH") %>%
                       tidy())) %>%
  unnest(pairwise) %>%
  filter(p.value < 0.05)
prot_pairwise_dens
