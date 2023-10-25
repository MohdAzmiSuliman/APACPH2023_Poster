# libraries
pacman::p_load(tidyverse,
               bibliometrix,  # bibliometric analysis software
               janitor,       # data cleaning
               stringr,       # data wrangling – string manipulation
               kableExtra)    # nice table

# convert to bibliometrix dataframe
bibds_pm <- convert2df(file = "23-05-22 scopus search.bib",
                       dbsource = "scopus",
                       format = "bibtex")
bibds_pm95_10 <- bibds_pm %>%
  filter(PY <= 2011)
bibds_pm11_22 <- bibds_pm %>%
  filter(PY >= 2011)

# bibliometric analysis (descriptive)
bibres <- biblioAnalysis(bibds_pm, sep = ";")
bibres95_10 <- biblioAnalysis(bibds_pm95_10, sep = ";")
bibres11_22 <- biblioAnalysis(bibds_pm11_22, sep = ";")

# Descriptive Analysis
bibres_summary <- summary(bibres, k = 25)
bibres_summary95_10 <- summary(bibres95_10, k = 25)
bibres_summary11_22 <- summary(bibres11_22, k = 25)

# Figure 1. Publication Per Year
subset1 <- tibble(Year = 1995:2011,
                  n = 1.5 * exp(0.08 * (1995:2011 -1995)))
subset2 <- tibble(Year = 2011:2022,
                  n = 5.5 * exp(0.13 * (2011:2022 -2011)))
pubnoplot <- tibble(Article = rownames(bibds_pm),
                    Year = bibds_pm$PY) %>%
  group_by(Year) %>%
  summarise(n = n()) %>%
  mutate(Group = if_else(Year <= 2010, "1995-2010", "2011-2022")) %>%
  ggplot(aes(x = Year, y = n)) +
  geom_area(alpha = .2) +
  geom_point(aes(colour = Group)) +
  geom_line() +
  geom_line(data = subset1, linetype = 2, linewidth = 1,
            alpha = 0.4, colour = "blue") +
  geom_line(data = subset2, linetype = 4, linewidth = 1,
            alpha = 0.4, colour = "purple") +
  geom_vline(xintercept = 2011, linetype = 3) +
  geom_text(aes(x = 2011, y = 15, label = "Growth Spurt @ 2011",
                angle = 90), vjust = -.5) +
  scale_y_continuous(breaks = seq(2, 26, 4), expand = c(0, 0)) +
  scale_x_continuous(breaks = c(1995, 1999, 2003, 2007, 2011, 2015, 2019, 2022),
                     expand = c(0, 0.2)) +
  scale_y_continuous(sec.axis = sec_axis(~ .)) +
  coord_cartesian(ylim = c(0, 24)) +
  labs(x = "Year", y = "Number of Publication") +
  scale_color_manual(values = c("blue", "purple")) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        panel.border = element_blank(),
        panel.background = element_rect(fill = "transparent")) +
  guides(color = "none")
pubnoplot

# Table 2. Top Cited Articles
tibble(bibres_summary$MostCitedPapers) %>%
  select(DOI, TC, TCperYear) %>%
  arrange(desc(TCperYear)) %>%
  head(n = 10) %>%
  kable() %>% kable_classic()

# Table 2. Top Average Cited Per Year Articles
tibble(Title = bibds_pm$TI,
       Author = bibds_pm$AU,
       Year = bibds_pm$PY,
       DOI = bibds_pm$DI,
       Citations = bibds_pm$TC) %>%
  mutate(across(.cols = c(Title, Author), .fns = str_to_title)) %>%
  arrange(desc(Citations)) %>%
  head(n=10) %>%
  kable() %>% kable_classic()

# Figure 2. Thematic Map
bib_thememap <- thematicMap(bibds_pm, field = "DE", n = 200, minfreq = 20,
                            stemming = F, size = .5, n.labels = 4, repel = T)
plot(bib_thememap$map)

# Other: Countries Rank by Number of Publication
bibres_countrylist <- bibres$Countries
bibres_countrytable <- tibble(Rank = seq_along(bibres_countrylist),
                              Country = rownames(bibres_countrylist),
                              Np = as.integer(bibres_countrylist))
bibressum_countrytable <- tibble(Rank = 1:25,
                                 bibres_summary$TCperCountries) %>%
  rename("Country" = "Country     ") %>%
  mutate(Country = str_trim(Country),
         Country = fct_reorder(Country, Rank),
         `Total Citations` = as.integer(`Total Citations`),
         `Average Article Citations` = as.double(`Average Article Citations`),
         percent = `Total Citations` / sum(`Total Citations`) * 100,
         percent = round(percent,1)) %>%
  inner_join(x = ., y = select(bibres_countrytable, Country, Np),
             by = "Country") %>%
  relocate(percent, .after = `Total Citations`)
bibressum_countrytable

# Other: Country Collaboration Network Plot
bib_concolab <- metaTagExtraction(bibds_pm, Field = "AU_CO", sep = ";")
bib_concolab_NetMatrix <- biblioNetwork(bib_concolab, analysis = "collaboration",
                                        network = "countries", sep = ";")
bib_concolab_Plot <- networkPlot(bib_concolab_NetMatrix,
                                 n = dim(bib_concolab_NetMatrix)[1],
                                 Title = "Country collaboration",
                                 type = "auto",
                                 size=20,
                                 size.cex=T,
                                 edgesize = 2,
                                 labelsize=1,
                                 #edges.min = 1,
                                 remove.isolates = T,
                                 community.repulsion = 0,
                                 cluster = "optimal")


# Other: Institution Rank by Number of Publication
bibres_instlist <- bibres$Affiliations
bibres_insttable <- tibble(Rank = seq_along(bibres_instlist),
                           InstitutionAffiliation = rownames(bibres_instlist),
                           Np = as.integer(bibres_instlist)) %>%
  mutate(InstitutionAffiliation = fct_reorder(InstitutionAffiliation, Rank))
bibres_insttable

# Other: Institution Collaboration Network
bib_educolab_NetMatrix <- biblioNetwork(bibds_pm, analysis = "collaboration",
                                        network = "universities", sep = ";")
bib_educolab_Plot <- networkPlot(bib_educolab_NetMatrix,
                                 n = 100,
                                 cluster = "optimal",
                                 type = "auto",
                                 size.cex = F,
                                 size = 3,
                                 remove.multiple = F,
                                 labelsize=1,
                                 alpha = .7,
                                 edgesize = 1,
                                 edges.min = 2,
                                 remove.isolates = T,
                                 community.repulsion = 0,
                                 Title = "Institutions collaboration")

# Other: Journal Rank by Citation
bibres_sourcelist <- bibres$Sources
bibres_sourcetable <- tibble(Rank = seq_along(bibres_sourcelist),
                             SourceJournal = rownames(bibres_sourcelist),
                             Np = as.integer(bibres_sourcelist)) %>%
  mutate(SourceJournal = fct_reorder(SourceJournal, Rank))
bibres_sourcetable

# Other: Bradford’s Law
bib_bradford <- bradford(bibds_pm)
bib_bradfordtable <- bib_bradford$table %>%
  select(Zone, Freq, Rank) %>%
  tibble() %>%
  group_by(Zone) %>%
  summarise(nSO = n(),
            nArt = sum(Freq),
            RankRange = str_c(min(Rank), max(Rank), sep = "-")) %>%
  mutate(percent = nArt / sum(nArt) * 100,
         percent = round(percent,1))
bib_bradfordtable

# Other: Number of Author per Article
bibds_noaufreq <- bibds_pm %>%
  select(TI, AU, DT) %>%
  tibble() %>%
  mutate(no_auth = str_count(AU, pattern = ";") + 1) %>%
  rename("paper" = "TI", "author" = "AU", "type" = "DT") %>%
  group_by(no_auth, type) %>%
  summarise(freq = n(), .groups = "drop") %>%
  mutate(percent = freq / sum(freq) * 100,
         percent = round(percent,1))
bibds_noaufreq %>%
  ggplot(aes(no_auth, freq)) +
  geom_bar(stat = "identity") +
  labs(x = "Number of Authors", y = "Frequency (Number of Articles)") +
  scale_x_continuous(breaks = seq(-3,30,2)) +
  scale_y_continuous(breaks = seq(-2,40,4)) +
  theme_bw()

# Other: Author Rank by Number of Publication
bibres_aulist <- bibres$Authors
bibres_autable <- tibble(Rank = seq_along(bibres_aulist),
                         Author = rownames(bibres_aulist),
                         Np = as.integer(bibres_aulist)) %>%
  mutate(Author = fct_reorder(Author, Rank))
bibres_autable

# Other: Common Keyword – Author and Keyword Plus
cbind(Rank = 1:25, bibres_summary$MostRelKeywords)
