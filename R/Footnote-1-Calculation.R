# Citation Question

library(readr)
library(dplyr)
library(magrittr)

df <- read_csv("data/mp_journals.csv")

df %>% 
  select(Authors, Title, journal, `Cited by`) %>%
#  filter(journal == "PoM") %>%
  arrange(desc(`Cited by`)) %>%
  mutate(cbz = scale(`Cited by`)) %>%
  filter(Title == "Songs and emotions: Are lyrics and melodies equal partners?")
