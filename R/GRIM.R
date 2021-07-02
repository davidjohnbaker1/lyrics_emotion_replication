#----------------------------------------------
# GRIM Notes
#----------------------------------------------

set_one <- c(17,19,19,20,20,21,21,21,21,22,24,26)

set_two <- c(18,19,19,20,20,21,21,21,21,22,24,26)

set_one
set_two

mean(set_one) - mean(set_two)

32*1
32*9

ints <- c(184,185,186)
round(ints/32, 2) 

32 * 5.79

5.79 

# multiply sample size by mean to get two closest interger values 
# Divide new bookend ranges by sample size 
# 

grim_test <- function(sample_size, likert_start, likert_end, vector_of_means){
  integer_values <- sample_size * vector_of_means
  bottom <- round(floor(integer_values)/ sample_size, 2)
  top <- round(ceiling(integer_values)/ sample_size, 2)
  df <- data.frame(bottom, top, vector_of_means)
  df$possible <- ifelse(df$bottom == vector_of_means | df$top == vector_of_means, yes = TRUE,no = FALSE)
}

experiment_1_anova_means <- c(7.85, 8.18, 7.96, 6.62, 6.91, 6.72,
                              5.79, 5,20, 5.59, 6.51, 6.11, 6.38,
                              7.75, 7.95, 7.82, 4.05, 5.41, 5.52,
                              5.00, 4.41, 4.80, 6.08, 6.59, 6.26, 
                              6.60, 6.44, 6.54, 5.82, 6.26, 5.97)

table_1_values <- c(7.85, 8.18, 6.62, 6.91, 
                    5.79, 5.20, 6.51, 6.11, 
                    7.75, 7.95, 4.05, 5.41, 
                    5.00, 4.41, 6.08, 6.59)



table_2_values <- c(6.94, 6.42, 4.11, 
                    7.00, 5.73, 5.50,
                    6.47, 5.81, 3.98,
                    6.25, 4.81, 3.41)

table_3_values <- c(5.26, 5.64, 4.69,
                    3.63, 3.74, 2.57,
                    4.61, 5.74, 4.99, 
                    3.29, 2.95, 2.19)

table_4_values <- c(5.03, 5.11, 4.54, 4.31, 
                    3.53, 3.42, 2.86, 2.45,
                    6.38,  5.18, 4.40, 4.57, 
                    3.13, 3.22, 2.57, 2.30)


# Add what ones  are true and  not 


table_1 <- grim_test(32, 1, 9, table_1_values)
table_2 <- grim_test(32, 1, 9, table_2_values)
table_3 <- grim_test(36, 1, 9, table_3_values)
table_4 <- grim_test(36, 1, 9, table_4_values)

table_1

table_1$gender <- rep(c("woman","man"),8)
table_1$lyrics <- rep(c("no lyric", "no lyric", "lyric","lyric"),4)
table_1$emotions <- c(rep("happy",4), rep("sad",4), rep("calm",4), rep("angry",4))



table_1  
  
table_2$condition <- rep(c("mclc","mclm","lcmm"),4)
table_3$condition <- rep(c("lyrics_and_music","music_no_lyrics","no_music"),4)
table_4$condition <- rep(c("melody_lyrics_congruent","melody_congruent_lyrics_mismatch","lyrics_congruent_melody_mismatch","no_music"),4)

library(magrittr)
library(tibble)
library(ggplot2)
library(dplyr)

#  Visualzing Their Results 

# Main Effect of Intended Emotion
table_1 %>%
  tibble() %>%
  group_by(emotions) %>%
  summarise(mean_ratings = mean(vector_of_means))  %>%
  ggplot(aes(y = mean_ratings, 
             x = emotions)) +
  geom_bar(stat = "identity") +
  labs(title = "Main Effect of Intended Emotion") +
  theme_minimal()

# Did not declare global alpha or type i error rate

# I guess they are now looking for valence in the analysis 
table_1 %>%
  tibble() %>%
  mutate(valence = case_when(
    emotions == "happy" ~ "positive",
    emotions == "calm" ~ "positive",
    emotions == "angry" ~ "negative",
    emotions == "sad" ~ "negative"
  )) %>%
  group_by(valence) %>%
  summarise(mean_ratings = mean(vector_of_means))  %>%
  ggplot(aes(y = mean_ratings, 
             x = valence)) +
  geom_bar(stat = "identity") +
  labs(title = "Main Effect of Valence") +
  theme_minimal()

table_1 %>%
  tibble() %>%
  mutate(valence = case_when(
    emotions == "happy" ~ "positive",
    emotions == "calm" ~ "positive",
    emotions == "angry" ~ "negative",
    emotions == "sad" ~ "negative"
  )) %>%
  group_by(lyrics) %>%
  summarise(mean_ratings = mean(vector_of_means))  %>%
  ggplot(aes(y = mean_ratings, 
             x = lyrics)) +
  geom_bar(stat = "identity") +
  labs(title = "Main Effect of Lyrics") +
  theme_minimal()

table_1 %>%
  tibble() %>%
  mutate(valence = case_when(
    emotions == "happy" ~ "positive",
    emotions == "calm" ~ "positive",
    emotions == "angry" ~ "negative",
    emotions == "sad" ~ "negative"
  )) %>%
  group_by(gender) %>%
  summarise(mean_ratings = mean(vector_of_means))  %>%
  ggplot(aes(y = mean_ratings, 
             x = gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Main Effect of gender") +
  theme_minimal()

# significant interaction for women with lyrics 

# what would a simulation look  like?
# not possible unless  have SD for normal distribution
#  but even  with SUPER TIGHT SD, would it work out?

table_1 %>%
  tibble() %>%
  mutate(id = row_number()) %>%
  mutate(id2 = paste(gender, emotions, lyrics)) %>%
  ggplot(aes(y = vector_of_means, 
             x = id, 
             fill  = lyrics)) +
  geom_bar(stat = "identity") +
  facet_wrap(~gender) 


table_1 %>%
  tibble() %>%
  mutate(id = row_number()) %>%
  mutate(id2 = paste(gender, emotions, lyrics)) %>%
  ggplot(aes(y = vector_of_means, 
             x = id, 
             color = possible,
             fill  = gender)) +
  geom_bar(stat = "identity") +
  facet_wrap(~lyrics)

# Why was gender only a factor in the first one?

# What do we expect to find in  our analysis?
# Within congruent conditions
# No  effect of gender in model 
# Effect of Emotions with happy higest
# Min Effect of lyrics





# Experiment 2 , 3 ,4 

# We did not have design for experiment, 2 3 4

