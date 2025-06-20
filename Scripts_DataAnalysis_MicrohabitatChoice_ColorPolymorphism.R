setwd("") # Working directory with the raw data

library(readxl) # Open the raw data files
rawdata_canabal <- read_excel("MicrohabitatChoice_RawData.xlsx", sheet = "HC_2023")
rawdata_portino <- read_excel("MicrohabitatChoice_RawData.xlsx", sheet = "HC_2024")

# Canabal (2023) ----------------------------------------------------------
library(tidyverse)
data_canabal <- rawdata_canabal %>%
  mutate(quadrat = factor(quadrat),
         habitat_in = factor(case_when(habitat_in == 0 ~ "bare rock",
                                       habitat_in == 1 ~ "lichina")),
         snail_ID = factor(snail_ID),
         color = factor(case_when(color == 1 ~ "aurantia",
                                  color == 2 ~ "lineata")),
         scar = factor(scar),
         recapture = factor(case_when(recapture == 0 ~ "lost",
                                      recapture == 1 ~ "recaptured")),
         habitat_out = factor(case_when(habitat_out == 0 ~"bare rock",
                                        habitat_out == 1 ~ "lichina")),
         change = factor(change),
         direction = factor(case_when(direction == -1 ~ "earth",
                                      direction == 1 ~ "sea",
                                      is.na(direction) ~ "undefined")),
         sex = factor(case_when(sex == 0 ~ "immature",
                                sex == 1 ~ "male",
                                sex == 2 ~ "female")))
str(data_canabal)

# Function for conducting contingency G Test and its power analysis
library(DescTools) # G Test
library(pwr) # Power Analysis
GTest_Power <- function(obs) { # obs refers to observed frequencies, i.e. our contingency table for each case
  exp <- outer(rowSums(obs), colSums(obs)) / sum(obs) # Calculation of expected frequencies
  list(obs, # Observed frequencies
       exp, # Expected frequencies
       obs %>% GTest, # G Test performance
       pwr.chisq.test(w = sqrt(sum((obs - exp)^2 / exp) / sum(obs)), # Effect size Index (Cohen)
                      N = sum(obs), # Number of observations
                      df = (nrow(obs) - 1) * (ncol(obs) - 1), # Degrees of freedom
                      sig.level = 0.05, # Alpha
                      power = NULL)) # Power of the model, to be determined
}


GTest_Power_n <- function(obs) { # obs refers to observed frequencies, i.e. our contingency table for each case
  exp <- outer(rowSums(obs), colSums(obs)) / sum(obs) # Calculation of expected frequencies
  list(obs %>% GTest, # G Test performance
       pwr.chisq.test(w = sqrt(sum((obs - exp)^2 / exp) / sum(obs)), # Effect size Index (Cohen)
                      N = NULL, # Number of observations needed of an statistical power of 80%
                      df = (nrow(obs) - 1) * (ncol(obs) - 1), # Degrees of freedom
                      sig.level = 0.05, # Alpha
                      power = 0.8)) # Power of the model set as 0.8 (threshold for a reliable model)
}


# Experiment 1 (Canabal, 2023)  -------------------------------------------------------------
# Kruskal-Wallis test for size between color morphs
data_canabal %>% kruskal.test(size ~ color, .)
# OUTPUT: no differences in shell size between color morphs (P = 0.207 > 0.05)

# Kruskal-Wallis test for microhabitat area between release microhabitats
data_canabal %>% kruskal.test(area ~ habitat_in, .) 
# OUTPUT: bare rock and lichina patches had different areas (p < 0.05)

# G test for qualitative variables for Experiment 1 (Canabal, 2023) ----------------------------------------
# recapture ~ color
recapturecolor_canabal <- data_canabal %>% 
  group_by(color) %>%
  summarise(recaptured = sum(recapture == "recaptured"),
            lost = sum(recapture == "lost")) %>% 
  {matrix(c(.$recaptured, .$lost), nrow = 2, byrow = TRUE)} # Contingency table
recapturecolor_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 6% to detect an effect in case it does exist
recapturecolor_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 16000 snails would be required to achieve statistically valid inferences (80%)

# recapture_habitat ~ color
habitatoutcolor_canabal <- data_canabal %>% 
  filter(recapture == "recaptured") %>% 
  group_by(color) %>% 
  summarise(barerock = sum(habitat_out == "bare rock"),
            lichina = sum(habitat_out == "lichina")) %>% 
  {matrix(c(.$barerock,.$lichina), nrow = 2, byrow = TRUE)} # Contingency table
habitatoutcolor_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 8% to detect an effect in case it does exist
habitatoutcolor_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 2600 snails would be required to achieve statistically valid inferences (80%)

# habitat_change ~ color
changecolor_canabal <- data_canabal %>% filter(recapture == "recaptured") %>% 
  group_by(color) %>% 
  summarise(changed = sum(change == "1"),
            same = sum(change == "0")) %>%
  {matrix(c(.$changed, .$same), nrow = 2, byrow = TRUE)}
changecolor_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 5% to detect an effect in case it does exist
changecolor_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 303000 snails would be required to achieve statistically valid inferences (80%)

# habitat_change ~ habitat_in 
changehabitatin_canabal <- data_canabal %>% filter(recapture == "recaptured") %>% 
  group_by(habitat_in) %>% 
  summarise(changed = sum(change == "1"),
            same = sum(change == "0")) %>% 
  {matrix(c(.$changed, .$same), nrow = 2, byrow = TRUE)}
changehabitatin_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 92% to detect an effect in case it does exist
changehabitatin_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n = 69 snails would be enough to achieve statistically valid inferences (80%)

# recapture ~ scar
scarrecapture_canabal <- data_canabal %>% 
  group_by(recapture) %>% 
  summarise(scarred = sum(scar == "1"),
            non_scarred = sum(scar == "0")) %>% 
  {matrix(c(.$scarred, .$non_scarred), nrow = 2, byrow = TRUE)}
scarrecapture_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 7% to detect an effect in case it does exist
scarrecapture_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n = 5170 snails would be required to achieve statistically valid inferences (80%)

# scar ~ color
scarcolor_canabal <-  data_canabal %>%
  group_by(color) %>% 
  summarise(scarred = sum(scar == "1"),
            non_scarred = sum(scar == "0")) %>% 
  {matrix(c(.$scarred, .$non_scarred), nrow = 2, byrow = TRUE)}
scarcolor_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 99% to detect an effect in case it does exist
scarcolor_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n = 45 snails would be enough to achieve statistically valid inferences (80%)

# sex ~ color
sexcolor_canabal <- data_canabal %>% 
  filter(recapture == "recaptured") %>%
  group_by(sex) %>% 
  summarise(aurantia = sum(color == "aurantia"),
            lineata = sum(color == "lineata")) %>% 
  {matrix(c(.$aurantia, .$lineata), nrow = 2, byrow = TRUE)}
sexcolor_canabal %>% GTest_Power # G Test
# OUTPUT: probability of 19% to detect an effect in case it does exist
sexcolor_canabal %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n = 582 snails would be enough to achieve statistically valid inferences (80%)

# Distance for Experiment 1 (Canabal, 2023) ----------------------------------------------------------------
#Linear regression
glm_canabal <- glm(snail_distance ~ q_distance_mean + q_distance_variance + change + habitat_in + habitat_out + area + color + size + scar + sex, 
                   data = data_canabal %>% filter(recapture == "recaptured"),
                   family = gaussian()) 
glm_canabal %>% step %>% summary
# OUTPUT: only habitat change and release habitat

# Redefine the model with only these variables
glm_canabal_def <- glm(snail_distance ~ change + habitat_in , 
                   data = data_canabal %>% filter(recapture == "recaptured"),
                   family = gaussian())
library(simr) # Power analysis
# Power Analysis for whether a snail changed its habitat
glm_canabal_def %>% powerSim(., fixed("change"), 
                             seed = 123,
                             nsim = 1000)
# OUTPUT: 100% of power with our actual sample size (n = 99)

powerCurve(glm_canabal_def, fixed("change"),
                               along = "snail_ID", 
                               breaks = seq(10, 40, by = 5),
                               seed = 123,
                               nsim = 1000) %>% plot
# OUTPUT: n ~ 24 would be enough to get a high statistical power (80%) regarding the association of the snail distance and whether it was recaptured in a new habitat

# Power Analysis for release habitat predictor
glm_canabal_def %>% powerSim(., fixed("habitat_in"), 
                             seed = 123,
                             nsim = 1000)
# OUTPUT: 82.7% of power with our actual sample size (n = 99)

# Mean distance crawled by those snails that changed their habitat or remained; Figure 2
data_canabal %>% filter(recapture == "recaptured") %>% 
  group_by(change, color) %>% 
  summarise(mean_distance = mean(snail_distance),
            se_distance = sd(snail_distance) / sqrt(n())) %>% 
  ggplot(aes(x = change,
             fill = color)) +
  geom_bar(aes(y = mean_distance),
           stat = "identity",
           position = position_dodge(0.9),
           alpha = 0.2) +
  geom_errorbar(aes(y = mean_distance,
                    ymin = pmax(mean_distance - se_distance, 0), 
                    ymax = mean_distance + se_distance,
                    color = color),
                width = 0.3,,
                position = position_dodge(0.9)) +
  geom_point(aes(y = mean_distance,
                 color = color),
             position = position_dodge(0.9),
             size = 4.5) +
  labs(x = "Habitat change",
       y = "Mean distance (cm)",
       fill = "Color morph",
       color = "Color morph") +
  scale_fill_manual(values = c("aurantia" = "#f5bc80",
                               "lineata" = "#989482")) + 
  scale_color_manual(values = c("aurantia" = "#f5bc80",
                                "lineata" = "#989482")) + 
  theme_classic() +
  theme(text = element_text(size = 20),
        legend.position = "top")


# Experiment 2 (Portino, 2024) ----------------------------------------------------------
data_portino <- rawdata_portino %>% 
  mutate(session = factor(session),
         quadrat = factor(quadrat),
         snail_ID = factor(snail_ID),
         color = factor(case_when(color == 1 ~ "lutea",
                                  color == 2 ~ "lineata")),
         scar = factor(scar),
         recapture = factor(case_when(recapture == 0 | is.na(recapture) ~ "lost",
                                      recapture == 1 ~ "recaptured")),
         habitat = factor(case_when(habitat == 0 ~ "bare rock",
                                    habitat == 1 ~ "balanus",
                                    habitat == 2 ~ "crevice",
                                    habitat == 3 ~ "lichina",
                                    habitat == 4 ~ "tidepool"),
                          levels = c("bare rock", "balanus", "crevice", "lichina", "tidepool")),
         inside = factor(case_when(inside == 0 ~ "outside",
                                   inside == 1 ~ "inside")),
         slope = factor(case_when(slope == -1 ~ "below",
                                  slope == 0 ~ "equal",
                                  slope == 1 ~ "above")),
         direction = factor(case_when(direction == -1 ~ "earth",
                                      direction == 0 ~ "intermediate",
                                      direction == 1 ~ "sea")))
str(data_portino)

# G Tests for qualitative variables for Experiment 2 (Portino, 2024) ------------------------
# recapture ~ color (pooled)
recapturecolor_portino <- data_portino %>% 
  group_by(color) %>%
  summarise(recaptured = sum(recapture == "recaptured"),
            lost = sum(recapture == "lost")) %>% 
  {matrix(c(.$recaptured, .$lost), nrow = 2, byrow = TRUE)} # Contingency table
recapturecolor_portino %>% GTest_Power # G Test
# OUTPUT: probability of 6% to detect an effect in case it does exist
recapturecolor_portino %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 12488 snails would be required to achieve statistically valid inferences (80%)

# recapture ~ color (session 1)
recapturecolor_portino_s1 <- data_portino %>% 
  filter(session == "1") %>% 
  group_by(color) %>%
  summarise(recaptured = sum(recapture == "recaptured"),
            lost = sum(recapture == "lost")) %>% 
  {matrix(c(.$recaptured, .$lost), nrow = 2, byrow = TRUE)} # Contingency table
recapturecolor_portino_s1 %>% GTest_Power # G Test
# OUTPUT: probability of 15% to detect an effect in case it does exist
recapturecolor_portino_s1 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 783 snails would be required to achieve statistically valid inferences (80%)

# recapture ~ color (session 2)
recapturecolor_portino_s2 <- data_portino %>% 
  filter(session == "2") %>% 
  group_by(color) %>%
  summarise(recaptured = sum(recapture == "recaptured"),
            lost = sum(recapture == "lost")) %>% 
  {matrix(c(.$recaptured, .$lost), nrow = 2, byrow = TRUE)} # Contingency table
recapturecolor_portino_s2 %>% GTest_Power # G Test
# OUTPUT: probability of 7% to detect an effect in case it does exist
recapturecolor_portino_s2 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 3014 snails would be required to achieve statistically valid inferences (80%)

# scar ~ color (pooled)
scarcolor_portino <- data_portino %>% 
  filter(recapture == "recaptured") %>% 
  group_by(color) %>% 
  summarise(scarred = sum(scar == "1"),
            non_scarred = sum(scar == "0")) %>% 
  {matrix(c(.$scarred, .$non_scarred), nrow = 2, byrow = TRUE)} # Contingency table
scarcolor_portino %>% GTest_Power # G Test
# OUTPUT: probability of 16% to detect an effect in case it does exist
scarcolor_portino %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 614 snails would be required to achieve statistically valid inferences (80%)

# scar ~ color (session 1)
scarcolor_portino_s1 <- data_portino %>% 
  filter(recapture == "recaptured",
         session == "1") %>% 
  group_by(color) %>% 
  summarise(scarred = sum(scar == "1"),
            non_scarred = sum(scar == "0")) %>% 
  {matrix(c(.$scarred, .$non_scarred), nrow = 2, byrow = TRUE)} # Contingency table
scarcolor_portino_s1 %>% GTest_Power # G Test
# OUTPUT: probability of 7% to detect an effect in case it does exist
scarcolor_portino_s1 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 1784 snails would be required to achieve statistically valid inferences (80%)

# scar ~ color (session 2)
scarcolor_portino_s2 <- data_portino %>% 
  filter(recapture == "recaptured",
         session == "2") %>% 
  group_by(color) %>% 
  summarise(scarred = sum(scar == "1"),
            non_scarred = sum(scar == "0")) %>% 
  {matrix(c(.$scarred, .$non_scarred), nrow = 2, byrow = TRUE)} # Contingency table
scarcolor_portino_s2 %>% GTest_Power # G Test
# OUTPUT: probability of 19% to detect an effect in case it does exist
scarcolor_portino_s2 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 215 snails would be required to achieve statistically valid inferences (80%)

# habitat ~ color (pooled)
habitatcolor_portino <- data_portino %>% 
  filter(recapture == "recaptured") %>% 
  group_by(color) %>% 
  summarise(balanus = sum(habitat == "balanus"),
            lichina = sum(habitat == "lichina"),
            bare_rock = sum(habitat == "bare rock")) %>% 
  {matrix(c(.$balanus, .$lichina, .$bare_rock), nrow = 3, byrow = TRUE)}
habitatcolor_portino %>% GTest_Power # G Test
# OUTPUT: probability of 7% to detect an effect in case it does exist
habitatcolor_portino %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 3246 snails would be required to achieve statistically valid inferences (80%)

# habitat ~ color (session 1)
habitatcolor_portino_s1 <- data_portino %>% 
  filter(recapture == "recaptured",
         session == "1") %>% 
  group_by(color) %>% 
  summarise(balanus = sum(habitat == "balanus"),
            lichina = sum(habitat == "lichina"),
            bare_rock = sum(habitat == "bare rock")) %>% 
  {matrix(c(.$balanus, .$lichina, .$bare_rock), nrow = 3, byrow = TRUE)}
habitatcolor_portino_s1 %>% GTest_Power # G Test
# OUTPUT: probability of 5% to detect an effect in case it does exist
habitatcolor_portino_s1 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 7884 snails would be required to achieve statistically valid inferences (80%)

# habitat ~ color (session 2)
habitatcolor_portino_s2 <- data_portino %>% 
  filter(recapture == "recaptured",
         session == "2") %>% 
  group_by(color) %>% 
  summarise(balanus = sum(habitat == "balanus"),
            bare_rock = sum(habitat == "bare rock")) %>% 
  {matrix(c(.$balanus, .$bare_rock), nrow = 2, byrow = TRUE)}
habitatcolor_portino_s2 %>% GTest_Power # G Test
# OUTPUT: probability of 11% to detect an effect in case it does exist
habitatcolor_portino_s2 %>% GTest_Power_n # sample size needed for 80% power
# OUTPUT: n > 532 snails would be required to achieve statistically valid inferences (80%)


# Distance for Experiment 2 (Portino, 2024) -------------------------------
library(car) # Non-Constant Variance Test

# Pooled sessions
lm_portino <- lm(snail_distance ~ habitat + color + scale(size) + scar + direction, 
                 data = data_portino %>% filter(recapture == "recaptured"))
shapiro.test(residuals(lm_portino)); ncvTest(lm_portino)
lm_portino %>% step %>% summary

# Session 1
lm_portino_s1 <- lm(snail_distance ~ habitat + color + scale(size) + scar + direction, 
                    data = data_portino %>% filter(recapture == "recaptured",
                                                   session == "1"))
shapiro.test(residuals(lm_portino_s1)); ncvTest(lm_portino_s1)
lm_portino_s1 %>% step %>% summary

lm_portino_s1 <- lm(snail_distance ~ habitat + color + scale(size) + scar + direction, 
                    data = data_portino %>% filter(recapture == "recaptured",
                                                   session == "1"))
shapiro.test(residuals(lm_portino_s1)); ncvTest(lm_portino_s1)
lm_portino_s1 %>% step %>% summary

glm_portino_s1_def <- glm(snail_distance ~ habitat + scar,
                          data = data_portino %>% filter(recapture == "recaptured",
                                                         session == "1"))
# Power Analysis for whether a snail changed its habitat
glm_portino_s1_def %>% powerSim(., fixed("habitat"), 
                                seed = 123,
                                nsim = 1000)
# OUTPUT: 70.9% of power with our actual sample size (n = 42)

# Session 2
lm_portino_s2 <- lm(snail_distance ~ habitat + color + scale(size) + scar + direction, 
                    data = data_portino %>% filter(recapture == "recaptured",
                                                   session == "2"))
shapiro.test(residuals(lm_portino_s2)); ncvTest(lm_portino_s2)
lm_portino_s2 %>% step %>% summary


