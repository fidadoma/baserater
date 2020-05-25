# baserater
R package for computing baserates. 

# Installation

Clone the repository and install it locally.

# Usage

Following code is used for computing baserates.

```{r}
library(tidyverse)
library(baserater)

v <- c("Extraversion","Empathy.Agreeableness","Conscientiousness","Instability.Neuroticism","Openness_to_Experience.Intellect","Honesty.Humility",
       "Disinhibition", "Detachment", "Psychoticism","Negative_Affect","Antagonism")

df <- neuropsychology::personality %>% as_tibble()

df<- df %>%
  mutate_at(v, ~(-.))
sett <- create_settings(quo(Sex),quo(Meditation))

df_controls <- df %>% filter(Mood_Disorder == "Absence")
df_patients <- df %>% filter(Mood_Disorder == "Presence")


sdi <- c(2,1.5,1)

pat_classified <- classify_group(df_patients,v,df_controls,sdi,sett = sett)
con_classified <- classify_group(df_controls,v,df_controls,sdi,sett = sett)


pat_classified %>%
  group_by(sdi) %>%
  summarize(m_possible = mean(possibleSexMeditation) * 100,
            sd_possible = sd(possibleSexMeditation) * 100,
            m_probable = mean(probableSexMeditation) * 100,
            sd_probable = sd(probableSexMeditation) * 100) %>% 
            knitr::kable()

con_classified %>%
  group_by(sdi) %>%
  summarize(m_possible = mean(possibleSexMeditation) * 100,
            sd_possible = sd(possibleSexMeditation) * 100,
            m_probable = mean(probableSexMeditation) * 100,
            sd_probable = sd(probableSexMeditation) * 100) %>% 
            knitr::kable()
```
