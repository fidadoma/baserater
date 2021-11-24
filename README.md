# baserater
Purpose of this package is for computing base-rates. When using test battery, typical approach is to compute scores for each subtest and compare this value with some cutoff (usually defined as x-times SD below mean, where x is some multiplier). However, with more subtests, chance to obtain low score increses. Base-rate approach computes distribution of low scores and determines cutoffs based on the sum of low scores. this package is used to simplify these computations. 

# Installation

First, this package needs to be installed using `remotes` package

```{r}
install.packages("remotes")
remotes::install_github("fidadoma/baserater")
```

Now we can load the package to start the analysis.


# Usage

`baserater` package is supplied with UDS 2.0 test battery measured on three groups:
* healthy controls - n = 481 - sample of healthy participants, that will be used for baserate calculations
* patients - n = 67 - sample of patients with diagnosed MCI (mild cognitive disorder)
* validation sample - n = 62 - another sample of healthy participants used for validation

UDS 2.0 battery consists of 10 subtests, which produce 14 scales. We will use all 14 scales for computation of baserates

First, we load the library (including tidyverse for easier data manipulation)

```{r}
library(tidyverse)
library(baserater)
```

Then we need to select the variables used for base-rate calculation. 

```{r}
v <- quo(MMSE_orientation:BNT) %>% expand_varsselect(controls,.)
```

We are using tidyselect syntax with range, because columns are next to each other, however, we could specify all values manually. Following code produces the same result as previous line

```{r}
v <- c("MMSE_orientation","MMSE_total","LM_immed","LM_delayed","DS_forward_total","DS_forward_ls","DS_backward_total","DS_backward_ls","CF_animals","CF_vegetables","TMT_A", "TMT_B", "WAIS","BNT")
```

Base-rates are computed for each subgroup individually (e.g. for each age group or/and education category). We need to select variables, which will be used to create subgroups. Here we divide the data using age groups (75< and 76+) and education categories (lower - <12 years and higher - >12 years)

```{r}
sett <- create_settings(quo(age_cat),quo(edu_cat))
```

Load data supplied with the package

```{r}
controls <- data(controls)
patients <- data(patients)
```

Set SD that will be used for determining what will be treated as low-score. Here, we will use three different SDs 
```{r}
sdi <- c(2,1.5,1)
```

Finally, we can classify group based on the base-rates from the healthy sample. This classifies data into possible MCI (number of low scores that would lead to 0.8 specificity on healthy sample) and probable MCI (number of low scores that would lead to 0.9 specificity on healthy sample)

```{r}
pat_classified <- classify_group(patients, v, controls, sdi, sett = sett)
val_classified <- classify_group(validation, v, controls, sdi, sett = sett)
```

Finally, we can report some descriptive statistics for each SD
```{r}

pat_classified %>%
  group_by(sdi) %>%
  summarize(m_possible = mean(possibleSexMeditation) * 100,
            sd_possible = sd(possibleSexMeditation) * 100,
            m_probable = mean(probableSexMeditation) * 100,
            sd_probable = sd(probableSexMeditation) * 100) %>% 
            knitr::kable(caption = "Patients sample", digits = 2)

val_classified %>%
  group_by(sdi) %>%
  summarize(m_possible = mean(possibleSexMeditation) * 100,
            sd_possible = sd(possibleSexMeditation) * 100,
            m_probable = mean(probableSexMeditation) * 100,
            sd_probable = sd(probableSexMeditation) * 100) %>% 
            knitr::kable(caption = "Validation sample", digits = 2)
```
