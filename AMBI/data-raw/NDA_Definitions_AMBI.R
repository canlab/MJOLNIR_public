library(tidyverse)

df_NDA <- read.csv('./data-raw/NDA definitions_ambi-02.csv')
df_NDA <- df_NDA[df_NDA$ElementName %>% str_detect('^ambi_') %>% which,]

df_ambi_namescoringkeys <- read.csv2('./data-raw/AMBI_NAME_SCORINGKEYS.csv')

df_example_aliases <- read.csv('./data-raw/df_example_aliases.csv')

df_example_elementname <- read.csv('./data-raw/df_examples_elementname.csv')

v_AMBI_recode_test <- sample(c(1:5,-99,88,77),30, replace=T)

## code to prepare `NDA_Definitions_AMBI` dataset goes here

usethis::use_data(df_NDA, df_ambi_namescoringkeys,df_example_elementname,df_example_aliases, v_AMBI_recode_test, overwrite = TRUE)
