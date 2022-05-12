#' calculate_big5
#' Calculate the big5 from the personality trait scale calculated with AMBI::calculate_AMBI()
#' @param df a dataframe with the data issued by AMBI::calculate_AMBI()
#' @param calculation_method must be in c('normalisation', 'scaling','semscoring') (default 'normalisation'). semscoring generate a sem using the model of construction of the big five and then use the estimate to calculate the latent variable (the big 5).
#' @param semscoring_method if calculation method use the semscoring methods, then it is possible to normaliza the result or scale them. Calculation method/semscoring method must be in c('', scale','normalisation'), (default 'normalisation').
#'
#' @return if calculation_method in c('normalisation', 'scaling'), then a dataframe with the big5. If calculation_method is 'semscoring', then a list with the SEM and a dataframe with the big5.
#' @import lavaan
#' @import tidyverse
#' @export
calculate_big5 <- function(df,
                           calculation_method='normalisation',
                           semscoring_method='normalisation'){
  if(!(calculation_method %in% c('normalisation', 'scaling','semscoring'))) stop("calculation_method must be in c(normalisation, 'scaling','semscoring')")
  if(calculation_method=='scaling'){
    df$AMBI_big5_Neuroticism <- (df$AMBI_MSR_1_NEOPIR_ANXIETY + df$AMBI_MSR_2_NEOPIR_ANGRYHOSTILITY + df$AMBI_MSR_3_NEOPIR_DEPRESSION + df$AMBI_MSR_4_NEOPIR_SELFCONSCIOUSNESS + df$AMBI_MSR_5_NEOPIR_IMPULSIVENESS + df$AMBI_MSR_6_NEOPIR_VULNERABILITY) %>% scale()
    df$AMBI_big5_Extraversion <- (df$AMBI_MSR_7_NEOPIR_WARMTH + df$AMBI_MSR_8_NEOPIR_GREGARIOUSNESS + df$AMBI_MSR_9_NEOPIR_ASSERTIVENESS + df$AMBI_MSR_10_NEOPIR_ACTIVITY + df$AMBI_MSR_11_NEOPIR_EXCITEMENTSEEKING + df$AMBI_MSR_12_NEOPIR_POSITIVEEMOTIONS) %>% scale()
    df$AMBI_big5_Openness <- (df$AMBI_MSR_13_NEOPIR_FANTASY + df$AMBI_MSR_14_NEOPIR_AESTHETICS + df$AMBI_MSR_15_NEOPIR_FEELINGS + df$AMBI_MSR_16_NEOPIR_ACTIONS + df$AMBI_MSR_17_NEOPIR_IDEAS + df$AMBI_MSR_18_NEOPIR_VALUES) %>% scale()
    df$AMBI_big5_Agreeableness <- (df$AMBI_MSR_19_NEOPIR_TRUST+df$AMBI_MSR_20_NEOPIR_STRAIGHTFORWARDNESS+df$AMBI_MSR_21_NEOPIR_ALTRUISM+df$AMBI_MSR_22_NEOPIR_COMPLIANCE+df$AMBI_MSR_23_NEOPIR_MODESTY+df$AMBI_MSR_24_NEOPIR_TENDERMINDEDNESS) %>% scale()
    df$AMBI_big5_Conscientiousness <- (df$AMBI_MSR_25_NEOPIR_COMPETENCE+df$AMBI_MSR_26_NEOPIR_ORDER+df$AMBI_MSR_27_NEOPIR_DUTIFULNESS+df$AMBI_MSR_28_NEOPIR_ACHIEVEMENTSTRIVING+df$AMBI_MSR_29_NEOPIR_SELFDISCIPLINE+df$AMBI_MSR_30_NEOPIR_DELIBERATION) %>% scale()
    res <- df
  }else if(calculation_method=='normalisation'){
    df$AMBI_big5_Neuroticism <- (df$AMBI_MSR_1_NEOPIR_ANXIETY + df$AMBI_MSR_2_NEOPIR_ANGRYHOSTILITY + df$AMBI_MSR_3_NEOPIR_DEPRESSION + df$AMBI_MSR_4_NEOPIR_SELFCONSCIOUSNESS + df$AMBI_MSR_5_NEOPIR_IMPULSIVENESS + df$AMBI_MSR_6_NEOPIR_VULNERABILITY) %>% f_normalisation()
    df$AMBI_big5_Extraversion <- (df$AMBI_MSR_7_NEOPIR_WARMTH + df$AMBI_MSR_8_NEOPIR_GREGARIOUSNESS + df$AMBI_MSR_9_NEOPIR_ASSERTIVENESS + df$AMBI_MSR_10_NEOPIR_ACTIVITY + df$AMBI_MSR_11_NEOPIR_EXCITEMENTSEEKING + df$AMBI_MSR_12_NEOPIR_POSITIVEEMOTIONS) %>% f_normalisation()
    df$AMBI_big5_Openness <- (df$AMBI_MSR_13_NEOPIR_FANTASY + df$AMBI_MSR_14_NEOPIR_AESTHETICS + df$AMBI_MSR_15_NEOPIR_FEELINGS + df$AMBI_MSR_16_NEOPIR_ACTIONS + df$AMBI_MSR_17_NEOPIR_IDEAS + df$AMBI_MSR_18_NEOPIR_VALUES) %>% f_normalisation()
    df$AMBI_big5_Agreeableness <- (df$AMBI_MSR_19_NEOPIR_TRUST+df$AMBI_MSR_20_NEOPIR_STRAIGHTFORWARDNESS+df$AMBI_MSR_21_NEOPIR_ALTRUISM+df$AMBI_MSR_22_NEOPIR_COMPLIANCE+df$AMBI_MSR_23_NEOPIR_MODESTY+df$AMBI_MSR_24_NEOPIR_TENDERMINDEDNESS) %>% f_normalisation()
    df$AMBI_big5_Conscientiousness <- (df$AMBI_MSR_25_NEOPIR_COMPETENCE+df$AMBI_MSR_26_NEOPIR_ORDER+df$AMBI_MSR_27_NEOPIR_DUTIFULNESS+df$AMBI_MSR_28_NEOPIR_ACHIEVEMENTSTRIVING+df$AMBI_MSR_29_NEOPIR_SELFDISCIPLINE+df$AMBI_MSR_30_NEOPIR_DELIBERATION) %>% f_normalisation()
    res <- df
  }else if(calculation_method=="semscoring"){
    model <- '
Neuroticism =~ AMBI_MSR_1_NEOPIR_ANXIETY + AMBI_MSR_2_NEOPIR_ANGRYHOSTILITY + AMBI_MSR_3_NEOPIR_DEPRESSION + AMBI_MSR_4_NEOPIR_SELFCONSCIOUSNESS + AMBI_MSR_5_NEOPIR_IMPULSIVENESS + AMBI_MSR_6_NEOPIR_VULNERABILITY
Extraversion =~ AMBI_MSR_7_NEOPIR_WARMTH + AMBI_MSR_8_NEOPIR_GREGARIOUSNESS + AMBI_MSR_9_NEOPIR_ASSERTIVENESS + AMBI_MSR_10_NEOPIR_ACTIVITY + AMBI_MSR_11_NEOPIR_EXCITEMENTSEEKING + AMBI_MSR_12_NEOPIR_POSITIVEEMOTIONS
Openness =~ AMBI_MSR_13_NEOPIR_FANTASY + AMBI_MSR_14_NEOPIR_AESTHETICS + AMBI_MSR_15_NEOPIR_FEELINGS + AMBI_MSR_16_NEOPIR_ACTIONS + AMBI_MSR_17_NEOPIR_IDEAS + AMBI_MSR_18_NEOPIR_VALUES
Agreeableness =~ AMBI_MSR_19_NEOPIR_TRUST+AMBI_MSR_20_NEOPIR_STRAIGHTFORWARDNESS+AMBI_MSR_21_NEOPIR_ALTRUISM+AMBI_MSR_22_NEOPIR_COMPLIANCE+AMBI_MSR_23_NEOPIR_MODESTY+AMBI_MSR_24_NEOPIR_TENDERMINDEDNESS
Conscientiousness =~ AMBI_MSR_25_NEOPIR_COMPETENCE+AMBI_MSR_26_NEOPIR_ORDER+AMBI_MSR_27_NEOPIR_DUTIFULNESS+AMBI_MSR_28_NEOPIR_ACHIEVEMENTSTRIVING+AMBI_MSR_29_NEOPIR_SELFDISCIPLINE+AMBI_MSR_30_NEOPIR_DELIBERATION
'
    fit <- cfa(model, data = df, estimator = "ML")
    df_score_big5 <- semscoring_latent(fit, df, calculation=semscoring_method)
    colnames(df_score_big5) <- paste0("AMBI_BIG5_", colnames(df_score_big5))
    df <- cbind(df, df_score_big5)

    res <- list(dataframe=df, SEM=fit)
  }
  return(res)
}
