#' calculate_AMBI This function aim to calculate measure for 200 personality scales with 200 items
#'
#' @param df your data containing the answer of participants to 200 items, and gives in result a dataframe with measure of 200 personality scales. The data frame put in entry has either to have columns' names named like AMBI::df_NDA[,'ElementName'] or AMBI::df_NDA[,'Aliases'] or have the columns in the same order as AMBI::df_example_elementname. A dataframe example as been put under the name of AMBI::df_example_elementname and of AMBI::df_example_aliases, if you use the aliases as variable names.
#' @param var_name a character string that should be in c('element_name','aliases') or NULL (by default 'element_name'); element_name: indicate that the variable name of your item are in AMBI::df_NDA[,'ElementName']; aliases: indicate that the variable name of your item are in AMBI::df_NDA[,'Aliases']; NULL: indicate that the variable indexes of your item  are in the same order than the row in AMBI::df_NDA
#' @param recodage a function on how to recode your variable if necessary (by default AMBI::recode_AMBI()).
#' @param calculation_function a character string in c('normalisation_0-1', 'scale') or a function on how the calculation of the AMBI should be done (by default 'normalisation_0-1'). 'normalisation_0-1' makes the sum of the items of a personality trait (reverse item are reversed before) and then normalize the result between 0 and 1. 'scale' makes the sum of the items of a personality trait (reverse item are reversed before) and then scale them. These functions suppose that your data is coding between 1 and 5 for each items. If you want to add your own function, it must use and have 'df_pos' (dataframe with the column of the items positively correlated with AMBI) et 'df_rev' (dataframe with the column of the items negatively correlated with AMBI) arguments.
#'
#' @return a dataframe combining your dataframe put in entry and the calculated measure of AMBI personality scale
#' @import tidyverse
#' @references T. Yarkoni, "The abbreviation of personality, or how to measure 200 personality scales with 200 items", *Journal of Research in Personality* 44 (2010) 180–198, doi:10.1016/j.jrp.2010.01.002
#'
#'@example
#'calculate_AMBI(AMBI::df_example_elementname)
#'calculate_AMBI(AMBI::df_example_aliases, var_name = "aliases", calculation_function = "scale")
#' @author Léo HENRY
#' @export
calculate_AMBI <- function(df,
                           var_name="element_name",
                           recodage=function(v){AMBI::recode_AMBI(v)},
                           calculation_function="normalization_0-1"){
  if(!(var_name %in% c('element_name','aliases')|is.null(var_name))){
    stop("var_name should be in c('element_name','aliases') or NULL (by default 'element_name)
            - element_name: indicate that the variable name of your item are in AMBI::df_NDA[,'ElementName']
            - aliases: indicate that the variable name of your item are in AMBI::df_NDA[,'Aliases']
            - NULL: indicate that the variable indexes of your item  are in the same order than the row in AMBI::df_NDA")
  }else{
    # We detect here the indexes of the column cointaining the answers to the items
    v_AMBI_colid <- c(1:ncol(df))
    if(!is.null(var_name)){
      df <- df %>%
        rename_with(str_to_upper)
      if(var_name=="element_name"){
        v_AMBI_colid <- (colnames(df) %in% (AMBI::df_NDA$ElementName %>% str_to_upper)) %>% which()
        if(ncol(df[,v_AMBI_colid])!=(AMBI::df_NDA$ElementName %>% length)) stop("There are missing items and variables in your data frame, or the names are different from AMBI::df_NDA$ElementName")
      }else if(var_name=="aliases"){
        v_AMBI_colid <- (colnames(df) %in% (AMBI::df_NDA$Aliases %>% str_to_upper)) %>% which()
        if(ncol(df[,v_AMBI_colid])!=(AMBI::df_NDA$Aliases %>% length)) stop("There are missing items and variables in your data frame, or the names are different from AMBI::df_NDA$Aliases")
      }
    }
  }
  # We apply on the identified columns the recodage function
  df_ambi_clean <- map(df[,v_AMBI_colid],
                       function(x){recodage(x)}) %>% as.data.frame

  # Here, we need to rename the variable so that we can identify them relatively to the scoring keys in AMBI::df_ambi_namescoringkeys$Scoring.key
  if(is.null(var_name)){
    if(ncol(df_ambi_clean)!=nrow(AMBI::df_NDA)) stop("if you choose var_name==NULL, then the number of column of your data frame must be equal to the number of row of AMBI::df_NDA")
    colnames(df_ambi_clean) <- paste0('AMBI_',c(1:(AMBI::df_NDA$Aliases %>% length)))
  }else if(var_name=="aliases"){
    colnames(df_ambi_clean) <- map(colnames(df_ambi_clean),
                                   function(x){
                                     AMBI::df_NDA$ElementName[str_detect((AMBI::df_NDA$Aliases %>% str_to_upper),paste0("^", x, "$")) %>% which()] %>%
                                       str_to_upper()
                                   }) %>%
      as_vector()
  }

  df <- df_ambi_clean

  # Now we make the calculation of the AMBI according to the wanted function

  df_ambi_namescoringkeys <- AMBI::df_ambi_namescoringkeys

  l_keys_reverse <- df_ambi_namescoringkeys$Scoring.key %>%
    str_extract_all('[0-9]{1,3}R') %>%
    lapply(function(x){str_remove_all(x,'R')})

  l_keys_positive <- df_ambi_namescoringkeys$Scoring.key %>%
    str_remove_all('[0-9]{1,3}R') %>%
    str_extract_all('[0-9]{1,3}')


  if(class(calculation_function)=="function"){
    if(!str_detect(args(calculation_function) %>% as.list() %>% names() %>% paste0(collapse = ""), "(?=.*df_pos)(?=.*df_rev).*")) stop("your function in 'calculation_function' must have 'df_pos' (dataframe with the column of the items positively correlated with AMBI) et 'df_rev' (dataframe with the column of the items negatively correlated with AMBI) arguments.")
    f_measure_calculation <- function(v_pos, v_rev){
      df_pos <- df[,c(paste0("AMBI_",v_pos),"NUL")]
      df_rev <- df[,c(paste0("AMBI_",v_rev),"NUL")]
      r <- calculation_function(df_pos=df_pos, df_rev=df_rev)
      return(r)
      }
  }else{
    if(!(calculation_function %in% c("normalization_0-1", "scale"))) stop("calculation_function must be in c('normalization_0-1', 'scale'), or be a function with 'df_pos' (dataframe with the column of the items positively correlated with AMBI) et 'df_rev' (dataframe with the column of the items negatively correlated with AMBI) arguments.")
    if(calculation_function=="normalization_0-1")   f_measure_calculation <- function(v_pos, v_rev){AMBI_measurecalculation_normalisation(df, v_pos, v_rev)}
    if(calculation_function=="scale")   f_measure_calculation <- function(v_pos, v_rev){AMBI_measurecalculation_scale(df, v_pos, v_rev)}
  }

  df_ambi_msr <- map2(l_keys_positive, l_keys_reverse,function(x,y){f_measure_calculation(v_pos = x, v_rev = y)}) %>%
    as.data.frame

  colnames(df_ambi_msr) <- paste("AMBI_MSR",
                                 df_ambi_namescoringkeys$Scale.no.,
                                 df_ambi_namescoringkeys$Inventory %>% str_replace_all("[ -]","") %>% str_to_upper,
                                 df_ambi_namescoringkeys$Scale.name %>% str_replace_all("[ -]","") %>% str_to_upper,
                                 sep='_')
  dfr <- cbind(df, df_ambi_msr)

  return(dfr)
}

