hardness_eq <- function(hardness, E_A, E_B, CF_A, CF_B, CF_C){
  if (is.na(CF_A) & is.na(CF_B)){
    CF2 <- CF_C
  } else if (!is.na(CF_A) & !is.na(CF_B)){
    CF2 <- CF_A - (log(hardness) * CF_B)
  }
  result <- exp(E_A * log(hardness) + E_B) * CF2
  
  return(result)
}