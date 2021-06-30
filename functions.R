# In this function belong all modifications of the text
normalize_text <- function(x){
  x %>%
    str_squish() %>%
    str_replace_all("\r", "") %>%
    str_replace_all("\\n", " ") %>%
    str_replace_all("- (?!und)", "") %>%
    str_replace_all("  ", " ")
}

# v <- "Bauablauf und Witterung bedingte Ab-, Zwischen- und Wiederantransporte oder Wartezeiten vorgenannter"


# Paste for character variables: "Angebot", "Einheit"
# The "if" statement is required because it was observed that not only Angebotsposotionen have "NA" in the "PositionsNr" column, but also some without Angebot.
paste_chr <- function(x, vectorr, list){
  temp <- str_c(na.omit(vectorr[list[[x]]]), collapse = " ")
  # if (identical(temp,character(0))) {
  if (identical(temp,"")) {
    temp <- NA
  } else {
    temp <- temp
  }
  return(temp)
}

# Paste for numeric variables: "LV_menge", "Budgetierter_Einheitspreis", "Betrag"
paste_num <- function(x, vectorr, list){
  temp <- parse_number(str_c(na.omit(vectorr[list[[x]]]), collapse = " "))
  # if (identical(temp,numeric(0))) {
  #   temp <- NA
  #   } else {
  #     temp <- temp
  #     }
  return(temp)
}

# Split each of the existing groups into two sublists
# The first one should be concatenated and removed
# The second should be expanded with information from the first
list_grouping <- function(list_element, boundary, x){
  list(first = list_element[[x]][1]:boundary[x],
       second = seq(boundary[x]+1, tail(list_element[[x]], n=1)))
}


# Really trivial one, used only for Bezeichnung. Not an optimized solution
paste_to_previous_grouped <- function(char_vector, list, x){
  temp <- str_c(na.omit(char_vector[list[[x]]$first]), collapse = " ")
  if (identical(temp,character(0))) {
    temp <- NA
  }
  return(temp)
}























