list.of.packages <- c("readr", "tidyverse", "readxl", "reshape2", "openxlsx")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, library, character.only = TRUE)

# Give the name of the .xlsx file with the LV (Leistungsverzeichnis) of the project:
name_LV_xlsx <- "Rüthi_ARGE"
path <- paste(name_LV_xlsx,".xlsx", sep = "")
sheet_names <- readxl::excel_sheets(path)
# Remove first worksheet, as it is the summary
# Kapitel 102 and 103 are also not in the prices
sheet_names <- sheet_names[-c(1:2)]
# Get the number of each Kapitel in a string vector:
kapitel_nr <- as.numeric(str_sub(sheet_names, 12, 14))
# "skip" omits the undesired column
# Warnings are expected!
# Read warnings and tell where there are inconsistencies with the column types
# For example, "Expecting numeric in G400 / R400C7: got 'per'" indicates that
# in one excel sheet in cell "G400" (LV_Menge column) there is the string "per" instead
# a numeric type. This string is correct omitted (not the entire column) - replaced with "NA"
Raw_data <- lapply(sheet_names, readxl::read_xlsx, path = path,
                   col_names = c("Regie", "Position_Nr", "Unterposition_Nr",
                                 "ToDrop1", "Bezeichnung", "Angebot",
                                 "LV_Menge", "Einheit", "ToDrop2",
                                 "Budgetierter_Einheitspreis", "Betrag"),
                   col_types = c("text", "numeric", "numeric",
                                 "skip", "text", "text",
                                 "numeric", "text", "skip",
                                 "numeric", "numeric"))
# i <- 2
# sheet_names[i]
# kapitel_nr[i]
# test <- Raw_data[[i]]
# 
# LV_tibble <- test
# kptl_nr <- kapitel_nr[i]
# 
# # Source functions:
source("functions.R")

# Function takes a tibble as input (LV_tibble) and modifies it
manipulate_data <- function(LV_tibble, kptl_nr){
  # Remove empty rows and rows with no "Bezeichnung"
  LVsheet <- LV_tibble[apply(LV_tibble,1,function(x)any(!is.na(x))),]
  LVsheet <- LVsheet %>%
    filter(!is.na(Bezeichnung))
  # Remove last summary row and keep Name for later use:
  # [[:digit:]] could also be written as [:digit:], but is not really correct
  kap_name <- str_extract(LVsheet[nrow(LVsheet),"Bezeichnung"], '(?<=[[:digit:]]{3} ).*')
  LVsheet <- LVsheet[-nrow(LVsheet),]
  # Normalize text in "Bezeichnung" cells
  # Create new column for "Gliederung"
  LVsheet <- LVsheet %>%
    mutate_at(vars(Bezeichnung), normalize_text) %>%
    mutate(Gliederung=NA)
  
  # All rows with text but NA in Position_Nr column, should go with previous!
  # Difficult task, optimization may be required, but solution is not bad
  # Find row number of observations with NA in Position_Nr
  indices <- which(is.na(LVsheet$Position_Nr))
  # Group consecutive NAs so as to be attached to previous non-NA
  group <- cumsum(c(1, abs(indices[-length(indices)] - indices[-1]) > 1))
  # Wonderful function, which shows the results:
  group_list <- by(indices, group, identity)
  
  # Paste all consecutive NA-in-Position_Nr-variables to the last non-NA value:
  # Do this for every group:
  # Get first elements of list:
  # Base R: lapply(list_name, `[[`, 1)
  # tidyverse: as.numeric(map(group_list, 1))
  indices_to_copy_on <- map_int(group_list, 1)-1L
  
  # Append to "group_list" to have the whole group:
  group_list <- mapply(append, indices_to_copy_on, group_list, SIMPLIFY = FALSE)
  # FALSE: lapply(group_list, append, indices_to_copy_on)
  # ALTERNATIVE but maybe slower:
  # sapply(seq_along(indices_to_copy_on), function(x)c(indices_to_copy_on[x],group_list[[x]]))

  # In each of these groups that are created, the text should stop there, where the first non-empty "Angebot" is.
  # Everything below is "Gliederung"
  # This function MAY PRODUCE NAs for the Positions without any "Angebot" (keine Ausschreibungsposition)
  splitted_until_this <- map_int(group_list,~ .[which(!is.na(LVsheet$Angebot[.]))][1])
  # Previous solution was far less elegant than the proposed above:
  # sapply(seq_along(group_list), function(x) group_list[[x]][which(!is.na(LVsheet$Angebot[group_list[[x]]]))][1])
  
  # Do this only for these rows that have NAs in Pos_Nr & Unterpos_Nr & Angebot:
  na_angebot_list <- group_list[is.na(splitted_until_this)]
  na_angebot_first <- map_int(na_angebot_list,1)
  LVsheet$Bezeichnung[na_angebot_first] <- map_chr(na_angebot_list,
                                                   function(x) str_c(na.omit(LVsheet$Bezeichnung[x]), collapse = " "))
  group_list <- group_list[!is.na(splitted_until_this)]
  splitted_until_this <- splitted_until_this[!is.na(splitted_until_this)]
  
  ######## Unfortunate attempt to work with data.frame. Did not work########
  # pivot_df1 <- reshape2::melt(group_list)
  # pivot_df1 <- pivot_df1 %>%
  #   mutate(splitted_until = rep(splitted_until_this,
  #                               tapply(value, L1, length))) %>%
  #   group_by(L1) %>%
  #   mutate(secondary = as.factor(if_else(value<=splitted_until, "first", "second")))
  # 
  # # It is not working this way.
  # # melt(group_list) %>%
  # #   group_by(L1) %>%
  # #   mutate(Secondary = if_else(splitted_until_this[j]<=value, "first", "second"))
  # LVsheet$Bezeichnung[unlist(pivot_df1 %>%
  #           filter(secondary == "first") %>%
  #           ungroup() %>%
  #           select(value), use.names = FALSE)]
  # pivot_df1 %>%
  #   filter(secondary == "second") %>%
  #   group_by(L1) %>%
  #   group_map(~ str_c(na.omit(.x, collapse = " ")))
  # 
  # map(split(pivot_df1, pivot_df1$L1),
  #     ~ str_c(na.omit(LVsheet$Bezeichnung[unlist(split(pivot_df1, pivot_df1$L1)[[2]] %>% 
  #                                           filter(secondary == "first") %>% 
  #                                           ungroup() %>% 
  #                                           select(value))]),
  #                     collapse = " "))
  # 
  # split(pivot_df1, pivot_df1$L1)[[5]]
# ######################################################
  
  mod_grouped_list <- lapply(seq_along(group_list),
         list_grouping,
         list_element = group_list,
         boundary = splitted_until_this)
  pivot_df <- melt(mod_grouped_list)
  
  appended_bez <- sapply(seq_along(mod_grouped_list), paste_to_previous_grouped,
                         char_vector = LVsheet$Bezeichnung,
                         list = mod_grouped_list)
  LVsheet$Gliederung[pivot_df$value[pivot_df$L2=="second"]] <- LVsheet$Bezeichnung[pivot_df$value[pivot_df$L2=="second"]]
  
    # Have not understood how to do it with sapply/lapply...
  for(j in seq_along(appended_bez)){
    # Copy Bezeichnung to Gliederung rows
    LVsheet$Bezeichnung[mod_grouped_list[[j]]$second] <- appended_bez[j]
    # Copy Regie to Gliederung rows
    LVsheet$Regie[mod_grouped_list[[j]]$second] <- LVsheet$Regie[mod_grouped_list[[j]]$first[1]]
    # Copy Position_Nr to Gliederung rows
    LVsheet$Position_Nr[mod_grouped_list[[j]]$second] <- LVsheet$Position_Nr[mod_grouped_list[[j]]$first[1]]
    # Copy Unterposition_Nr to Gliederung rows
    LVsheet$Unterposition_Nr[mod_grouped_list[[j]]$second] <- LVsheet$Unterposition_Nr[mod_grouped_list[[j]]$first[1]]
  }
  
  # Drop rows with NA in Position_Nr column
  # Replace NAs in Unterposition_Nr with zeros
  LVsheet <- LVsheet %>%
    slice(-pivot_df$value[pivot_df$L2 == "first"]) %>% 
    filter(!is.na(Position_Nr)) %>%
    mutate_at(vars(Unterposition_Nr), ~replace(., is.na(.), 0)) %>%
    mutate(Kapitel=kptl_nr, Name=kap_name) %>%
    mutate(Position=paste(sprintf("%03d", Kapitel), ".", sprintf("%03d", Position_Nr), ".",
                          sprintf("%03d", Unterposition_Nr), sep = "")) %>%
    select(Regie, Name, Kapitel, Position_Nr, Unterposition_Nr, Position,
           Bezeichnung, Angebot, Gliederung, everything())
  return(LVsheet)
}

# Quick check:
# i <- 10
# var1 <- Raw_data[[i]]
# var2 <- kapitel_nr[i]
# temp <- manipulate_data(var1,var2)
# as.data.frame(temp[37,6])


condensed_data <- map2(Raw_data, kapitel_nr, manipulate_data)
# It makes sense that the function may produce warnings.
# Here for example, this is because in two LV positionen (117.362.004 and 267.231.402)
# there are two prices. That means the excel file is not properly generated
# (inherently flawed)
# You can do nothing about it

condensed_data <- bind_rows(condensed_data)
# condensed_data <- bind_rows(condensed_data, .id = "column_label")
write.xlsx(condensed_data, file = paste0(c(name_LV_xlsx, "mod.xlsx"), collapse = "_"), asTable = TRUE)










































































































