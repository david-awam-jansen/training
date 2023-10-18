## Code to open the datasheets of the .... project
## Data collected by Zack and others
packages <- c("tidyverse"
              , "cellranger"
              , "data.table"
              , "here"
              , "janitor"
              , "purrr"
              , "readxl"
              , "tidyverse"
              )

## install packages if needed and open libaries
if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())), dependencies = TRUE)
}

invisible(lapply(packages, require, character.only = TRUE, warn.conflicts = TRUE, quietly =TRUE))


## q


## all the datasheets are in a folder on Box
## on my computer this is at C:\Users\dajansen3\Box\All data sheet entries EC 2023
## adapt to your path

sheet_folder = "C://Users//dajansen3//Box//All data sheet entries EC 2023"  ## no spaces :-)

## importance of consistency
## dates are in many formats
## this makes writing general code hard
tibble(filename = list.files(path = sheet_folder)[c(4, 164, 876, 992,  1091)])





## before dealing with all these problems we should check if we have all the data.
## how may sheets per house should there be?
# there should be 68 houses * 20 sheets = 1360

#But we have
length(list.files(path = sheet_folder))
(68 * 20) - length(list.files(path = sheet_folder)) ## are missing

tibble(filename = list.files(path = sheet_folder)) %>%
  mutate(house = as.numeric(str_sub(filename, 1, 3))) %>%
  group_by(house) %>%
  summarise(nr_sheets = n()) %>%
  summarise(max_nr_sheets = max(nr_sheets),
            min_nr_sheets = min(nr_sheets),
            median_nr_sheets = median(nr_sheets))

## the warning is because naming was not consistent

tibble(filename = list.files(path = sheet_folder)) %>%
  mutate(house = as.numeric(str_sub(filename, 1, 3))) %>%
  group_by(house) %>%
  summarise(nr_sheets = n()) %>%
  group_by(nr_sheets) %>%
  summarise(nr_houses = n()) %>%
  arrange(-nr_sheets)


## ok back to actually opening and combininmg the data

## The files have a few lines with metadata.
## These should not be loaded for count data
## Using skip the first 4 line are excluded

## For the count data the first column is also not need.
## Only the 2nd to 6th column contain data on tick counts
read_csv(list.files(path = sheet_folder, full.names = TRUE)[2], skip = 4, col_select = c(2:6))

## or
read_csv(list.files(path = sheet_folder, full.names = TRUE)[3], skip = 4, col_select = c(2:6))
## note that here there are NA values, because the tick count was not filled out
## are these all 0 or are the not sampled?

## and if we look at the metadata for the same 2 files
read_csv(list.files(path = sheet_folder, full.names = TRUE)[2], col_select = c(2:6), n_max = 1)

## or
read_csv(list.files(path = sheet_folder, full.names = TRUE)[3], col_select = c(2:6), n_max =1 )


## we will get back to the metadata issue later
## lets first see if we can we can open all of these
## we should have
all_datasheets <- list.files(path = sheet_folder)


## lets make a function to read the datasheets

read_data_v1 = function(filepath) {     ## hint v1 is suggesting there are going to be more :-)
  print(filepath) ## cab help to find issues
  read_csv(filepath, ## path to specific file,
           skip = 4, ## skip first 4 rows
           col_select = c(2:6),  ## select column 2 to 6
           show_col_types = FALSE, ## don't show the message,,
           name_repair = "minimal" ## don't try and repair names
           )
}

tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  slice(c(40)) %>% ## lest first try it on the first 25
  mutate(data = map(.x = path_to_sheet, .f = read_data_v1))

## lets try it on all of themn
tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(data = map(.x = path_to_sheet, .f = read_data_v1))

## problems
## what is happening

## errors stop the running of code. by using possibly from the purr package it keeps running
## and gives an alternative outcome, here and empty tibble. This can help to find problem files

read_data_possibly_v1 <- purrr::possibly(.f = read_data_v1, otherwise = tibble())

tibble(datasheet = all_datasheets) %>%
  slice(1:10) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(data = map(.x = path_to_sheet, .f = read_data_possibly_v1)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow))

read_lines(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "001_060523_pretreatment.csv")) %>%
  head(20)

shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "001_060523_pretreatment.csv"))
## extra lines of white space
## lets update the read functiopn to deal with this
## after a lot of trail and error I found that the fread function from the data.table package could deal with this

read_data_v2 = function(filepath) {     ## hint v2 is suggesting there are going to be more :-)
  data.table::fread(filepath, header = TRUE, skip = "transect", select = 2:6, blank.lines.skip = TRUE) %>%  as_tibble()
}

read_data_possibly_v2 <- purrr::possibly(.f = read_data_v2, otherwise = tibble())

tibble(datasheet = all_datasheets) %>%
  slice(1:10) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(data = map(.x = path_to_sheet, .f = read_data_possibly_v2)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow))

## better but there is still an problem
## lets look at that file
read_lines(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "001_6_22_23_PostTreatment2.csv"))
### that looks like an excel file ??

shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "001_6_22_23_PostTreatment2.csv"))
read_excel(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "001_6_22_23_PostTreatment2.csv"),
           skip = 4)

## ok so we need a new version of the read data files
## I'll also share now that there are real excel files with an xlsx extension
## to solve it we first need to know which files are:
# 1. real csv
# 2. real excel fies
# 3. excel files with csv extension

excel_files_with_csv_extension <-tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(data = map(.x = path_to_sheet, .f = read_data_possibly_v2)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow)) %>%
  filter(nr_rows_in_tibble == 0) %>%
  select(datasheet) %>% ## get only the datasheet names
  pull() ## and pull these into a vector

excel_files_with_csv_extension  %>%  length()

read_data_v3 = function(filetype, filepath) {     ## hint v2 is suggesting there are going to be more :-)
  if(filetype == "csv") {
    data.table::fread(filepath, header = TRUE, skip = "transect", select = 2:6, blank.lines.skip = TRUE)
  } else {
    read_excel(filepath, skip = 4) %>%
      select(transect, section, tick_species, tick_stage, tick_count)

  }
}

read_data_possibly_v3 <- purrr::possibly(.f = read_data_v3, otherwise = tibble())

tibble(datasheet = all_datasheets) %>%
  slice(1:10) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(data = map2(.x = real_filetype, .y = path_to_sheet, .f = read_data_possibly_v3)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow))


## it looks like we are getting there
## lets run it on all datasheets
combined_sheets <- tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(data = map2(.x = real_filetype, .y = path_to_sheet, .f = read_data_possibly_v3)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow))

combined_sheets %>%
  filter(nr_rows_in_tibble != 96)
shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "024 07_07_2023.csv"))
shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "024_6_23_23_PostTreatment.xlsx"))

combined_sheets %>%
  filter(nr_rows_in_tibble == 95) %>%
  unnest(cols = c(data)) %>%
  group_by(transect) %>%
  summarise(n())  ## we should have 4 x 2 * 3 points per zone

combined_sheets %>%
  filter(nr_rows_in_tibble == 95) %>%
  unnest(cols = c(data)) %>%
  filter(transect == "lawn") %>%
  group_by(section) %>%
  summarise(n())  ## we should have 2 * 3 points per zone

combined_sheets %>%
  filter(nr_rows_in_tibble == 95) %>%
  unnest(cols = c(data)) %>%
  filter(transect == "lawn"& section == "d") %>%
  janitor::tabyl(tick_species, tick_stage)

## so we are good?
combined_sheets %>%
  unnest(cols = c(data))

## another sigh moment
combined_sheets_temp <- combined_sheets %>%
    mutate(temp_data = map(.x = data, .f = mutate_all, as.character))

combined_sheets_temp %>%
  select(datasheet, temp_data) %>%
  unnest(cols = c(temp_data)) %>%
  mutate(temp_count = as.numeric(tick_count)) %>%
  filter(!is.na(tick_count)) %>%
  filter(is.na(temp_count))

## ok there are cells that have different values then NA or a number
combined_sheets_temp %>%
  select(datasheet, temp_data) %>%
  unnest(cols = c(temp_data)) %>%
  mutate(temp_count = as.numeric(tick_count)) %>%
  filter(is.na(temp_count)) %>%
  select(tick_count) %>%
  distinct() %>%
  pull()

## so there are multiple values for NA
## we need to tell R what these are
## we could do a mutate, but better is to fix the read_data function

read_data_v4 = function(filetype, filepath) {
  if(filetype == "csv") {
    data.table::fread(filepath
                      , header = TRUE
                      , skip = "transect"
                      , select = 2:6
                      , blank.lines.skip = TRUE
                      , na.strings = c(NA, "N/A", "NA", "Na", "nA", "N.A")
                      )
  } else {
    read_excel(filepath
               , skip = 4
               , na = c("N/A", "NA", "Na", "nA", "N.A"),
               , .name_repair = "unique_quiet"
               , progress = FALSE) %>%
      select(transect, section, tick_species, tick_stage, tick_count)

  }
}

read_data_possibly_v4 <- purrr::possibly(.f = read_data_v3, otherwise = tibble())

## before we run this lets look at the other values that are not NA values
combined_sheets_temp %>%
  select(datasheet, temp_data) %>%
  unnest(cols = c(temp_data)) %>%
  filter(tick_count %in% (c("20+", "1(M)", "1(F)", "1 (F)", "1pm")))

shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "051_7_18_23.csv"))
shell.exec(paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", "009_072823_posttreatment3.csv"))

## since there are not a lot it probably is best to fix
## to check the datasheets and fix by hand
## to be save you could always add a comment in a new column
## once these are fixed the new function should deal with the various NA values

combined_sheets <- tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(data = map2(.x = real_filetype, .y = path_to_sheet, .f = read_data_possibly_v4))

full_count_dataset <- combined_sheets %>%
  unnest(cols = c(data))

## if not a bit more of coding to do

## next metadat data

## to first two lines contain metadata regarding the houses
## besides the header row there is only 1 line of data
## as a reminder

read_csv(list.files(path = sheet_folder, full.names = TRUE)[2], n_max = 1)

## so only one row of data and we have need all 12 column

## but we know now that not all files are simply csv with a fixed structure
## lets see if we can adapt the last version of the read_data function

read_metadata_v1 = function(filetype, filepath) {     ## can we do it with 1 version?
  if(filetype == "csv") {
    data.table::fread(filepath
                      , header = TRUE
                      , blank.lines.skip = TRUE
                      , na.strings = c(NA, "N/A", "NA", "Na", "nA", "N.A")
                      , nrows = 1 ## besides the header we only want 1 row
    )
  } else {
    read_excel(filepath
               , na = c("N/A", "NA", "Na", "nA", "N.A")
               , n_max = 1
               )

  }
}




## first on a few
temp_metadata <- tibble(datasheet = all_datasheets) %>%
  slice(1:10) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(metadata = map2(.x = real_filetype, .y = path_to_sheet, .f = read_metadata_v1)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = metadata, .f = nrow))

temp_metadata ## looks good

# but
temp_metadata %>%
  slice(3:9) %>%
  unnest(cols = c(metadata))

## again a conflict between datatypes in a column

temp_metadata %>%
  mutate(temp_metadata = map(.x = metadata, .f = mutate_all, as.character)) %>%
  select(datasheet, temp_metadata) %>%
  unnest(cols = c(temp_metadata))


## ok it only complained about temp, but I also see issues in other variables
## suggestions on what they are ?

## lets go ahead and run open all the data and see if we have a big issue
## or just a few that we can fix manually

temp_metadata <- tibble(datasheet = all_datasheets) %>%
  mutate(path_to_sheet = paste0("C://Users//dajansen3//Box//All data sheet entries EC 2023/", datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(metadata = map2(.x = real_filetype, .y = path_to_sheet, .f = read_metadata_v1)) %>%
  mutate(temp_metadata = map(.x = metadata, .f = mutate_all, as.character)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = metadata, .f = nrow))


check_metadata <- temp_metadata %>%
  select(datasheet, temp_metadata) %>%
  unnest(cols = c(temp_metadata))

check_metadata

## we seem to have 2 extra columns lets check those first
check_metadata %>%
  filter(!is.na(Temp))

step1 <- check_metadata %>%
  mutate(temp = if_else(is.na(temp), Temp, temp)) %>%
  select(-Temp)

step1 %>%
  filter(!is.na(...1)) ## ok so ...1 is house

step2 <- step1 %>%
  mutate(house = if_else(is.na(house), ...1, house)) %>%
  select(-...1)

step2 %>%
  mutate(temp_numeric = as.numeric(temp))

# invalid multibyte string at '<a1> F' ## no idea what that means
# has something to do with a non standard character
## you can find them by looking at character length
## thanks google

as.numeric_possible <- possibly(.f = as.numeric, otherwise = 999)

step2 %>%
  mutate(temporal_temp = map_dbl(.x = temp, .f = as.numeric_possible)) %>%
  select(datasheet, house, temp, temporal_temp) %>%
  filter(is.na(temporal_temp) | temporal_temp == 999) %>%
  View()

## tricky one to solve unless you go into all those datasheets and fix it
## but I noticed that none of the temperatures in the problems sheets were above 99
## so we could just take the first 2 digits

step2 %>%
  mutate(temporal_temp = map_dbl(.x = temp, .f = as.numeric_possible)) %>%
  select(datasheet, house, temp, temporal_temp) %>%
  mutate(new_temp = as.numeric(if_else(condition = is.na(temporal_temp) | temporal_temp == 999,
                            true = str_sub(string = temp, 1, 2),
                            false = temp))) %>%
  filter(is.na(new_temp)) %>%
  View()

## that seems ok as all the NA are NA values in the old tempo
step3 <- step2 %>%
  mutate(temporal_temp = map_dbl(.x = temp, .f = as.numeric_possible)) %>%
  mutate(temp_numeric = as.numeric(if_else(condition = is.na(temporal_temp) | temporal_temp == 999,
                                       true = str_sub(string = temp, 1, 2),
                                       false = temp))) %>%
  select(-temporal_temp)

## Next VMC
step3 %>%
  mutate(VMC_numeric = as.numeric(VMC)) %>%
  filter(is.na(VMC_numeric))

## see if we remove % sign we fix the issue
step3 %>%
  mutate(VMC_numeric = as.numeric(str_remove_all(string = VMC, pattern = "%"))) %>%
  filter(!is.na(VMC) & is.na(VMC_numeric)) %>%
  select(datasheet, VMC)

step4 <- step3 %>%
  mutate(VMC_numeric = as.numeric(str_remove_all(string = VMC, pattern = "%")))

step4 %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  select(datasheet, date) %>%
  mutate(date_ymd = mdy(date)) %>%
  mutate(date_ymd = if_else(is.na(date_ymd), ymd(date), date_ymd)) %>%
  filter(is.na(date_ymd))

step5 <-step4 %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(date_ymd = mdy(date)) %>%
  mutate(date_ymd = if_else(is.na(date_ymd), ymd(date), date_ymd))

## almost there
## just the time ones left
step5 %>%
  filter(!is.na(start_time) | !is.na(end_time)) %>%
  select(datasheet, start_time, end_time) %>%
  mutate(start_time_hm = hm(start_time),
         end_time_hm = hm(end_time)) %>%
  filter(is.na(start_time_hm) | is.na(end_time_hm))


step5 %>%
  filter(!is.na(start_time) | !is.na(end_time)) %>%

  select(datasheet, start_time, end_time) %>%
  mutate(start_time_hm = hm(start_time),
         end_time_hm = hm(end_time)) %>%
  filter(is.na(start_time_hm) | is.na(end_time_hm)) %>%
  mutate(start = str_remove(start_time, "1899-12-31 "),
       end =  str_remove(end_time, "1899-12-31 ")) %>%
  mutate(start_time_hm = if_else(is.na(start_time_hm), hms(start),start_time_hm),
         end_time_hm = if_else(is.na(end_time_hm), hms(end),end_time_hm)) %>%
  filter((!is.na(start_time) & is.na(start_time_hm)) |
           (!is.na(end_time) & is.na(end_time_hm)))

## fix these by hand

final_metadata <- step5 %>%
  mutate(start_time_hm = hm(start_time),
         end_time_hm = hm(end_time)) %>%
  mutate(start = str_remove(start_time, "1899-12-31 "),
         end =  str_remove(end_time, "1899-12-31 ")) %>%
  mutate(start_time_hm = if_else(is.na(start_time_hm), hms(start),start_time_hm),
         end_time_hm = if_else(is.na(end_time_hm), hms(end),end_time_hm))

## need to fix some of the column names
full_count_dataset %>%
  inner_join(final_metadata)  ## this won't work, because the errors that are still in the dataset


## Use cleaned up data
updated_sheet_folder = "C://Users//dajansen3//Box//EC 2023 data by housecode/"
all_datasheets_cleaned <- list.files(updated_sheet_folder, all.files = TRUE, recursive = TRUE, pattern = "csv")

excel_files_with_csv_extension <-tibble(datasheet = all_datasheets_cleaned) %>%
  mutate(path_to_sheet = paste0(updated_sheet_folder, datasheet)) %>%
  slice(1:10) %>%
  mutate(data = map(.x = path_to_sheet, .f = read_data_possibly_v2)) %>%
  mutate(nr_rows_in_tibble = map_dbl(.x = data, .f = nrow)) %>%
  filter(nr_rows_in_tibble == 0) %>%
  select(datasheet) %>% ## get only the datasheet names
  pull() ##



tibble(datasheet = all_datasheets_cleaned) %>%
  slice(1:10) %>%
  mutate(path_to_sheet = paste0(updated_sheet_folder, datasheet)) %>%
  mutate(filetype = if_else(str_detect(datasheet, "csv"), "csv", "xlsx")) %>%
  mutate(real_filetype = if_else(datasheet %in% excel_files_with_csv_extension, 'xlsx', filetype)) %>%
  mutate(data = map2(.x = real_filetype, .y = path_to_sheet, .f = read_data_possibly_v4)) %>%
  mutate(nr_row_on_sheet = map_dbl(.x = data, .f = nrow)) %>%
  unnest(cols = c(data))

shell.exec(paste0(updated_sheet_folder, "036/036_2023_06_27_posttreatment3 (2).csv"))

full_count_dataset <- combined_sheets %>%
  unnest(cols = c(data))


### kust one line per house
house_metadata_folder <- "C://Users//dajansen3//Box//EC 2023 house metadata//"

housemetadata <- tibble(datasheet = list.files(house_metadata_folder)) %>%
	filter(datasheet != "_metadata.csv") %>%
	mutate(filepath = paste0(house_metadata_folder, datasheet)) %>%
	mutate(data = map(.x = filepath, .f = fread))

housemetadata %>%
	unnest(cols = c(data)) %>%
	View()




