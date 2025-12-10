#### load packages ####
library(tidyverse)


### Import data ####

# Note current script location
current_script_dir <- this.path::this.dir()
cat("Script directory:", current_script_dir, "\n")

# Set project root
project_root <- this.path::dirname2(current_script_dir)
setwd(project_root)
cat("Project root set to:", getwd(), "\n")


# load data
load(file.path(project_root, "01_data/processed/data.Rdata"))


### To produce Table 1 in the Paper ####

# Age

age_tbl <- data |>
  count(age_fac, groups, name = "n") |>
  pivot_wider(names_from  = groups,
              values_from = n,
              values_fill = 0) |>
  rowwise(age_fac) |>
  mutate(full_sample = sum(c_across(everything()))) |>
  ungroup()  |>
  dplyr::relocate("full_sample") |> ### move to second position
  dplyr::relocate("age_fac") |> ### move to 1st column
  select(!contains("NA")) |>   ### remove NA column
  mutate(across((2:6), ~ (.x / sum(.x)) * 100)) ### make everything percent

age_tbl <- tinytable::tt(age_tbl ,
                         width = c(.5, .1, .1, .1, .1, .1) ,
                         digits = 2)

# Gender

gender_tbl <- data |>
  count(female_fac, groups, name = "n") |>
  pivot_wider(names_from  = groups,
              values_from = n,
              values_fill = 0) |>
  rowwise(female_fac) |>
  mutate(full_sample = sum(c_across(everything()))) |>
  ungroup()  |>
  dplyr::relocate("full_sample") |> ### move to second position
  dplyr::relocate("female_fac") |> ### move to 1st column
  select(!contains("NA")) |>   ### remove NA column
  mutate(across((2:6), ~ (.x / sum(.x)) * 100)) ### make everything percent

gender_tbl <- tinytable::tt(gender_tbl ,
                            width = c(.5, .1, .1, .1, .1, .1) ,
                            digits = 2)

# gender_table <- table(data$female_fac, data$groups)
# full_sample <- table(data$female_fac)
#
# gender_combined <- cbind(full_sample, gender_table)
#
# # Make Colwise to 100%
#
# N <- colSums(gender_combined)
# gender_combined <- t(apply(gender_combined, MAR = 1,
#                            function (x) (x / N) * 100))
# gender_combined <- rbind(round(gender_combined,1),
#                          N)
#

# Education

educ_tbl <- data |>
  count(education_fac, groups, name = "n") |>
  pivot_wider(names_from  = groups,
              values_from = n,
              values_fill = 0) |>
  rowwise(education_fac) |>
  mutate(full_sample = sum(c_across(everything()))) |>
  ungroup()  |>
  dplyr::relocate("full_sample") |> ### move to second position
  dplyr::relocate("education_fac") |> ### move to 1st column
  select(!contains("NA")) |>   ### remove NA column
  mutate(across((2:6), ~ (.x / sum(.x)) * 100)) ### make everything percent

educ_tbl <- tinytable::tt(educ_tbl ,
                          width = c(.5, .1, .1, .1, .1, .1) ,
                          digits = 2)


# education_table <- table(data$education_fac, data$groups)
# full_sample <- table(data$education_fac)
#
# education_combined <- cbind(full_sample, education_table)
#
#
# # Make Colwise to 100%
#
# N <- colSums(education_combined)
# education_combined <- t(apply(education_combined, MAR = 1,
#                               function (x) (x / N) * 100))
# education_combined <- rbind(round(education_combined,1),
#                             N)




# Income

income_tbl <- data |>
  count(income_fac, groups, name = "n") |>
  pivot_wider(names_from  = groups,
              values_from = n,
              values_fill = 0) |>
  rowwise(income_fac) |>
  mutate(full_sample = sum(c_across(everything()))) |>
  ungroup()  |>
  dplyr::relocate("full_sample") |> ### move to second position
  dplyr::relocate("income_fac") |> ### move to 1st column
  select(!contains("NA")) |>   ### remove NA column
  mutate(across((2:6), ~ (.x / sum(.x)) * 100)) ### make everything percent

income_tbl <- tinytable::tt(income_tbl,
                            width = c(.5, .1, .1, .1, .1, .1) ,
                            digits = 2)

# Obs per group

n_tbl <- data |>
  count(income_fac, groups, name = "n") |>
  pivot_wider(names_from  = groups,
              values_from = n,
              values_fill = 0) |>
  rowwise(income_fac) |>
  mutate(full_sample = sum(c_across(everything()))) |>
  ungroup()  |>
  dplyr::relocate("full_sample") |> ### move to second position
  dplyr::relocate("income_fac") |> ### move to 1st column
  select(!contains("NA")) |>   ### remove NA column
  summarise(across(where(is.numeric), \(x) sum(x, na.rm = TRUE))) |> ### Make just the sum
  mutate(N = "N") |>
  dplyr::relocate("N") ### move N to 1st column

n_tbl <- tinytable::tt(n_tbl  ,
                       width = c(.5, .1, .1, .1, .1, .1) ,
                       digits = 2)

#
# income_table <- table(data$profile_gross_personal_eu_2_fac, data$groups)
#
# full_sample <- table(data$profile_gross_personal_eu_2_fac)
#
# income_combined <- cbind(full_sample, income_table)
#
#
# # Make Colwise to 100%
#
# N <- colSums(income_combined)
# income_combined <- t(apply(income_combined, MAR = 1,
#                            function (x) (x / N) * 100))
# income_combined <- rbind(round(income_combined,1),
#                          N)


# Combine to generate Table 1 in the paper

for_tab_1 <- rbind2(age_tbl, gender_tbl, headers = FALSE, use_names = FALSE)
for_tab_1 <- rbind2(for_tab_1, educ_tbl, headers = FALSE, use_names = FALSE)
for_tab_1 <- rbind2(for_tab_1,
                    income_tbl,
                    headers = FALSE,
                    use_names = FALSE)
tab_1 <- rbind2(for_tab_1, n_tbl, headers = FALSE, use_names = FALSE)

rm(age_tbl, gender_tbl, educ_tbl , income_tbl, n_tbl, for_tab_1)




