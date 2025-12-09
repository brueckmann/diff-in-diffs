library(tidyverse)

### Produce Table 1 in the Paper


age_table <- table(Replication_data$age_rc2, Replication_data$groups)
full <- table(Replication_data$age_rc2)
age_combined <- cbind(full, age_table)
colnames(age_combined) <- c('Full Sample', 
                            'Diesel Euro 4',
                            'Diesel Euro 5', 
                            'Petrol Euro 4',
                            'Petrol Euro 5')

N <- colSums(age_combined)
age_combined <- t(apply(age_combined, MAR = 1,
                        function (x) (x / N) * 100))
age_combined <- rbind(round(age_combined,1),N)

# Income


income_table <- table(Replication_data$profile_gross_personal_eu_2_fac, Replication_data$groups)

full <- table(Replication_data$profile_gross_personal_eu_2_fac)

income_combined <- cbind(full, income_table)
colnames(income_combined) <- c('Full Sample', 
                               'Diesel Euro 4',
                               'Diesel Euro 5', 
                               'Petrol Euro 4',
                               'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(income_combined)
income_combined <- t(apply(income_combined, MAR = 1,
                           function (x) (x / N) * 100))
income_combined <- rbind(round(income_combined,1),
                         N)

# Education

education_table <- table(Replication_data$education_fac, Replication_data$groups)
full <- table(Replication_data$education_fac)



education_combined <- cbind(full, education_table)
colnames(education_combined) <- c('Full Sample', 
                                  'Diesel Euro 4',
                                  'Diesel Euro 5', 
                                  'Petrol Euro 4',
                                  'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(education_combined)
education_combined <- t(apply(education_combined, MAR = 1,
                              function (x) (x / N) * 100))
education_combined <- rbind(round(education_combined,1),
                            N)

# Gender

gender_table <- table(Replication_data$female, Replication_data$groups)
full <- table(Replication_data$female)

gender_combined <- cbind(full, gender_table)
colnames(gender_combined) <- c('Full Sample', 
                               'Diesel Euro 4',
                               'Diesel Euro 5', 
                               'Petrol Euro 4',
                               'Petrol Euro 5')

# Fix up table for presentation

N <- colSums(gender_combined)
gender_combined <- t(apply(gender_combined, MAR = 1,
                           function (x) (x / N) * 100))
gender_combined <- rbind(round(gender_combined,1),
                         N)

# Final Table 
library(xtable)

combined_table <- rbind(age_combined, income_combined, education_combined, gender_combined)

