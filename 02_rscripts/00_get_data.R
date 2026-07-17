# 00_get_raw_data.R

library("dataverse")
#  Colantone, Italo; Livio Di Lonardo; Yotam Margalit; Marco Percoco, 2023, "Replication Data for: The Political Consequences of Green Policies: Evidence from Italy", https://doi.org/10.7910/DVN/NSC8TJ, Harvard Dataverse, V2, UNF:6:RuQPm48kSIo+TzZ84zn8XA== [fileUNF] 

Replication_data <- get_dataframe_by_name(
  filename = "Replication_Dataset.tab",
  dataset = "https://doi.org/10.7910/DVN/NSC8TJ", 
  server = "dataverse.harvard.edu")


# Note current script location
current_script_dir <- this.path::this.dir()
cat("Script directory:", current_script_dir, "\n")

# Set project root
project_root <- this.path::this.proj() 
setwd(project_root)
cat("Project root set to:", getwd(), "\n")


# Define the RDS directory using this.path
raw_data_dir <- this.path::path.join(project_root, "01_data", "raw")
# Create directory if it doesn't exist
dir.create(raw_data_dir, showWarnings = FALSE, recursive = TRUE)


## Save to Dta file using haven:
haven::write_dta(Replication_data, 
this.path::path.join(raw_data_dir, "Replication_Dataset.dta"))