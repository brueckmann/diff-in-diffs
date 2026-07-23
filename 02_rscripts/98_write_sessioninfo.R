# Note current script location
current_script_dir <- this.path::this.dir()
cat("Script directory:", current_script_dir, "\n")

# Set project root
project_root <- this.path::dirname2(current_script_dir)
setwd(project_root)
cat("Project root set to:", getwd(), "\n")


#### Session Info 
writeLines(capture.output(sessionInfo()),("session_info.txt"))

      

