#################################
# Animal movement - PhD course  #
#################################

# Automated script to install the R packages needed for the programme of this course.

# Checking the version of R required for the pipeline
if (!grepl("R version 4.2.3", R.version$version.string)) {
  stop("This course requires R version 4.2.3 to be installed.\n")
}

# List of packages required for the pipeline
ListPackages<-c("plyr", "dplyr", "ggplot2", "gganimate", "tidyverse", "lubridate", "knitr", "png", "grid",# General packages
                "amt", "ctmm", "adehabitatLT", "circular", "move2", "KernSmooth", "sf", "raster", "terra", # Outdoor packages
                "sp", "rgdal", "pracma", "adehabitatHR", "igraph", "ergm", "network", "vroom" # Indoor  packages
                )

for (i in c(1:length(ListPackages))){
  package_name<-ListPackages[i]
  if (!requireNamespace(package_name, quietly = TRUE)) {
    # Install the package if is not
    install.packages(package_name, dependencies = TRUE, quiet=TRUE)
    
    if (!requireNamespace(package_name, quietly = TRUE)) {
      cat("Error: Unable to install package: ", package_name, "\n")
    } else {
      cat("Package", package_name, "installed and loaded successfully.\n")
    }
  } else {
    cat("Package: ", package_name, "was already installed.\n")
  }
}

# Install R tools (not required so far)
#print("Warning: some packages might require Rtools installed in your computer")
#install.packages("https://cran.r-project.org/bin/windows/Rtools/rtools42/files/rtools42-5355-5357.exe", repos = NULL, type = "win.binary")