library(DAPAR)

data <- readExcel("~/data/pept/__toto..xlsx", sheet = "Feature Meta Data" )

index <- which(is.na(data$Proteins)) # && which(data$Proteins == "")

if (length(index)>0) {
  paste0(length(index), " peptides don't have parent protein.")
}
