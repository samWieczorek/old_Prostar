col <- read.table("~/data/Classeur1.txt", sep="\t", h=F,stringsAsFactors = F)
dim(col)
col <- col[,1]

# check if empty lines
if (length(which(col=="" | is.na(col))) > 0) {
  "Protein ID contains NA or empty values"
}

# check the type. All character?
sapply(col, is.integer)
class(col[1])

# check the separators
inputUser <- " "

separators <- c(' ', '.', ",", "_", ";", "-")
separators <- separators[-which(inputUser == separators)]

sepToCheck <- character()
for (i in separators) {
  sepToCheck <- paste0( sepToCheck, gsub('"',"",i),"|" )
}
sepToCheck <- substr(sepToCheck,1,nchar(sepToCheck)-1)
sepToCheck <- gsub("\\.", "\\\\.", sepToCheck)

liste <- sapply(col, function(x) strsplit(x, sepToCheck))
if (length(which(lengths(liste)>1))>1) {
  "Others separators exist"
}
