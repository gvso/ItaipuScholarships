ROOT_DIR <- "~/Data/Itaipu/2019/"

data <- read.csv(paste0(ROOT_DIR, "2019_por_ci.csv"))

data <- data[!is.na(as.numeric(as.character(data$total))),]

write.csv(data, paste0(ROOT_DIR, "scores.csv"), row.names = FALSE)