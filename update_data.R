#--------------------------------------------------
# UPDATE DATA SCRIPT
#--------------------------------------------------
# Run Me to Updata Datafiles
#--------------------------------------------------
source("R/data_cleaner.R")
setwd("data/raw/experimental/")
score_emotional_data()
setwd("processed/")
create_emo_dataset()
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("../../../..")
#--------------------------------------------------
source("R/score_gmsi_extra.R")
setwd("data/raw/gmsi/")
score.gmsi.extra()
create.gmsi.dataset()
junk <- dir(pattern="_data") 
file.remove(junk)
setwd("../../../")
#--------------------------------------------------
# Make Master

experimental_data <- read_csv("data/cooked/experimental_data.csv")
gmsi_data <- read_csv("data/Current_GMSI_Data.csv")

gmsi_data %>%
  rename(subject = subjectNo) -> gmsi_data

#View(gmsi_data)

experimental_data %>%
  select(-X1) %>%
  left_join(gmsi_data) -> joined_data

#View(joined_data)

write_csv(joined_data, "Master_Data.csv")
