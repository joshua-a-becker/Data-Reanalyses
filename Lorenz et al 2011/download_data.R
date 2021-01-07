library(readxl)
library(httr)
library(dplyr)

url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"


GET(url, write_disk(tf <- "lorenz_et_al.xls"))
d2 <- read_excel(tf) %>% subset(Information_Condition!="no")
