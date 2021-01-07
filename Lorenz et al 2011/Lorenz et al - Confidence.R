library(readxl)
library(httr)
library(dplyr)

url = "http://www.pnas.org/highwire/filestream/606236/field_highwire_adjunct_files/1/sd01.xls"


GET(url, write_disk(tf <- "lorenz_et_al.xls"))
d2 <- read_excel(tf) %>% subset(Information_Condition!="no") %>%
  mutate(
      err1 = abs(log(E1/Truth))
    , err5 = abs(log(E5/Truth))
  )

### confidence negatively correlated with error
### (more confidence, less error)
cor.test(d2$err1, d2$C1)
cor.test(d2$err5, d2$C5)

### confidence goes up
mean(d2$C1)
mean(d2$C5)
