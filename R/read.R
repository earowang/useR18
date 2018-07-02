library(tidyverse)
library(lubridate)

data_15 <- paste0("data/", list.files("data/", "csv"))

links <- c(
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/96d997fe-3d33-4f50-9ae6-c2249cdd1a34/download/ca18295631-contact-centre-customer-enquiries-jan-mar-2018.csv", 
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/bbfa5ce4-2063-41b4-afd7-cd03cc766655/download/ca1860771-contact-centre-customer-enquiries-oct-dec-2017.csv",
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/bd78fdaa-1b9a-4c13-a38e-c6ee946c80fd/download/ca171090090-contact-centre-customer-enquiries-jul-sep-2017.csv",
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/dfc00fd0-95d2-4c7c-9e11-234af3e1f105/download/ca171090074-contact-centre-customer-enquiries-jan-jun-2017.csv",
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/abd1db81-2edb-48ee-910b-56156777fa2d/download/ca1860891-contact-centre-customer-enquiries-jan-dec-2016.csv",
  "https://www.data.brisbane.qld.gov.au/data/dataset/beaaf316-7415-4333-a3a5-dba94db9dd57/resource/8264f69e-5ad3-4131-ac02-94db483b63d4/download/ca18198349-contact-centre-customer-enquiries-jan-dec-2014.csv"
  )

dat <- map_dfr(c(links, data_15), ~ read_csv(.) %>% 
    rename_all(tolower) %>% 
    rename(work_site = `work site`) %>% 
    mutate(date = dmy(date))
  )

enquiry <- dat %>% 
  filter(work_site == "Call Centre") %>% 
  mutate(
    channel = gsub("( | -).*$", "", channel),
    channel = if_else(channel == "e-mail", "Email", channel),
    channel = if_else(channel == "WEB", "Web", channel)
  ) %>% 
  group_by(date, service, category, channel) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup() %>% 
  mutate(
    category = if_else(
      category == "Building Control Services", 
      "Built Structure Control", category
    ),
    category = if_else(
      service == "Native Species Programs",
      "Other", category
    ),
    service = if_else(
      service == "Native Species Programs",
      "Other", service
    )
  )

category_sel <- enquiry %>% 
  group_by(category) %>% 
  summarise(volume = sum(volume)) %>% 
  arrange(desc(volume)) %>% 
  slice(1:11) %>% 
  pull(category)

service_sel <- enquiry %>% 
  filter(category %in% category_sel) %>% 
  distinct(service) %>% 
  pull(service)

enquiry <- enquiry %>% 
  filter(service != category) %>% 
  mutate(
    service = fct_other(service, keep = service_sel),
    category = if_else(service == "Other", "Other", category),
    category = if_else(category == "Commercial Services", "Other", category),
    category = fct_relevel(category, "Other", after = Inf),
    channel = if_else(channel %in% c("Online", "Web"), "Online", channel),
    channel = fct_other(channel, drop = c("SMS", "Fax", "Face2Face"))
  ) %>% 
  group_by(date, channel, category, service) %>% 
  summarise(volume = sum(volume)) %>% 
  ungroup()

write_rds(enquiry, path = "data/enquiry.rds", compress = "gz")
