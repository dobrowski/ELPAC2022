


library(tidyverse)
library(janitor)
library(MCOE)
library(here)
library(ggthemes)
library(vroom)
library(ggrepel)


con <- mcoe_sql_con()

ent <- read_delim(here("data","sa_elpac2022_entities_csv_v1.zip"),
           delim = "^")


elpac.mry <- tbl(con, "SELPAC") %>% 
 #   head(20) %>%
    filter(CountyCode == "27",
           # DistrictCode == "10272",
           TestYear >= "2022"
           ) %>%
    collect() %>%
    left_join_codebook("SELPAC", "StudentGroupID") %>%
    rename(StudentGroup = definition) %>%
    left_join_codebook("SELPAC", "RecordType") %>%
    rename(EntityType = definition) %>%
    left_join(ent, by = c("CountyCode", "DistrictCode", "SchoolCode")) 
    


### Graphs Countywide  ----


# County  by subgroup
elpac.mry %>%
    filter(Grade == 13,
           DistrictCode == "00000",
        #   is.na(District_Name),
           
           # Subgroup_ID == "1",
       #    Test_Id == 2, # ELA 
           #          Entity_Type == "District",
           !is.na(OverallPerfLvl4Pcnt),
       #    !str_detect(Subgroup, " - ")
    ) %>%
    mutate(StudentGroup.n = paste0(StudentGroup," (",TotalTestedWithScores,")" )) %>%
    lollipop(OverallPerfLvl4Pcnt,
             StudentGroup.n,
             "pink") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("ELPAC ", " Rates at Level 4 for Monterey County by Student Group"),
         caption = "Source: ELPAC Research Files  \n https://caaspp-elpac-preview.ets.org/elpac/ResearchFilesSA") 

ggsave(here("figs", paste0("Monterey County ELPAC by subgroup ",  Sys.Date(),".png" )),
       width = 8, height = 7)


# County by District
elpac.mry %>%
    filter(Grade == 13,
           SchoolCode == "0000000",
           #   is.na(District_Name),
           
            StudentGroupID == "001",
           #    Test_Id == 2, # ELA 
           #          Entity_Type == "District",
           !is.na(OverallPerfLvl4Pcnt),
           #    !str_detect(Subgroup, " - ")
    ) %>%
    mutate(DistrictName.n = paste0(DistrictName," (",TotalTestedWithScores,")" )) %>%
    lollipop(OverallPerfLvl4Pcnt,
             DistrictName.n,
             "pink") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("ELPAC ", " Rates at Level 4 for Monterey County by District"),
         caption = "Source: ELPAC Research Files  \n https://caaspp-elpac-preview.ets.org/elpac/ResearchFilesSA") 


ggsave(here("figs", paste0("Monterey County ELPAC by District ",  Sys.Date(),".png" )),
       width = 8, height = 6)

# County by grade
elpac.mry %>%
    filter(Grade != 13,
           DistrictCode == "00000",
           #   is.na(District_Name),
           
           StudentGroupID == "001",
           #    Test_Id == 2, # ELA 
           #          Entity_Type == "District",
           !is.na(OverallPerfLvl4Pcnt),
           #    !str_detect(Subgroup, " - ")
    ) %>%
    mutate(Grade.n = paste0(Grade," (",TotalTestedWithScores,")" )) %>%
    lollipop(OverallPerfLvl4Pcnt,
             Grade.n,
             "pink") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("ELPAC ", " Rates at Level 4 for Monterey County by Grade"),
         caption = "Source: ELPAC Research Files  \n https://caaspp-elpac-preview.ets.org/elpac/ResearchFilesSA") 



ggsave(here("figs", paste0("Monterey County ELPAC by Grade ",  Sys.Date(),".png" )),
       width = 8, height = 6)



# District by School

elpac.district.by.school <- function(dist) {
 
elpac.mry %>%
    filter(Grade == 13,
           str_detect(DistrictName,dist), # Alisal
           #   is.na(District_Name),
           StudentGroupID == "001",
           #    Test_Id == 2, # ELA 
           #          Entity_Type == "District",
           !is.na(OverallPerfLvl4Pcnt),
           #    !str_detect(Subgroup, " - ")
    ) %>%
    mutate(SchoolName.n = if_else(is.na(SchoolName),
                                  paste0(DistrictName," (",TotalTestedWithScores,")" ),
                                  paste0(SchoolName," (",TotalTestedWithScores,")" )
                                  )
           ) %>%
    lollipop(OverallPerfLvl4Pcnt,
             SchoolName.n,
             "pink") +
    labs(x = "",
         y = "",
         color ="",
         title = paste0("ELPAC ", " Rates at Level 4 for Alisal by School"),
         caption = "Source: ELPAC Research Files  \n https://caaspp-elpac-preview.ets.org/elpac/ResearchFilesSA") 

    
}


elpac.district.by.school("Alisal")

elpac.district.by.school("Monterey Peninsula")

elpac.district.by.school("Soledad")
