library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

DATA.DIR = '../data/'

data = read.csv(paste0(DATA.DIR,'tidy.csv')) %>% set_colnames(tolower(names(.)))

tellme = function(x){
    read.csv(paste0(DATA.DIR,'/police_stop_data_dictionary.csv')) %>%
        filter(Column.Name==x) %>%
        select(Description) %>%
        return
}

app.key.list = function(n){
    key = paste0('rpt',as.character(n))
    data %>% select(starts_with(key)) %>% names %>% return    
}


welfare.arrest = data %>% filter(rpt2.welfare.check==1 & rpt6.arrest ==1)
equip.arrest =  data %>%  filter(rpt2.equipment.violation==1 & rpt6.arrest ==1)

cites.race = data %>% filter(rpt6.citation==1) %>% select(race) %>% table
#cites.race = warnings.race/table(data$race)
