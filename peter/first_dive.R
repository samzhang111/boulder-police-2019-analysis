library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

DATA.DIR = '../data'

main = read.csv(paste0(DATA.DIR,'/police_stop_data_main_2018.csv'))
res = read.csv(paste0(DATA.DIR,'/police_stop_data_results_2018.csv'))
desc = read.csv(paste0(DATA.DIR,'/police_stop_data_dictionary.csv'))

names(main) = main %>% names %>% tolower
names(res) = res %>% names %>% tolower

tellme = function(x) {dict %>% filter(Column.Name==x) %>% select(Description) %>% return}

main$race2 = main$race %>% as.character
main$race2[ main$race2=='W' & main$ethnic=='H' ] = 'H'

race.counts = table(main$race2) %>% as.data.frame %>% set_colnames(c('race2','freq'))
main = main %>% left_join(race.counts,by='race2')

p = ggplot(main %>% filter(min<60), aes(x=race2,y=min,fill=as.factor(freq))) +
    geom_boxplot() +
    scale_fill_brewer(palette = 'RdYlGn')
                     
