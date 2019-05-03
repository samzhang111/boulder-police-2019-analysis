library(reshape2)
library(magrittr)
library(dplyr)
library(ggplot2)
library(RColorBrewer)

DATA.DIR = '../data/'

data = read.csv(paste0(DATA.DIR,'tidy.csv')) %>% set_colnames(tolower(names(.)))
data$searched = !(data$rpt3.no.search | data$rpt3..no.search)

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

# posterior distribution given binomial likelihood and beta prior
unif.beta.updated = function(x, outcome_vec, alpha=1,beta=1){
    x %>% dbeta(alpha + sum(outcome_vec), beta + length(outcome_vec) - sum(outcome_vec)) %>%
        return
}

welfare.arrest = data %>% filter(rpt2.welfare.check==1 & rpt6.arrest ==1)
equip.arrest =  data %>%  filter(rpt2.equipment.violation==1 & rpt6.arrest ==1)


cites.race = data %>% filter(rpt6.citation==1) %>% select(race) %>% table

# is there a stat sig diff in warning rates vs race?
data$only.warning = !(apply(data %>% select( app.key.list(6) ) %>% select(-rpt6.warning),
                          1,
                          sum) %>% as.logical) & data$rpt6.warning
    
warn.race = data %>%
    filter(race %in% c('W','B')) %>%
    select(race, only.warning) %>%
    table %>%
    fisher.test

# given that you didn't a warning was there racialized diffs in warnings?


# given a search was performed, was there a stat sig diff in whether contraband was found?
data$cont.found = apply(data %>%
                        select( starts_with('rpt5') ) %>%
                        select(-rpt5.nothing.seized),
                        1, any)

race.search.outcomes = data %>%
    filter(searched==1) %>%
    select(race,cont.found) %>%
    table %>%
    fisher.test

prob.cont.given.searched.by.race = function(x,r){ data %>%
                                                         filter(searched==1) %>%
                                                         filter(race==r) %>%
                                                         select(cont.found) %>%
                                                         as.matrix %>%
                                                         unif.beta.updated(x,.) %>%
                                                         return }
x = seq(0,1,.001)

data$raceethn2 = data$race %>% as.character
to.change = data$ethnic=='H'
data$raceethn2[ to.change ] = paste0(data$race[to.change], data$ethnic[to.change])

plot.df = sapply(data$raceethn2 %>% unique %>% as.character,
                 function(r){x %>% prob.cont.given.searched.by.race(r)}) %>% as.data.frame
plot.df$x = x
plot.df = plot.df %>% melt('x') %>% set_colnames(c('x','race','post'))
p = ggplot( plot.df, aes(x=x, y=post, color=race, group=race) ) +
    geom_line() +
    scale_colour_brewer(palette = 'Accent')
