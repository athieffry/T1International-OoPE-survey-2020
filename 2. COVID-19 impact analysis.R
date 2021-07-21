#### T1International Survey 2020: 2. COVID-19 impact
#### Axel Thieffry - April 2021
library(tidyverse)
library(tidylog)
library(magrittr)
library(reshape2)
library(factoextra)
library(eulerr)
library(RColorBrewer)
'h' <- head
'l' <- length
'rename' <- dplyr::rename


# 0. READ DATA ####
# -----------------
data <- readRDS('1b - Clean Data/cleaned_data_04_21_2021.rds')
assertthat::assert_that(nrow(data) == 1066)

top5_countries <- readRDS('1b - Clean Data/top5_countries.rds')



# 1. COVID VENN DIAGRAMS ####
# ---------------------------
# a. extract COVID-related info from survey
covid <- data %>% select(alpha2, coverage, matches('covid'))

# b. overall venn diagram
covid %>%
  select(-alpha2, -coverage) %>%
  equals('Checked') %>%
  euler(shape='ellipse') %>% plot(quantities=T, main='COVID had an impact?')

# c. remove entries "Prefer not to answer"
covid %<>% filter(covid_pnta == 'Unchecked') %>% select(-covid_pnta)

# d. new venn diagram
covid %>%
  select(-alpha2, -coverage) %>%
  equals('Checked') %>%
  euler() %>% plot(quantities=T, main='COVID had an impact?')

# e. venn diagram per Top 5 country
    # USA
    covid %>%
      filter(alpha2=='US') %>%
      select(-alpha2, -coverage) %>%
      equals('Checked') %>%
      euler() %>% plot(quantities=T, main='COVID had an impact? (US)')
    # GHANA
    covid %>%
      filter(alpha2=='GH') %>%
      select(-alpha2, -coverage) %>%
      equals('Checked') %>%
      euler() %>% plot(quantities=T, main='COVID had an impact? (GH)')
    # CANADA
    covid %>%
      filter(alpha2=='CA') %>%
      select(-alpha2, -coverage) %>%
      equals('Checked') %>%
      euler() %>% plot(quantities=T, main='COVID had an impact? (CA)')
    # PHILIPPINES
    covid %>%
      filter(alpha2=='PH') %>%
      select(-alpha2, -coverage) %>%
      equals('Checked') %>%
      euler() %>% plot(quantities=T, main='COVID had an impact? (PH)')
    # GREAT-BRITAIN
    covid %>%
      filter(alpha2=='GB') %>%
      select(-alpha2, -coverage) %>%
      equals('Checked') %>%
      euler() %>% plot(quantities=T, main='COVID had an impact? (GB)')



# 2. COVID IMPACT HEATMAP ####
# ----------------------------
# a. make initial heatmap dataframe
heatmap_df <- data %>%
              filter(covid_pnta == 'Unchecked') %>% # remove pnta for covid question
              filter(alpha2 %in% top5_countries$alpha2) %>% # must be in top5 countries
              select(ID, alpha2, coverage, income_class, usd_household, matches('covid'), -covid_pnta)

# b. binarize covid-related answers
bin_mat <- heatmap_df %>%
           select(covid_no:covid_unsure) %>%
           equals('Checked') %>%
           multiply_by(1)

heatmap_df <- cbind(select(heatmap_df, ID:usd_household),
                    bin_mat) %>%
              as_tibble()

# c. remove rows with no covid response whatsoever
heatmap_df %<>% filter(rowSums(bin_mat) != 0)

# d. finalize heatmap binary matrix
bin_mat <- heatmap_df %>%
           column_to_rownames('ID') %>%
           select(covid_no:covid_unsure) %>%
           set_colnames(str_remove(colnames(.), 'covid_'))

# e. make annotation dataframes
annot_col_df <- data.frame('impact'=c('no', rep('yes', 5))) %>%
                set_rownames(colnames(covid_binary_matrix) %>% str_remove('covid_'))

annot_row_df <- heatmap_df %>%
                column_to_rownames('ID') %>%
                select(alpha2:usd_household) %>%
                rename('country'='alpha2',
                       'country_income'='income_class')

# f. make row colors
col_colors <- list('coverage'=c('none'='#E41A1C',
                                'partial'='#377EB8',
                                'full'='#4DAF4A',
                                'pnta'='white'),
                   'country'=c('US'='#1B9E77',
                               'GH'='#D95F02',
                               'CA'='#7570B3',
                               'PH'='#E7298A',
                               'GB'='#66A61E'))

# g. plot heatmap
pheatmap::pheatmap(bin_mat, cluster_rows=T, cluster_cols=T, cellwidth=20, legend=F,
                   clustering_method='ward.D', clustering_distance_rows='maximum',
                   show_rownames=F, breaks=c(-1, 0, 1), color=c('white', 'black'), annotation_colors=col_colors,
                   annotation_col=annot_col_df, annotation_row=annot_row_df)



# 3. COVID IMPACT BARPLOT ####
# ----------------------------
# a. make initial dataframe for barplot
bar_df <- heatmap_df %>%
          select(-ID, -coverage, -income_class, -usd_household)

# b. count total number of responses by country
bar_total <- count(bar_df, alpha2) %>% rename('total'='n')

# c. summarize yes/no answers
yes_no_df <- bar_df %>% group_by(alpha2, covid_no) %>% summarize('n'=n())
yes_no_df %<>% left_join(bar_total, by='alpha2') %>% mutate('ratio'=n/total, 'covid_no'=ifelse(covid_no==1, 'No', 'Yes')) %>% rename('impact'='covid_no')
yes_no_df %<>% mutate('alpha2'=factor(alpha2, levels=c('US', 'GH', 'CA', 'PH', 'GB'))) %>% mutate('impact'=factor(impact, levels=c('No', 'Yes')))

# d. plot main barplot
ggplot(yes_no_df, aes(fill=impact, y=ratio*100, x=alpha2)) +
       geom_col(col='black', lwd=.3, alpha=.5) +
       geom_text(aes(label=n), position=position_stack(vjust=.5)) +
       geom_hline(yintercept=50, lty=2) +
       scale_fill_brewer(palette='Set1', name='', direction=-1) +
       cowplot::theme_cowplot() + theme(aspect.ratio=1) +
       scale_y_continuous(expand=c(0, 0, 0.04, 0)) +
       scale_x_discrete(expand=c(0, 0.5)) +
       labs(title='Impact of COVID-19 on T1D',
            x='Top5 most-represented countries',
            y='Ratio of responses [%]')

# e. make dataframe for 'yes' answers
impact_df <- bar_df %>%
             filter(covid_no==0) %>%
             select(-covid_no, -covid_unsure) %>%
             set_colnames(str_remove(colnames(.), 'covid_'))

impact_total <- count(impact_df, alpha2) %>% rename('total'='n')

# f. summarize 'yes' answers
impact_df %<>%
  melt(id.vars='alpha2', variable.name='impact') %>%
  filter(value!=0) %>%
  group_by(alpha2, impact) %>%
  summarize('n'=n()) %>%
  left_join(impact_total, by='alpha2') %>%
  mutate('ratio'=n/total,
         'alpha2'=factor(alpha2, levels=c('US', 'GH', 'CA', 'PH', 'GB'))) %>%
  rename('country'='alpha2')

# g. barplot for yes answers
impact_df %>%
  mutate('impact'=str_replace(impact, '_', ' ')) %>%
  ggplot(aes(x=impact, fill=country, y=ratio*100)) +
         geom_col(col='black', lwd=.3, position=position_dodge(), alpha=.75) +
         geom_text(aes(label=n), position=position_dodge(width=.9), vjust=-.5) +
         cowplot::theme_cowplot() + theme(aspect.ratio=1) +
         scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
         labs(title='Detail of COVID-19 impacts',
              y='Ratio of responses [%]',
              x='Impact of COVID-19') +
         scale_fill_brewer(palette='Dark2', name='Country')

# h. percentage of most represented COVID-19 impact (supply disrupt)
impact_df %>%
  ungroup() %>%
  filter(impact=='supply_disrupt') %>%
  summarize('mean_pc'=mean(ratio))

impact_df %>%
  ungroup() %>%
  filter(impact=='supply_disrupt') %>%
  summarize('tot_N'=sum(n))

# i. percentage of people reporting both supply disrupt & price up
covid %>%
  filter(alpha2 %in% top5_countries$alpha2) %>%
  select(covid_supply_disrupt, covid_price_up) %>%
  mutate('covid_supply_disrupt'=ifelse(covid_supply_disrupt=='Checked', T, F),
         'covid_price_up'=ifelse(covid_price_up=='Checked', T, F)) %$%
  table('DISRUPT'=covid_supply_disrupt, 'UP'=covid_price_up)
