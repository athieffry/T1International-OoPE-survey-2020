#### T1International Survey 2020: 3. Underuse/rationing analysis
#### Axel Thieffry - May 2021
library(tidyverse)
library(tidylog)
library(magrittr)
library(reshape2)
library(factoextra)
library(RColorBrewer)
library(chisq.posthoc.test)
'h' <- head
'l' <- length
'rename' <- dplyr::rename
'%!in%' <- Negate('%in%')



# 0. READ DATA ####
# -----------------
data <- readRDS('1b - Clean Data/cleaned_data_04_21_2021.rds')
assertthat::assert_that(nrow(data) == 1066)

top5_countries <- readRDS('1b - Clean Data/top5_countries.rds')



# 1. WORLWIDE ####
# ----------------
# a. Worldwide rationing percentages: overall view
df_1a <- data %>%
         select(ID, freq_ratio_not, freq_test_not) %>%
         melt(id.vars='ID') %>%
         na.omit() %>%
         select(-ID) %>%
         group_by(variable, value) %>%
         summarize('n'=n()) %>%
         filter(value != 'Prefer not to answer')

df_1a_total <- group_by(df_1a, variable) %>% summarize('sum'=sum(n))

df_1a %<>% left_join(df_1a_total, by='variable')
df_1a %<>% mutate('pc'=n/sum*100,
                  'value'=factor(value, levels=c('Never',
                                                 'At least once per year',
                                                 'At least once per month',
                                                 'At least once per week',
                                                 'Every day')))
df_1a %>%
   mutate('variable'=case_when(variable=='freq_ratio_not' ~ 'Ration or skip\ninsulin due to cost',
                               variable=='freq_test_not' ~  'Not testing BG')) %>%
   ggplot(aes(x=variable, y=pc, fill=value)) +
          geom_hline(yintercept=seq(12.5, 87.5, 12.5)) +
          geom_col(lwd=.3, col='black') +
          geom_text(aes(label=n), position=position_stack(vjust=.5)) +
          coord_flip() +
          labs(title='Worldwide rationing', x='', y='Percentage of respondents') +
          cowplot::theme_cowplot() +
          theme(aspect.ratio=.2) +
          scale_y_continuous(expand=c(0, 0)) +
          scale_fill_brewer(palette='Spectral', name='Rationing frequency', direction=-1)

# b. rationing per healthcare coverage
df_1b <- data %>%
         select(coverage, freq_ratio_not, freq_test_not) %>%
         melt(id.vars='coverage', variable.name='ratio') %>%
         na.omit() %>%
         filter(value != 'Prefer not to answer') %>%
         filter(coverage != 'pnta') %>%
         group_by(coverage, ratio, value) %>%
         summarize('n'=n()) %>%
         ungroup()

df_1b_total <- df_1b %>%
               group_by(coverage, ratio) %>%
               summarize('sum'=sum(n)) %>%
               ungroup()

df_1b %<>% left_join(df_1b_total, by=c('coverage', 'ratio')) %>%
           mutate('pc'=n/sum*100)

df_1b %>%
   mutate('ratio'=case_when(ratio=='freq_ratio_not' ~ 'Ration/skip\ninsulin (cost)',
                            ratio=='freq_test_not' ~  'Not testing\nBG'),
          'value'=factor(value, levels=c('Never',
                                         'At least once per year',
                                         'At least once per month',
                                         'At least once per week',
                                         'Every day')),
          'coverage'=factor(coverage, levels=c('none', 'partial', 'full'))) %>%
   ggplot(aes(x=coverage, y=pc, fill=value)) +
          geom_hline(yintercept=seq(12.5, 87.5, 12.5)) +
          geom_col(lwd=.3, col='black') +
          geom_text(aes(label=ifelse(pc > 3, n, '')), position=position_stack(vjust=.5)) +
          facet_grid(ratio~.) +
          coord_flip() +
          scale_fill_brewer(palette='Spectral', name='Rationing frequency', direction=-1) +
          cowplot::theme_cowplot() +
          theme(aspect.ratio=.3, strip.background=element_blank()) +
          scale_y_continuous(expand=c(0, 0)) +
          labs(y='Percentage of respondents', x='Healthcare coverage',
               title='Worldwide rationing per healthcare coverage')

# c. contingency table for healthcare coverage -vs- rationing
      ### RATION BG TESTING PER COVERAGE
      contable_bg <- df_1b %>%
                     filter(ratio=='freq_test_not') %>%
                     select(-ratio, -sum, -pc) %>%
                     mutate('value'=case_when(value=='Never' ~ 'No',
                                              TRUE ~ 'Yes')) %>%
                     group_by(coverage, value) %>%
                     summarize('sum'=sum(n)) %>%
                     ungroup() %>%
                     pivot_wider(names_from='coverage', values_from='sum') %>%
                     column_to_rownames('value') %>%
                     select(none, partial, full) %>%
                     as.matrix()

      chisq.test(contable_bg) # p-value: 1.163e-12
      rcompanion::cramerV(contable_bg, verbose=T, bias.correct=T) # Cramer's V: 0.2721 (corrected)
      chisq.posthoc.test(contable_bg, method='bonferroni', round=1)

      ### RATION INSULIN PER COVERAGE
      contable_insulin <- df_1b %>%
                          filter(ratio=='freq_ratio_not') %>%
                          select(-ratio, -sum, -pc) %>%
                          mutate('value'=case_when(value=='Never' ~ 'No',
                                                   TRUE ~ 'Yes')) %>%
                          group_by(coverage, value) %>%
                          summarize('sum'=sum(n)) %>%
                          ungroup() %>%
                          pivot_wider(names_from='coverage', values_from='sum') %>%
                          column_to_rownames('value') %>%
                          select(none, partial, full) %>%
                          as.matrix()

      chisq.test(contable_insulin) # p-value: 4.885e-07
      rcompanion::cramerV(contable_insulin, verbose=T, bias.correct=T) # Cramer's V: 0.1869 (corrected)
      chisq.posthoc.test(contable_insulin, method='bonferroni', round=1)


# d. rationing per country income class
df_1d <- data %>%
         select(income_class, freq_ratio_not, freq_test_not) %>%
         melt(id.vars='income_class', variable.name='ratio') %>%
         na.omit() %>%
         filter(value != 'Prefer not to answer') %>%
         group_by_all() %>%
         summarize('n'=n()) %>%
         ungroup()

df_1d_total <- df_1d %>%
               group_by(income_class, ratio) %>%
               summarise('tot'=sum(n))

df_1d %<>% left_join(df_1d_total, by=c('income_class', 'ratio')) %>%
           mutate('pc'=n/tot*100) %>%
           mutate('ratio'=case_when(ratio=='freq_ratio_not' ~ 'Ration/skip insulin (cost)',
                                    ratio=='freq_test_not' ~  'Not testing BG'),
                  'income_class'=factor(income_class, levels=c('Low', 'Middle', 'High')),
                  'value'=factor(value, levels=c('Never',
                                                 'At least once per year',
                                                 'At least once per month',
                                                 'At least once per week',
                                                 'Every day')))

ggplot(df_1d, aes(x=income_class, y=pc, fill=value)) +
       geom_col(lwd=.3, col='black') +
       geom_text(aes(label=ifelse(pc > 7, n, '')), position=position_stack(vjust=.5)) +
       facet_grid(ratio~.) +
       cowplot::theme_cowplot() + theme(aspect.ratio=.3, axis.text.x=element_text(angle=45, hjust=1)) +
       scale_y_continuous(expand=c(0, 0)) +
       coord_flip() +
       scale_fill_brewer(palette='Spectral', direction=-1, name='Rationing frequency') +
       labs(x='Country income level', y='Percentage of respondents', title='Rationing per country income level')

      ### RATION BG TESTING PER COUNTRY INCOME LEVEL
      contable2_bg <- df_1d %>%
                      filter(ratio=='Not testing BG') %>%
                      select(-ratio, -tot, -pc) %>%
                      mutate('value'=case_when(value=='Never' ~ 'No',
                                               TRUE ~ 'Yes')) %>%
                      group_by(income_class, value) %>%
                      summarize('sum'=sum(n)) %>%
                      ungroup() %>%
                      pivot_wider(names_from='income_class', values_from='sum') %>%
                      column_to_rownames('value') %>%
                      select(Low, Middle, High) %>%
                      as.matrix()

      chisq.test(contable2_bg) # p-value: 2.227e-14
      rcompanion::cramerV(contable2_bg, verbose=T, bias.correct=T) # Cramer's V: 0.2943 (corrected)
      chisq.posthoc.test(contable2_bg, method='bonferroni', round=1)

      ### RATION INSULIN PER COUNTRY INCOME LEVEL
      contable2_insulin <- df_1d %>%
                           filter(ratio=='Ration/skip insulin (cost)') %>%
                           select(-ratio, -tot, -pc) %>%
                           mutate('value'=case_when(value=='Never' ~ 'No',
                                                    TRUE ~ 'Yes')) %>%
                           group_by(income_class, value) %>%
                           summarize('sum'=sum(n)) %>%
                           ungroup() %>%
                           pivot_wider(names_from='income_class', values_from='sum') %>%
                           column_to_rownames('value') %>%
                           select(Low, Middle, High) %>%
                           as.matrix()

      chisq.test(contable2_insulin) # p-value: 0.0037
      rcompanion::cramerV(contable2_insulin, verbose=T, bias.correct=T) # Cramer's V: 0.1102 (corrected)
      chisq.posthoc.test(contable2_insulin, method='bonferroni', round=1)



# 2. TOP 5 COUNTRIES ####
# -----------------------
# a. overview
df_2a <- data %>%
         filter(alpha2 %in% top5_countries$alpha2) %>%
         select(ID, alpha2, freq_ratio_not, freq_test_not) %>%
         melt(id.vars=c('ID', 'alpha2')) %>%
         na.omit() %>%
         select(-ID) %>%
         group_by(alpha2, variable, value) %>%
         summarize('n'=n()) %>%
         filter(value != 'Prefer not to answer') %>%
         ungroup()

df_2a_total <- group_by(df_2a, alpha2, variable) %>% summarize('sum'=sum(n))

df_2a %<>% left_join(df_2a_total, by=c('alpha2', 'variable'))
df_2a %<>% mutate('pc'=n/sum*100,
                  'value'=factor(value, levels=c('Never',
                                                 'At least once per year',
                                                 'At least once per month',
                                                 'At least once per week',
                                                 'Every day')))
df_2a %<>% mutate('variable'=case_when(variable=='freq_ratio_not' ~ 'Ration insulin',
                                       variable=='freq_test_not' ~  'Ration testing BG'),
                  'alpha2'=factor(alpha2, levels=top5_countries$alpha2),
                  'group'=ifelse(value=='Never', 'no', 'yes'))

ggplot(df_2a, aes(x=interaction(group, variable), y=pc, fill=value)) +
       geom_hline(yintercept=seq(25, 100, 25), col='grey90') +
       geom_col(lwd=.3, col='black') +
       geom_text(aes(label=ifelse(pc > 6, n, NA)), position=position_stack(vjust=.5)) +
       facet_wrap(~alpha2, nrow=1) +
       labs(title='Rationing in Top5 countries', x='', y='Percentage of respondents') +
       cowplot::theme_cowplot() +
       theme(aspect.ratio=3, axis.text.x=element_text(angle=90, hjust=1)) +
       scale_y_continuous(expand=c(0, 0, .05, 0)) +
       scale_fill_brewer(palette='Spectral', name='Rationing frequency', direction=-1)

# stats
df_2a %>%
   filter(alpha2=='US', variable=='Ration insulin') %>%
   group_by(group) %>%
   summarize('total_n'=sum(n))

df_2a %>%
   filter(alpha2 %in% c('GH', 'PH')) %>%
   filter(group=='yes') %>%
   group_by(alpha2, variable) %>%
   summarize('sum_n'=sum(n),
             'sum_pc'=sum(pc))

df_2a %>%
   select(-value) %>%
   group_by(alpha2, variable, group) %>%
   summarize('n'=sum(n),
             'sum'=sum(sum),
             'pc'=sum(pc)) %>%
   filter(alpha2=='PH')

df_2a %>% filter(alpha2=='GH')