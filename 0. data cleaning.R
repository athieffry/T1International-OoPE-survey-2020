# T1International 2020 Survey : 0. DATA CLEANING
# Axel Thieffry - February 2021
library(tidyverse)
library(tidylog)
library(magrittr)
library(ggplot2)
library(reshape2)
library(forcats)
library(janitor)
library(RColorBrewer)
library(scales)
library(readxl)
library(WriteXLS)
library(lubridate)
library(eulerr)
'h' <- head
'l' <- length


# 1. READ IN RAW DATA WITH LABELS ####
# ------------------------------------
# a. read raw data
data_column_types <- c(rep('guess', 2), 'date', rep('guess', 116)) # vector of column type (to not lose timestamp from Excel format)
data <- read_xlsx('1a - Raw Data/survey/T1D data pulled 2-9-21 Labels.xlsx', trim_ws=T, col_types=data_column_types)
dim(data) # 1,080 x 119

# b.remove entries without provided consent
count(data, Consent) # 3 'not interested', 11 'NA', all the rest provided consent (1,066)
data %<>% filter(Consent == 'I have read the above information and I agree to participate') %>% select(-Consent)
dim(data) # 1,066 x 118


# c. get country alpha codes (2, 3) and region
if(FALSE) {
          country_codes <- readxl::read_xlsx('1a - Raw Data/country_data/country_codes.xlsx', col_names=T, trim_ws=T, col_types='text') # source: https://www.iban.com/country-codes
          regions <- readxl::read_xlsx('1a - Raw Data/country_data/country_zones.xlsx', col_names=T, trim_ws=T, col_types='text') %>%
                     separate('zone_name', 'region', sep='/', extra='drop') %>%
                     unique()
          country_income <- read_xls('1a - Raw Data/country_data/country_income_class.xls', col_names=T, trim_ws=T) %>%
                            mutate('income_class'=case_when(income_class=='High income' ~ 'High',
                                                            income_class=='Low income' ~ 'Low',
                                                            income_class %in% c('Lower middle income', 'Upper middle income') ~ 'Middle')) %>%
                            mutate('income_class'=factor(income_class, levels=c('Low', 'Middle', 'High'))) %>%
                            select(-country_name)

          country_infos <- left_join(country_codes, regions, by='alpha2') %>% na.omit() %>% arrange(country_name)
          country_infos %<>% left_join(country_income, by='alpha3')
          saveRDS(country_infos, '1a - Raw Data/country_data/country_data.rds')
          }

country_infos <- readRDS('1a - Raw Data/country_data/country_data.rds')



# 2. COLNAMES CLEANING & COUNTRY INFO ####
# ----------------------------------------
# a. remove empty columns & rows
data %<>% remove_empty(which=c('rows', 'cols')) # 5 columns removed
dim(data) # 1,066 x 113

# b. rename columns into script-friendly names (RDS was generated manually in Excel)
column_names_df <- read_xlsx('1a - Raw Data/survey/column names for R.xlsx')
column_names <- column_names_df$initial_name_column
names(column_names) <- column_names_df$renamed_column
data %<>% dplyr::rename(all_of(column_names))

# c. add country info
data %<>% left_join(country_infos, by=c('country'='country_name')) # NB: NA values are normal: some respondents didn't inform on their country
dim(data) # 1,066 x 117



# 3. RE-LEVEL & SANITY CHECK ALL ANSWERS ####
# -------------------------------------------
# a. connection to T1 diabetes
data %<>% mutate('T1con'=case_when(T1con == 'I am a medical professional completing survey on behalf of a specific patient with type 1 diabetes' ~ 'doc',
                                   T1con == 'I have type 1 diabetes' ~ 'patient',
                                   T1con == 'My child has type 1 diabetes' ~ 'mychild',
                                   T1con == 'My spouse/partner/significant other has type 1 diabetes' ~ 'betterhalf'))

nb_T1con_na <- data$T1con %>% is.na() %>% sum()
nb_T1con_notna <- data$T1con %>% na.omit() %>% l()

count(data, T1con) %>%
  na.omit() %>%
  mutate('T1con'=fct_reorder(T1con, n)) %>%
  ggplot(aes(x=T1con, y=n)) +
         geom_col(lwd=.2, col='black') +
         coord_flip() +
         geom_text(aes(label=n), hjust=-.3) +
         theme(aspect.ratio=.5) + scale_y_continuous(expand=c(0, 0, .1, 0)) +
         labs(title='', y=paste0('Nb. answers (N=', nb_T1con_notna,')'), caption=paste0('missing T1 connections: ', nb_T1con_na),
              x='Type of conection to T1D')

# b. gender
  # heatmap of missing values
  data %>%
    select(matches('gender')) %>%
    mutate_all(is.na) %>%
    not() %>%
    multiply_by(1) %>%
    pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, cellwidth=15, color=c('grey', 'navy'),
                       cutree_rows=5, treeheight_row=0, main='gender values', breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D2')

  # frequency of answers for doctors and patients
  bind_rows(data %>% select(gender_respondant) %>% na.omit() %>% mutate('set'='respondant') %>% rename('gender'='gender_respondant'),
            data %>% select(gender_patient) %>% na.omit() %>% mutate('set'='patient') %>% rename('gender'='gender_patient')) %>%
    count(gender, set) %>%
    ggplot(aes(x=gender, y=n, fill=set)) +
           geom_col(lwd=.2, col='black') +
           coord_flip() +
           geom_text(aes(label=n), position=position_stack(vjust=.5)) +
           theme(aspect.ratio=.75, legend.position='bottom') +
           scale_fill_brewer(palette='Set2', direction=-1, name='') +
           labs(title='Gender frequency', y='Number of answers', x='')

# c. country
n_distinct(data$country, na.rm=T) # 64 different countries

count(data, country) %>%
  arrange(desc(n)) %>%
  mutate('country'=fct_reorder(country, n)) %>%
  na.omit() %>%
  head(20) %>%
  ggplot(aes(x=country, y=n)) +
         geom_col(lwd=.3, col='black') +
         coord_flip() +
         geom_text(aes(label=n), hjust=-.3) +
         theme(aspect.ratio=.75, legend.position='bottom') +
         scale_fill_brewer(palette='Set2', direction=-1, name='') +
         scale_y_continuous(expand=c(0, 0, .1, 0)) +
         labs(title='Country frequency', x='', y='Number of answers',
              caption=paste0('Nb. missing answers:', sum(is.na(data$country))))

# d. health coverage
data %>% select(matches('coverage')) %>% colnames()
    # only the initial coverage question has answers!
    # remove all the coverage-related questions
    data %<>% select(-coverage_private_self, -coverage_private_work, -coverage_private_shared, -coverage_public, -coverage_prefnoans, -coverage_other)
    # re-code answers
    data %<>% mutate('coverage'=case_when(coverage == 'No, there is no coverage for any of my costs' ~ 'none',
                                          coverage == 'Prefer not to answer' ~ 'pnta',
                                          coverage == 'Yes, there is health care coverage for <b>all</b> of my costs (so I do not pay anything out of pocket)' ~ 'full',
                                          coverage == 'Yes, there is health care coverage for <b>some</b> of my costs' ~ 'partial'))
    # plot coverage answers
    count(data, coverage) %>%
    na.omit() %>%
    mutate('coverage'=fct_reorder(coverage, n)) %>%
    ggplot(aes(x=coverage, y=n)) +
           geom_col(lwd=.3, col='black') +
           coord_flip() +
           geom_text(aes(label=n), hjust=-.3) +
           theme(aspect.ratio=.75) +
           scale_fill_brewer(palette='Set2', direction=-1, name='') +
           scale_y_continuous(expand=c(0, 0, .1, 0)) +
           labs(title='Coverage frequency', x='', y='Number of answers',
                caption=paste0('Nb. missing answers:', sum(is.na(data$coverage))))

# e. pay help: re-level Unchecked/Checked to
data %>% select(matches('pay_help')) %>% colnames()
    # heatmap of pay_help values
    data %>%
      select(matches('pay_help'), -pay_help_other_desc) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D2', main='pay help')

    # venn diagram
    data %>%
      select(matches('pay_help'), -pay_help_other_desc) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      set_colnames(c('no', 'fam_friends', 'charity', 'donation', 'gov', 'pharma', 'other', 'no answer')) %>%
      euler(shape='ellipse') %>% plot(quantities=T)

# f. things done to pay OoPEs
data %>% select(matches('payOoPE')) %>% colnames()
    # heatmap of pay_help values
    data %>%
      select(matches('payOoPE'), -payOoPE_other_desc) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D2', main='pay OoPE')

    # venn diagram
    data %>%
      select(matches('payOoPE'), -payOoPE_other_desc) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      set_colnames(c('nothing', 'savings', 'borrow', 'asset', 'other', 'not tell')) %>%
      euler(shape='ellipse') %>% plot(quantities=T)

# g. COVID-19 impact
data %>% select(matches('covid')) %>% colnames()
    # heatmap of pay_help values
    data %>%
      select(matches('covid')) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D2', main='COVID')

    # venn diagram
    data %>%
      select(matches('covid')) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      set_colnames(c('none', 'supply_disrup', 'insulin_disrupt', 'priceUP', 'priceDOWN', 'unsure', 'not_tell')) %>%
      euler(shape='ellipse') %>% plot(quantities=T)

# h. insulin intake methods
data %>% select(matches('intake')) %>% colnames()
    # heatmap of pay_help values
    data %>%
      select(matches('intake')) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D2', main='insulin intake method')
    # venn diagram
    data %>%
      select(matches('intake')) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      set_colnames(c('vial', 'pump', 'pen', 'not_tell')) %>%
      euler(shape='ellipse') %>% plot(quantities=T)

# i. insulin types
data %>% select(matches('^insulin_')) %>% colnames()
    # heatmap of pay_help values
    data %>%
      select(matches('^insulin_'), -insulin_other_desc) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D', main='insulin types')
    # venn diagram for top 5 most used insulin types
    top5_insulin_types <- data %>%
                          select(matches('^insulin_'), -insulin_other_desc) %>%
                          equals('Checked') %>%
                          multiply_by(1) %>%
                          colSums() %>%
                          sort(decreasing=T) %>%
                          .[1:5] %>%
                          names()

    data %>%
      select(all_of(top5_insulin_types)) %>%
      equals('Checked') %>%
      multiply_by(1) %>%
      set_colnames(str_remove(colnames(.), 'insulin_')) %>%
      euler(shape='ellipse') %>% plot(quantities=T, main='Top 5 most used insulin types')

# j. nb of consumables per month
    df_k <- data %>% select(matches('^nb_'))
    # types of answers
    df_k %>% unlist() %>% table(useNA='ifany')
    # heatmap of missing values
    df_k %>%
      is.na() %>%
      not() %>%
      multiply_by(1) %>%
      pheatmap::pheatmap(cluster_rows=T, cluster_cols=F, show_rownames=F, color=c('grey', 'navy'), breaks=c(-1, 0, 1), legend_breaks=c(-.5, .5), legend_labels=c('NA' ,'has value'), clustering_method='ward.D', main='nb vials & pens')
    # venn diagram
    df_k[df_k=='Prefer not to answer'] <- NA
    df_k %>%
      is.na() %>%
      not() %>%
      multiply_by(1) %>%
      set_colnames(str_remove(colnames(.), 'nb_')) %>%
      as.data.frame() %>%
      euler(shape='ellipse') %>% plot(quantities=T, main='Vial & Pen usage')

# k. price of consumables per month
    # plot costs (except calculated sum of all + "others")
    data %>%
      select(matches('^usd_')) %>%
      select(-usd_sum, -usd_household) %>%
      melt(variable.name='expense', value.name='USD') %>%
      mutate('expense'=str_remove(expense, 'usd_'),
             'set'=case_when(expense %in% c('short', 'long', 'mix', 'other') ~ 'pens & vials',
                             expense %in% c('pump', 'strip', 'cgm', 'glucagon', 'ketstrip') ~ 'consumables'),
             'expense'=factor(expense, levels=c('short', 'long', 'mix', 'other', 'cgm', 'pump', 'strip', 'ketstrip', 'glucagon'))) %>%
      ggplot(aes(x=expense, y=USD, fill=set)) +
             geom_boxplot() +
             scale_y_log10(labels=comma) +
             cowplot::theme_cowplot() + theme(axis.text.x=element_text(angle=45, hjust=1), aspect.ratio=1.5) +
             labs(y='USD (monthly)', x='item', title='Monthly USD cost of consumables', subtitle='T1International.org')
    # do declared expenses add up with the calculated sum? => YES
    tibble('total_usd_calculated'=data %>% select(matches('^usd_'), -usd_sum, -usd_household) %>% rowSums(na.rm=T),
           'total_usd_redcap'=data$usd_sum) %>%
      ggplot(aes(x=total_usd_calculated, y=total_usd_redcap)) +
             geom_point() +
             geom_abline(intercept=0, slope=1, lty=2, col='red') +
             scale_x_log10() + scale_y_log10() +
             cowplot::theme_cowplot() + theme(aspect.ratio=1)

# l. insulin pumps
count(data, pump_brand)
count(data, pump_other)
  # put 'other pump brands' into 'pump_brand' & remove 'pump_other'
  transfer_idx <- which(!is.na(data$pump_other))
  data$pump_brand[transfer_idx] <- data$pump_other[which(!is.na(data$pump_other))]
  data %<>% select(-pump_other)
  rm(transfer_idx)
  # Top 5 Pump brands
  count(data, pump_brand) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    mutate('pump_brand'=fct_reorder(pump_brand, n)) %>%
    ggplot(aes(x=pump_brand, y=n)) +
           geom_col() + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
           labs(title='Top 5 pump brands', x='')

# m. test strips
count(data, strip_brand)
count(data, strip_other)
    # Top 5 (without 'Other')
    count(data, strip_brand) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    mutate('strip_brand'=fct_reorder(strip_brand, n)) %>%
    ggplot(aes(x=strip_brand, y=n)) +
           geom_col() + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
           labs(title='Top 5 strip brands', x='')

# n. CGMs
count(data, cgm)
count(data, cgm_other)
  # recode cgm & cgm_other
  data %<>% mutate('cgm'=case_when(cgm=='No' ~ 'No',
                                   cgm=='Prefer not to answer' ~ 'pnta',
                                   cgm=='Yes, Dexcom' ~ 'Dexcom',
                                   cgm=='Yes, Freestyle Libre' ~ 'Freestyle Libre',
                                   cgm=='Yes, Medtronic' ~ 'Medtronic',
                                   cgm=='Yes, other' ~ 'Other'),
                   'cgm_other'=case_when(cgm_other %in% c('Abbott Freestyle', 'Freestyle Libre + Miaomiao') ~ 'Freestyle Libre',
                                         cgm_other %in% c('Accu check', 'Accu check active', 'Accu chek', 'Accu-Chek') ~ 'Accu-Check',
                                         cgm_other %in% c('Eversense', 'Eversense XL', 'Eversense XL Sensor') ~ 'Eversense',
                                         TRUE ~ cgm_other))
  # transfer non-NA cgm_other to cgm that responded other
  transfer_idx <- which(data$cgm=='Other' & !is.na(data$cgm_other))
  data$cgm[transfer_idx] <- data$cgm_other[transfer_idx]
  data %<>% select(-cgm_other)
  count(data, cgm)
  rm(transfer_idx)
  # top 5 CGM brands
  count(data, cgm) %>%
    arrange(desc(n)) %>%
    head(5) %>%
    mutate('cgm'=fct_reorder(cgm, n)) %>%
    ggplot(aes(x=cgm, y=n)) +
           geom_col() + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
           labs(title='Top 5 CGM brands', x='')

# o. frequency of rationing/underuse
  # insulin due to cost
  count(data, freq_ratio_not) %>%
    mutate('freq_ratio_not'=fct_reorder(freq_ratio_not, n)) %>%
    ggplot(aes(x=freq_ratio_not, y=n)) +
           geom_col() + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
           labs(title='Frequency of insulin rationing', x='')

  # test BG
  count(data, freq_test_not) %>%
    mutate('freq_test_not'=fct_reorder(freq_test_not, n)) %>%
    ggplot(aes(x=freq_test_not, y=n)) +
           geom_col() + geom_text(aes(label=n), hjust=-.2) +
           coord_flip() +
           cowplot::theme_cowplot() + theme(aspect.ratio=.5) +
           scale_y_continuous(expand=c(0, 0, 0.1, 0)) +
           labs(title='Frequency of BG test rationing', x='')

# p. glucagon kits
df_m <- data %>% select(matches('^glucagon'), -glucagon_no_other_desc)
    # histogram of pnta, NA, yes, no
    count(df_m, glucagon) %>%
      na.omit() %>%
      filter(glucagon != 'Prefer not to answer') %>%
      separate('glucagon', c('glucagon', 'sub'), sep=', ') %>%
      mutate('sub'=ifelse(is.na(sub), glucagon, sub)) %>%
      mutate('glucagon'=factor(glucagon, levels=c('Yes', 'No')),
             'sub'=factor(sub, levels=c('nasal spray (Baqsimi)', 'Glucagon'))) %>%
      ggplot(aes(x=glucagon, y=n, fill=sub)) +
             geom_col(lwd=.3, col='black') +
             geom_text(aes(label=n), position=position_stack(vjust=.5)) +
             cowplot::theme_cowplot() + theme(aspect.ratio=3, legend.position='bottom') +
             scale_y_continuous(expand=c(0, 0)) +
             labs(caption='Prefer not to answer: N=8', title='Glucagon kit', y='N')
    # venn-diagram of why NO glucagon kit
    filter(df_m, glucagon=='No') %>%
      select(-glucagon) %>%
      mutate_all(~ case_when(.=='Checked' ~ 1,
                             .=='Unchecked'~ 0)) %>%
      set_colnames(c('too_expensive', 'not_available', 'not_exist', 'not_know_usage', 'not_want_keep', 'no_need', 'other_reason', 'pnta')) %>%
      euler(shape='ellipse') %>% plot(quantities=T, main='Reasons for no glucagon kit')

# q. ketone kits
df_n <- data %>% select(matches('^ketstrip'), -ketstrip_no_other_desc)
    # histogram of pnta, NA, yes, no
    count(df_n, ketstrip) %>%
      na.omit() %>%
      filter(ketstrip != 'Prefer not to answer') %>%
      separate('ketstrip', c('ketstrip', 'type'), sep=', ') %>%
      mutate('type'=ifelse(is.na(type), ketstrip, type)) %>%
      mutate('ketstrip'=factor(ketstrip, levels=c('Yes', 'No')),
             'type'=factor(type, levels=c('blood strips', 'urine strips'))) %>%
      ggplot(aes(x=ketstrip, y=n, fill=type)) +
             geom_col(lwd=.3, col='black') +
             geom_text(aes(label=n), position=position_stack(vjust=.5)) +
             cowplot::theme_cowplot() + theme(aspect.ratio=3, legend.position='bottom') +
             scale_y_continuous(expand=c(0, 0)) +
             labs(caption='Prefer not to answer: N=9', title='Ketone strips', y='N')
    # venn-diagram of why NO ketone kit
    filter(df_n, ketstrip=='No') %>%
      select(-ketstrip) %>%
      mutate_all(~ case_when(.=='Checked' ~ 1,
                             .=='Unchecked'~ 0)) %>%
      set_colnames(c('too_expensive', 'not_available', 'not_exist', 'not_know_usage', 'not_want_keep', 'no_need', 'other_reason', 'pnta')) %>%
      euler(shape='ellipse') %>% plot(quantities=T, main='Reasons for no ketone strip')



# 4. SAVE CLEANED DATA ####
# -------------------------
# check nrows
nrow(data)==1066
# save
saveRDS(data, '1b - Clean Data/cleaned_data_04_21_2021.rds')

