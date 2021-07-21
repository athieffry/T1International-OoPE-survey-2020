#### T1International Survey 2020: 1. Countries, Healthcare coverage, and Out-of-Pocket Expenses (OoPEs)
#### Axel Thieffry - April 2021
library(tidyverse)
library(tidylog)
library(magrittr)
library(reshape2)
library(factoextra)
library(RColorBrewer)
'h' <- head
'l' <- length
'rename' <- dplyr::rename


# 0. READ DATA ####
# -----------------
data <- readRDS('1b - Clean Data/cleaned_data_04_21_2021.rds')
assertthat::assert_that(nrow(data) == 1066)



# 1. GET TOP 5 COUNTRIES ####
# ---------------------------
# a. number of represented countries
n_countries <- data$country %>% na.omit() %>% n_distinct()

# b. plot number of answers per country
count(data, country, alpha2) %>%
  arrange(desc(n)) %>%
  filter(n > 3) %>%
  unite('country', c('country', 'alpha2'), sep=' - ') %>%
  mutate('country'=fct_reorder(country, n)) %>%
  ggplot(aes(x=country, y=n, fill=country=='NA - NA')) +
         geom_col(col='black', lwd=.3) +
         geom_text(aes(label=n), hjust=-.3, size=3.5) +
         coord_flip() +
         cowplot::theme_cowplot() + theme(legend.position='none') +
         scale_y_continuous(expand=c(0, 0, 0.075, 0)) +
         scale_fill_brewer(palette='Dark2') +
         labs(title='Responses by country',
              x=paste0('Country (N=', n_countries, ')'),
              y='Number of responses (N=1,066)',
              caption='Countries with at least 3 responses')

  # countries not in Figure 1A
  count(data, country, alpha2) %>%
    filter(n < 4)

# c. make top 5 most represented countries
if(FALSE){
          top5_countries <- count(data, country, alpha2) %>%
                            arrange(desc(n)) %>%
                            na.omit() %>%
                            head(5) %>%
                            mutate('alpha2'=fct_reorder(alpha2, n, .desc=T)) %>%
                            rename('total'='n')
          saveRDS(top5_countries, '1b - Clean Data/top5_countries.rds')
          }

top5_countries <- readRDS('1b - Clean Data/top5_countries.rds')



# 2. HEALTCARE COVERAGE IN TOP 5 MOST REPRESENTED COUNTRIES  ####
# ---------------------------------------------------------------
# a. check values
table(data$coverage, useNA='ifany')

# b. make colors for coverage types
coverage_levels <- factor(c('none', 'partial', 'full'))
coverage_colors <- c(brewer.pal(name='Set1', n=3))

# c. summarize coverage types per country
cov_df <- data %>%
          filter(alpha2 %in% top5_countries$alpha2) %>% # country must be in top5
          filter(coverage %in% c('none', 'partial', 'full')) %>% # coverage must be specified (not NA, not pnta)
          group_by(alpha2, coverage) %>%
          summarize('n'=n()) %>%
          ungroup()

cov_df %>% select(alpha2, coverage, n) %>% pivot_wider(values_from='n', names_from='coverage')

# d. add country-totals & calculate ratios
cov_df %<>% left_join(summarize(group_by(cov_df, alpha2) ,'tot_cov_ans'=sum(n)), by='alpha2')
cov_df %<>% mutate('pc'=n/tot_cov_ans)
cov_df %<>% mutate('alpha2'=fct_reorder(alpha2, tot_cov_ans, .desc=T),
                   'coverage'=factor(coverage, levels=coverage_levels))

# e. plot
ggplot(cov_df, aes(x=alpha2, fill=coverage, y=pc*100)) +
       geom_col(lwd=.3, col='black', alpha=.85) +
       geom_text(aes(label=n), position=position_stack(vjust=.5)) +
       scale_fill_manual(values=coverage_colors, name='Healthcare\ncoverage') +
       cowplot::theme_cowplot() + theme(aspect.ratio=1.5) +
       scale_y_continuous(expand=c(0, 0)) +
       labs(x='Country code [alpha-2]', y='Ratio of answers [%]', title='Healthcare coverage\nin top 5 most represented countries')

cov_df %>%
  filter(alpha2=='US')



# 3. TOTAL MONTHLY EXPENSES ####
# ------------------------------
# a. overall monthly expenses in top5 countries
data %>%
  filter(alpha2 %in% top5_countries$alpha2) %>%
  select(ID, usd_sum, alpha2, coverage) %>%
  mutate('coverage'=factor(coverage, levels=c('none', 'partial', 'full'))) %>%
  ggplot(aes(x=usd_sum+1, col=coverage)) +
         geom_density() +
         scale_x_log10() +
         facet_wrap(~alpha2, ncol=1, scales='free') +
         cowplot::theme_cowplot() + theme(aspect.ratio=.4) +
         labs(title='Monthly expenses in top5 countries', x='Total monthly out-of-pocket expenses\n(USD, log-scale, pseudocount: +1)') +
         scale_color_brewer(palette='Set1')

# b. correlation between monthly expenses and household income?
df_3b <- data %>%
         filter(coverage %in% c('none', 'partial', 'full')) %>%
         filter(alpha2 %in% top5_countries$alpha2) %>%
         mutate('coverage'=factor(coverage, levels=c('none', 'partial', 'full')))

p_3b <- ggplot(df_3b, aes(x=usd_sum+1, usd_household+1, col=coverage)) +
               geom_point(alpha=.5) +
               geom_smooth(method='lm', se=F) +
               scale_x_log10() + scale_y_log10() +
               ggpubr::stat_cor(label.y=c(.1, .3, .5), method='spearman', na.rm=T) +
               cowplot::theme_cowplot() +
               theme(aspect.ratio=1, legend.position='bottom') +
               labs(title='Correlation between OoPEs & income\nin Top5 countries',
                    x='OoPEs [monthly log(USD + 1)]',
                    y='Household income [monthly log(USD + 1)]',
                    caption="(Correlation: Spearman's R)") +
               scale_color_brewer(palette='Set1')

ggExtra::ggMarginal(p_3b, type='density', groupColour=T, groupFill=T, size=10, alpha=.2)

  # get N
  df_3b %>%
    select(usd_sum, usd_household, coverage) %>%
    filter(coverage=='partial') %>%
    nrow()



# 4. MAIN DRIVERS OF OoPEs ####
# -----------------------------
# a. household income distributions
data %>%
  filter(alpha2 %in% top5_countries$alpha2) %>%
  mutate('coverage'=factor(coverage, levels=c('none', 'partial', 'full'))) %>%
  ggplot(aes(x=usd_household, col=coverage)) +
         geom_density() +
         scale_x_log10() +
         facet_wrap(~alpha2, ncol=1, scales='free_y') +
         labs(title='Household income in Top5 countries',
              x='Monthly household income (USD, log-scale)') +
         scale_color_brewer(palette='Set1')

# b. make dataframe (sum costs across overarching categories)
df_4b <- data %>%
         filter(alpha2 %in% top5_countries$alpha2) %>%
         select(ID, alpha2, coverage, matches('usd')) %>%
         select(-usd_sum, -usd_household) %>%
         melt(id.vars=c('ID', 'coverage', 'alpha2'), value.name='usd') %>%
         separate('variable', c('foo', 'type'), sep='_') %>%
         select(-foo) %>%
         mutate('category'=case_when(type %in% c('short', 'long', 'mix', 'other') ~ 'insulin',
                                     type %in% c('pump', 'cgm') ~ 'devices',
                                     type == 'glucagon' ~  'glucagon',
                                     type %in% c('strip', 'ketstrip') ~ 'strips')) %>%
         mutate('coverage'=factor(coverage, levels=c('none', 'partial', 'full')),
                'alpha2'=factor(alpha2, levels=c('US', 'GH', 'CA', 'PH', 'GB')),
                'category'=factor(category, levels=c('insulin', 'devices', 'glucagon', 'strips'))) %>%
         na.omit() %>%
         group_by(ID, coverage, alpha2, category) %>%
         summarize('cat_sum_usd'=sum(usd)) %>%
         ungroup()

# c. overall OoPEs by category (insulins, devices & strips)
df_4b %>%
  mutate('category'=factor(category, levels=c('strips', 'glucagon', 'insulin', 'devices'))) %>%
  ggplot(aes(x=category, fill=category, y=cat_sum_usd+1)) +
         geom_violin(draw_quantiles=c(.25, .5, .75), trim=T, scale='count') +
         scale_y_log10() +
         cowplot::theme_cowplot() + theme(aspect.ratio=.3, legend.position='none') +
         scale_fill_brewer(palette='Set2') +
         coord_flip() +
         labs(x='', y='OoPEs (monthly USD +1, log-scale)')

    # calculate mean costs and CI
    Rmisc::group.CI(cat_sum_usd~category, data=df_4b) %>%
      column_to_rownames('category') %>%
      round(1) %>%
      select(3:1) %>%
      arrange(desc(cat_sum_usd.mean))

# d. OoPEs by category, healthcare coverage, and top 5 countries
df_4b %>%
  mutate('category'=factor(category, levels=c('devices', 'insulin', 'glucagon', 'strips')),
         'coverage'=factor(coverage, levels=c('full', 'partial', 'none'))) %>%
  ggplot(aes(x=cat_sum_usd+1, fill=coverage)) +
         geom_histogram(lwd=.2, col='black', binwidth=.2) +
         scale_x_log10() +
         cowplot::theme_cowplot() + theme(aspect.ratio=.65) +
         facet_grid(alpha2 ~ category, scales='free') +
         scale_fill_brewer(palette='Set1', name='Healthcare\ncoverage', direction=-1) +
         labs(title='OoPEs super detail', subtitle='in Top 5 countries',
              x='Monthly OoPEs [log(USD+1)]', y='Number of responses')

# e. OoPEs by healthcare coverage
df_4b %>%
  mutate('coverage'=factor(coverage, levels=c('none', 'full', 'partial'))) %>%
  ggplot(aes(x=cat_sum_usd+1, col=coverage)) +
         geom_density() +
         scale_x_log10() +
         scale_fill_brewer(palette='Set1', name='Healthcare\ncoverage') +
         cowplot::theme_cowplot() +
         theme(aspect.ratio=.4) +
         labs(x='Out-of-Pocket Expenses\n(monthly USD, pseudocount: +1, log-scale)',
              title='OoPEs by healthcare coverage type\nacross top 5 represe nted countries')

df_4b %>%
  group_by(coverage) %>%
  summarize('mean_usd'=mean(cat_sum_usd, na.rm=T),
            'sd_usd'=sd(cat_sum_usd, na.rm=T))

# f. OoPEs by top 5 country
df_4b %>%
  ggplot(aes(x=cat_sum_usd+1, col=alpha2)) +
         geom_density() +
         scale_x_log10() +
         scale_fill_brewer(palette='Set1', name='Healthcare\ncoverage') +
         cowplot::theme_cowplot() +
         theme(aspect.ratio=.4) +
         labs(x='Out-of-Pocket Expenses\n(monthly USD, pseudocount: +1, log-scale)',
              title='Global OoPEs in top 5\nmost represented countries')

df_4b %>%
  group_by(alpha2) %>%
  summarize('mean_usd'=mean(cat_sum_usd, na.rm=T),
            'sd_usd'=sd(cat_sum_usd, na.rm=T))
