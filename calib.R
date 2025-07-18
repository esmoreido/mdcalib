library(ggplot2)
library(lubridate)
library(tidyverse)
library(hydroGOF)

setwd('c:/Users/morey/Documents/R/mdcalib/')
df <- read.csv('Hdrs.csv', 
               check.names = F, stringsAsFactors = F, na.strings = '-99.000')
df <- df %>%
  mutate(Date = as.Date(strptime(as.character(Date), format = '%Y%m%d'))) %>%
  pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
  mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
         calc = sub('.*(?=.$)', '', var, perl=T)) %>%
  select(Date, post, calc, val)

# nse_home <- function(sim, obs){
#   nse <- 1 - ((sum((obs - sim) ^ 2, na.rm = T)) / (sum((obs - mean(obs, na.rm = T)) ^ 2, na.rm = T)))
#   return(nse)
# }

# df %>% 
#   filter(year(Date) > 1966 & year(Date) < 2004) %>%
#   summarise(nse_home(obs = `6473_Qm`, sim = `6473_Qs`))

nse_df <- df %>%
  # filter(year(Date) > 1966 & year(Date) <= 2004) %>%
    pivot_wider(id_cols = c(Date, post), 
                names_from = calc, values_from = val) %>%
    group_by(post) %>%
    summarise(nse = NSE(obs = m, sim = s), 
              rmse = rmse(m, s),
              r2 = cor(m, s, use = 'complete.obs') ^ 2 ) %>%
    mutate(calc = 'm')

count_df <- df %>%
  # filter(year(Date) > 1966 & year(Date) <= 2004) %>%
    group_by(post, calc) %>% 
    filter(!is.na(val)) %>%
    summarise(N = n(),
              mean = mean(val, na.rm = T),
              var = var(val, na.rm = T)) %>%
    left_join(nse_df, by = c('post', 'calc')) %>%
    arrange(calc, var) %>%
    mutate_if(is.numeric, ~round(., 3))
sel <- c(6487, 6497, 6491, 6490, 6479, 6456, 6473)
nsew <- count_df %>%
    filter(calc == 'm' & post %in% sel) %>%
    ungroup() %>%
    summarise(nse = sum(nse * var, na.rm = T) / 
                sum(var, na.rm = T)) %>%
    pull(nse)

input_df <- df %>%
  filter(year(Date) > 1966 & year(Date) <= 2004 & post %in% sel) %>%
  pivot_wider(id_cols = 'Date', names_from = c('post', 'calc'),  values_from = 'val') %>%
  na.exclude()

NSE(sim = input_df$`6473_s`, obs = input_df$`6473_m`)

dense_nse_df <- input_df %>%
  pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
  mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
         calc = sub('.*(?=.$)', '', var, perl=T)) %>%
  pivot_wider(id_cols = c(Date, post), 
              names_from = calc, values_from = val) %>%
  group_by(post) %>%
  summarise(nse = NSE(m, s)) %>%
  mutate(calc = 'm')
  
dense_df_stat <- input_df %>%
  pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
  mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
         calc = sub('.*(?=.$)', '', var, perl=T)) %>%
  select(Date, post, calc, val) %>%
  group_by(post, calc) %>% 
  filter(!is.na(val)) %>%
  summarise(N = n(),
            mean = mean(val, na.rm = T),
            var = var(val, na.rm = T)) %>%
  left_join(dense_nse_df, by = c('post', 'calc')) %>%
  arrange(calc, var) %>%
  mutate_if(is.numeric, ~round(., 3))

lm <- input_df %>%
  select(contains('_m')) %>%
  prcomp(scale = F)
summary(lm)$importance

loadings <- lm$rotation
loadings_t <- t(loadings)

varex <- summary(lm)$importance[1,]
varex
fact_matrix <- input_df %>%
  select(contains(c('_m')))
sim_matrix <- input_df %>%
  select(contains(c('_s')))

fact_m <- as.matrix(fact_matrix)  %*% loadings
sim_m <- as.matrix(sim_matrix)  %*% loadings

umatrix <- data.frame(fact_m, sim_m)

NSE(obs = umatrix$PC3, sim = umatrix$PC3.1)

clean_df <- umatrix %>%
  mutate_at(vars(!contains(c('PC1', 'PC2', 'PC1.1', 'PC1.2'))), function(x){x = 0}) 



clean_fact_matrix <- clean_df %>%
  select(!contains('.'))
clean_fact_matrix <- as.matrix(clean_fact_matrix) %*% loadings_t
clean_sim_matrix <- clean_df %>%
  select(contains('.'))
clean_sim <- data.frame(as.matrix(clean_sim_matrix) %*% loadings_t, check.names = F)
clean_sim$Date <- input_df$Date 
clean_sim <- relocate(clean_sim, Date)
  
ggplot(clean_sim, aes(x=Date)) + geom_line(aes(y=`6473_m`))
NSE(obs = input_df$`6487_m`, sim = clean_sim_matrix$`6487_m`)


clean_sim <- df %>%
  filter(calc == 's' & post %in% sel) %>%
  pivot_wider(id_cols = Date, names_from = post, values_from = val)

clean_sim <- data.frame(as.matrix(clean_sim[,-1]) %*% loadings) %>%
  mutate_at(vars(!contains(c('PC1', 'PC2'))), function(x){x = 0}) 

clean_sim <- as.data.frame(as.matrix(clean_sim) %*% t(loadings))
# меняем _m на _s в названии столбцов
clean_sim <- clean_sim %>%
  rename_with(.fn = ~paste0(gsub(pattern = "\\_.+", 
                                 replacement = "", 
                                 x = .x), '_s'))
clean_fact <- df %>%
  filter(calc == 'm' & post %in% sel) %>%
  pivot_wider(id_cols = Date, names_from = post, names_glue = '{post}_m', 
              values_from = val)

clean_sim <- cbind(clean_fact, clean_sim)

nse_clean <- clean_sim %>%
  pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
  mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
         calc = sub('.*(?=.$)', '', var, perl=T)) %>%
  select(Date, post, calc, val) %>%
  pivot_wider(id_cols = c(Date, post), 
              names_from = calc, values_from = val) %>%
  group_by(post) %>%
  summarise(nse = NSE(m, s), 
            rmse = rmse(m, s),
            r2 = cor(m, s, use = 'complete.obs') ^ 2 ) %>%
  mutate(calc = 'm')

count_clean <- clean_sim %>%
  pivot_longer(!Date, names_to = 'var', values_to = 'val') %>%
  mutate(post = gsub(pattern = "\\_.+", replacement = "", x = var),
         calc = sub('.*(?=.$)', '', var, perl=T)) %>%
  select(Date, post, calc, val) %>%
  group_by(post, calc) %>% 
  filter(!is.na(val)) %>%
  summarise(N = n(),
            mean = mean(val, na.rm = T),
            var = var(val, na.rm = T)) %>%
  left_join(nse_clean, by = c('post', 'calc')) %>%
  arrange(calc, var) %>%
  mutate_if(is.numeric, ~round(., 3))

nsew_clean <- count_clean %>%
  filter(calc == 'm') %>%
  ungroup() %>%
  summarise(nse = sum(nse * var, na.rm = T) / 
              sum(var, na.rm = T)) %>%
  pull(nse)


# режимный NSE ----
df <- df %>%
  select(c(Date, contains('5115')))
df <- df %>%
  rename('qfact' = '5115_Qm', 'qmod' = '5115_Qs')

writexl::write_xlsx(df, '5115.xlsx')
df %>%
  ggplot(aes(x=Date)) +
  geom_line(aes(y=qfact, col='M')) + 
  geom_line(aes(y=qmod, col='S')) +
  facet_wrap(year(Date)~., scales = 'free_x')

df <- df %>%
  group_by(yday = yday(Date)) %>%
  mutate(qreg = mean(qfact, na.rm = T)) %>%
  ungroup()

df %>%
  ggplot(aes(x=yday)) +
  geom_line(aes(y=qfact, col='M')) + 
  geom_line(aes(y=qmod, col='S')) +
  geom_line(aes(y=qreg, col='R')) +
  facet_wrap(year(Date)~., scales = 'free_x')

df1 <- df %>%
  mutate(year = year(Date)) %>%
  filter(year == 2008)

df2 <- df1 %>%
  arrange(desc(qfact), desc(qmod), .by_group=T)

df %>%
  mutate(n = row_number(),
         p = n / (n() + 1))

df1 <- apply(df, 2, sort, na.last = TRUE)


df %>%
  ggplot(aes(x=p)) +
  geom_line(aes(y=`5115_Qm`, col='M')) + 
  geom_line(aes(y=`5115_Qs`, col='S')) +
  geom_line(aes(y=qreg, col='R')) +
  facet_wrap(year(Date)~., scales = 'free')
  
nse <- df %>%
  group_by(year) %>%
  summarise(enum = mean((`5115_Qs` - `5115_Qm`) ^ 2),
            delim = mean((qreg - `5115_Qm`) ^ 2), 
            nse = NSE(obs = `5115_Qm`, sim = `5115_Qs`),
            nse_r = 1 - (enum / delim))

