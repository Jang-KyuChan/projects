# mask_data_setting
mask = ceiling(pmax(10^(-log10(runif(length(data$id))))))
unif = runif(length(data$id), 0, 1)

mask_data = data.frame(mask, unif) %>%
  mutate(mask = pmin(200, mask)) %>%
  mutate(left_mask = pmax(1, pmin(mask - 1, as.integer(mask * unif)))) %>%
  mutate(right_mask = mask - left_mask) %>%
  select(-c(mask, unif))
mask_data
write.csv(mask_data, 'mask_data.csv')

# EDA
interpolated_data %>%
  mutate(cum_hole = cumsum(is.na(Value))) %>%
  filter(cum_hole == lead(cum_hole)) %>%
  filter(is.na(Value)) %>%
  mutate(hole = ifelse(is.na(lag(cum_hole)), cum_hole, cum_hole - lag(cum_hole))) %>%
  select(hole) %>%
  group_by(hole < 10, hole < 100) %>% 
  summarize(n = n())