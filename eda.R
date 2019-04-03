source("refine.R")

## eswiki pv from Mexico by year
p <- pageviews %>%
  filter(
    country_code == 'MX',
    project == 'es.wikipedia',
    date < as.Date("2019-03-01")
  ) %>%
  group_by(year, month = lubridate::month(date)) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x=month, y=pageviews, colour=factor(year))) +
  geom_line(size=1.2) +
  scale_x_continuous(name = "Month", breaks = 1:12, labels = 1:12) +
  scale_y_continuous(labels=polloi::compress, name = "Pageviews") +
  scale_color_brewer("Year", palette = "Set1") +
  ggtitle("Spanish Wikipedia monthly pageviews from Mexico") +
  wmf::theme_min(base_size = 15)
ggsave("eswiki_pv.png", p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)

## eswiki pv from Mexico, compare to other wikis' traffic from Mexico
p1 <- pageviews %>%
  filter(
    country_code == 'MX',
    referer_class != 'unknown',
    date >= as.Date('2018-11-01'),
    date < as.Date('2019-02-28')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'es.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Pageviews from Mexico, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018",
       caption = "Other wikis include top 5 projects in Mexico with the most pageviews"
       )
ggsave("pv_mexico_bywiki.png", plot = p1, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)

## eswiki pv from Mexico compare to other countries
p1 <- pageviews %>%
  filter(
    project == 'es.wikipedia',
    referer_class != 'unknown',
    date >= as.Date('2018-11-01'),
    date < as.Date('2019-02-28')
  ) %>%
  mutate(country = ifelse(country_code %in% c('MX', 'ES'), country_code, 'Other countries')) %>%
  group_by(country, access_method, referer_class, date) %>%
  summarize(pageviews = sum(pageviews)) %>%
  ggplot(aes(x = date, y = pageviews, color = referer_class)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Pageviews", labels = polloi::compress) +
  scale_color_brewer("Referrer Class", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Spanish Wikipedia pageviews by countries, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018"
       )
ggsave("eswiki_pv_bycountries.png", plot = p1, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)


## eswiki ud from Mexico by year (grid by uniques_underestimate and uniques_offset)
p <- unique_devices %>%
  filter(
    country_code == 'MX',
    project == 'es.wikipedia',
    type == 'total',
    date < as.Date("2019-03-01")
  ) %>%
  group_by(year, month=lubridate::month(date), type) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x=month, y=uniques, colour=factor(year))) +
  geom_line(size=1.2) +
  scale_x_continuous(name = "Month", breaks = 1:12) +
  scale_y_continuous(labels=polloi::compress, name = "Unique Devices", limits = c(5e7, NA)) +
  scale_color_brewer("Year", palette = "Set1") +
  labs(title = "Spanish Wikipedia monthly unique devices from Mexico") +
  wmf::theme_min(base_size = 15)
ggsave("eswiki_mexico_ud.png", p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)

## eswiki ud from Mexico, compare to other wikis' ud from Mexico (grid by uniques_underestimate and uniques_offset)
p1 <- unique_devices %>%
  filter(
    country_code == 'MX',
    type != 'total',
    date >= as.Date('2018-11-01'),
    date < as.Date('2019-02-28')
  ) %>%
  mutate(project = case_when(
      project %in% c('en.wikipedia', 'es.wikipedia') ~ project,
      TRUE ~ 'other wikis'
    )) %>%
  group_by(project, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(project ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Unique devices from Mexico, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018",
       caption = "Other wikis include top 5 projects in Mexico with the most pageviews"
       )
ggsave("ud_mexico_bywiki.png", plot = p1, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)

## eswiki ud from Mexico compare to other countries (grid by uniques_underestimate and uniques_offset)
p1 <- unique_devices %>%
  filter(
    project == 'es.wikipedia',
    type != 'total',
    date >= as.Date('2018-11-01'),
    date < as.Date('2019-02-28')
  ) %>%
  mutate(country = ifelse(country %in% c('Mexico', 'Spain'), country, 'Other countries')) %>%
  group_by(country, access_method, type, date) %>%
  summarize(uniques = sum(uniques)) %>%
  ggplot(aes(x = date, y = uniques, color = type)) +
  geom_line() +
  facet_grid(country ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Unique Devices", labels = polloi::compress) +
  scale_color_brewer("Type of visits", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Spanish Wikipedia unique devices by countries, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018"
       )
ggsave("eswiki_ud_bycountries.png", plot = p1, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 10)

## Google search console impression/clicks to es.wikipedia from Mexico
p <- gsc_eswiki %>%
  select(-position, -ctr) %>%
  mutate(country = ifelse(country == "MEX", "Mexico", "Other countries")) %>%
  group_by(access_method, country, date) %>%
  summarize_all(sum) %>%
  gather(key = action, value = counts, clicks, impressions) %>%
  filter(date >= as.Date("2018-11-01"), date < as.Date("2019-02-28")) %>%
  ggplot(aes(x = date, y = counts, color = country)) +
  geom_line() +
  facet_grid(action ~ access_method, switch = "y", scales = 'free') +
  scale_y_continuous("Counts", labels = polloi::compress) +
  scale_color_brewer("Country", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_facet(base_size = 12) +
  labs(title = "Google search impressions and clicks to Spanish Wikipedia, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018"
       )
ggsave("gsc_eswiki.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 12)

p <- gsc_eswiki %>%
  select(-position, -clicks, -impressions) %>%
  filter(country == "MEX", date >= as.Date("2018-11-01"), date < as.Date("2019-02-28")) %>%
  ggplot(aes(x = date, y = ctr, color = access_method)) +
  geom_line() +
  scale_y_continuous("Clickthrough Rate", labels = scales::percent) +
  scale_color_brewer("Access Method", palette = "Set1", guide = guide_legend(nrow = 1)) +
  scale_x_date(name = "Date") +
  geom_vline(xintercept = as.numeric(as.Date("2018-11-15")),
             linetype = "dashed", color = "black") +
  geom_vline(xintercept = as.numeric(as.Date("2018-12-16")),
             linetype = "dashed", color = "black") +
  wmf::theme_min(base_size = 12) +
  labs(title = "Google search clickthrough rate to Spanish Wikipedia from Mexico, Nov 2018 - Feb 2019",
       subtitle = "Dashed line represents the start and end dates of the video campaign, Nov 15 - Dec 16 2018"
       )
ggsave("gsc_ctr_eswiki.png", plot = p, path = 'docs/figures', units = "in", dpi = 300, height = 6, width = 12)
