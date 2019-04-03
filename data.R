# Remote on stat1007:

# Pageviews to eswiki and other top wikis by traffic from Mexico and other countries
# spark2R --master yarn --executor-memory 2G --executor-cores 1 --driver-memory 4G
start_date <- as.Date("2015-05-01")
end_date <- Sys.Date() - 1
pageviews <- do.call(rbind, lapply(seq(start_date, end_date, "day"), function(date) {
  cat("Fetching pageviews from ", as.character(date), "\n")
  clause_data <- wmf::date_clause(date)
  query <- paste("
      SELECT CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
      year, month, day,
      country_code,
      project,
      access_method,
      referer_class,
      SUM(view_count) AS pageviews
      FROM wmf.projectview_hourly",
      clause_data$date_clause,
      "AND project IN ('en.wikipedia', 'es.wikipedia', 'es.wiktionary', 'commons.wikimedia', 'es.wikibooks')
      -- above include top 5 projects in Mexico with the most pv
      AND country_code IN ('MX','AR','CL','CO','CU','EC','GT','PE','ES','US','VE','DO')
      -- above include top 10 countries with most pv to es, and top 10 countries whose official language is Spanish
      -- see https://en.wikipedia.org/wiki/List_of_countries_where_Spanish_is_an_official_language
      AND agent_type = 'user'
      GROUP BY year, month, day, country_code, project, access_method, referer_class")
  results <- tryCatch(
  suppressMessages(collect(sql(query))),
  error = function(e) {
    return(data.frame(
      date = date,
      year = integer(),
      month = integer(),
      day = integer(),
      country_code = character(),
      project= character(),
      access_method= character(),
      referer_class= character(),
      pageviews = numeric()
    ))
  })
  return(results)
}))
save(pageviews, file = "data/mexico_campaign/pageviews.RData")

# Unique devices
query <- "
SELECT CONCAT(year, '-', LPAD(month, 2, '0'), '-', LPAD(day, 2, '0')) AS date,
year, month, day,
country_code, country,
domain,
SUM(uniques_estimate) AS uniques_estimate,
SUM(uniques_underestimate) AS uniques_underestimate,
SUM(uniques_offset) AS uniques_offset
FROM wmf.unique_devices_per_domain_daily
WHERE domain IN ('en.wikipedia.org', 'es.wikipedia.org', 'es.wiktionary.org', 'commons.wikimedia.org', 'es.wikibooks.org',
'en.m.wikipedia.org', 'es.m.wikipedia.org', 'es.m.wiktionary.org', 'commons.m.wikimedia.org', 'es.m.wikibooks.org')
-- above include top 5 projects in Mexico with the most pv
AND country_code IN ('MX','AR','CL','CO','CU','EC','GT','PE','ES','US','VE','DO')
-- above include top 10 countries with most pv to es, and top 10 countries whose official language is Spanish
-- see https://en.wikipedia.org/wiki/List_of_countries_where_Spanish_is_an_official_language
AND year >= 2015
GROUP BY year, month, day, country_code, country, domain;
"
unique_devices <- wmf::query_hive(query)
save(unique_devices, file = "data/mexico_campaign/unique_devices.RData")

# Local:
system("scp chelsyx@stat7:~/data/mexico_campaign/pageviews.RData data/")
system("scp chelsyx@stat7:~/data/mexico_campaign/unique_devices.RData data/")

# Google search console data
system("scp chelsyx@stat6:/home/bearloga/gsc/output/https/wikipedia/es.m.wikipedia.org/country-all.csv data/eswiki_m_gsc.csv")
system("scp chelsyx@stat6:/home/bearloga/gsc/output/https/wikipedia/es.wikipedia.org/country-all.csv data/eswiki_gsc.csv")
