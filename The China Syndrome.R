library(tidyverse)
library(lubridate)
library(utils)
library(httr)

# NOTE : All the charts and claims in the article are based on the data as of August 22

# I didn't include Singapore and Brunei because they're micro-states and
# they make the comparisons misleading and hard to present visually
east_asia <- c(
  "China",
  "South Korea",
  "Japan",
  "Mongolia",
  "Taiwan",
  "Macau",
  "Vietnam",
  "Laos",
  "Cambodia",
  "Myanmar",
  "Thailand",
  "Malaysia",
  "Indonesia",
  "East Timor",
  "Philippines"
)

# download the dataset from the ECDC website to a local temporary file
GET(
  "https://opendata.ecdc.europa.eu/covid19/casedistribution/csv",
  authenticate(":", ":", type = "ntlm"),
  write_disk(tf <- tempfile(fileext = ".csv"))
)

ecdc_data <- read_csv(tf) %>%
  mutate(
    date = dmy(dateRep),
    country = str_replace_all(countriesAndTerritories, "_", " ")
  ) %>%
  filter(date <= ymd("2020-08-22")) %>%
  group_by(countriesAndTerritories) %>%
  arrange(date) %>%
  mutate(
    cumulative_cases = cumsum(cases),
    cumulative_deaths = cumsum(deaths),
    CFR = cumulative_deaths / cumulative_cases * 100,
    case_rate = cumulative_cases / popData2019 * 100000,
    death_rate = cumulative_deaths / popData2019 * 1000000
  ) %>%
  ungroup() %>%
  select(
    date,
    country,
    cases,
    cumulative_cases,
    deaths,
    cumulative_deaths,
    CFR,
    case_rate,
    death_rate,
    popData2019
  )

data_cfr_plot <- ecdc_data %>%
  filter(date == max(date))

cfr_china <- data_cfr_plot$CFR[data_cfr_plot$country == "China"]
average_cfr <- mean(data_cfr_plot$CFR)

ggplot(data_cfr_plot, aes(x = CFR)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 1) +
  geom_vline(aes(xintercept = cfr_china, linetype = "china"), color = "red", size = 1) +
  geom_vline(aes(xintercept = average_cfr, linetype = "average"), color = "blue", size = 1) +
  xlim(c(0, 20)) +
  theme_minimal() +
  ggtitle("Distribution of case fatality rates across countries") +
  ylab("Number of countries") +
  xlab("CFR") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "china" = 1,
      "average" = 1
    ),
    labels = c(
      "China",
      "Average"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("red", "blue"))
    )
  ) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Distribution of case fatality rates across countries.png", width = 12, height = 6)

# I just remove Qatar and San Marino, which are outliers making it harder to visualize the distribution
data_case_plot <- ecdc_data %>%
  filter(date == max(date) & case_rate < 1400)

case_rate_china <- data_case_plot$case_rate[data_case_plot$country == "China"]
median_case_rate <- median(filter(ecdc_data, date == max(date))$case_rate)
n_case_rate_below_china <- nrow(filter(ecdc_data, date == max(date) & case_rate < case_rate_china))

ggplot(data_case_plot, aes(x = case_rate)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 25) +
  geom_vline(aes(xintercept = case_rate_china, linetype = "china"), color = "red", size = 1) +
  geom_vline(aes(xintercept = median_case_rate, linetype = "average"), color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("Distribution of number of cases per 100,000 across countries") +
  ylab("Number of countries") +
  xlab("Number of cases per 100,000") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "china" = 1,
      "average" = 1
    ),
    labels = c(
      "China",
      "Median"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("red", "blue"))
    )
  ) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Distribution of number of cases per 100,000 across countries.png", width = 12, height = 6)

data_case_plot_east_asia <- ecdc_data %>%
  filter(date == max(date) & country %in% east_asia)

median_case_rate_east_asia <- median(data_case_plot_east_asia$case_rate)

ggplot(data_case_plot_east_asia, aes(x = country, y = case_rate)) + 
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(case_rate, 1), vjust = -0.5),
            color = "black",
            size = 3) +
  theme_minimal() +
  ggtitle("Number of cases per 100,000 from COVID-19 in East Asia") +
  xlab("Country") +
  ylab("Number of cases per 100,000") +
  scale_color_discrete(name = "Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of cases per 100,000 from COVID-19 in East Asia.png", width = 12, height = 6)

# I remove San Marino because it's an outlier and makes it harder to visualize the distribution
data_death_plot <- ecdc_data %>%
  filter(date == max(date) & death_rate < 1000)

death_rate_china <- data_death_plot$death_rate[data_death_plot$country == "China"]
median_death_rate <- median(filter(ecdc_data, date == max(date))$death_rate)
n_death_rate_below_china <- nrow(filter(ecdc_data, date == max(date) & death_rate < death_rate_china))

ggplot(data_death_plot, aes(x = death_rate)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 5) +
  geom_vline(aes(xintercept = death_rate_china, linetype = "china"), color = "red", size = 1) +
  geom_vline(aes(xintercept = median_death_rate, linetype = "average"), color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("Distribution of number of deaths per million across countries") +
  ylab("Number of countries") +
  xlab("Number of deaths per million") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "china" = 1,
      "average" = 1
    ),
    labels = c(
      "China",
      "Median"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("red", "blue"))
    )
  ) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Distribution of number of deaths per million across countries.png", width = 12, height = 6)

data_death_plot_east_asia <- ecdc_data %>%
  filter(date == max(date) & country %in% east_asia)

median_death_rate_east_asia <- median(data_death_plot_east_asia$death_rate)

ggplot(data_death_plot_east_asia, aes(x = country, y = death_rate)) + 
  geom_bar(stat = "identity", color = "black", fill = "steelblue") +
  geom_text(aes(label = round(death_rate, 1), vjust = -0.5),
            color = "black",
            size = 3) +
  theme_minimal() +
  ggtitle("Number of deaths per million from COVID-19 in East Asia") +
  xlab("Country") +
  ylab("Number of deaths per million") +
  scale_color_discrete(name = "Country") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of deaths per million from COVID-19 in East Asia.png", width = 12, height = 6)

# https://github.com/CSSEGISandData/COVID-19
url_cases <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
url_deaths <- "https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"

data_cases <- read_csv(url(url_cases)) %>%
  rename(Province = `Province/State`) %>%
  filter(`Country/Region` == "China") %>%
  select(-c(`Country/Region`, Lat, Long)) %>%
  pivot_longer(-Province, names_to = "Date", values_to = "Cases")

data_deaths <- read_csv(url(url_deaths)) %>%
  rename(Province = `Province/State`) %>%
  filter(`Country/Region` == "China") %>%
  select(-c(`Country/Region`, Lat, Long)) %>%
  pivot_longer(-Province, names_to = "Date", values_to = "Deaths")

# http://data.stats.gov.cn/english/easyquery.htm?cn=E0103
china_population <- read_csv("AnnualbyProvince.csv") %>%
  rename(Province = Region) %>%
  mutate(Population2019 = `2019` * 10000) %>%
  select(Province, Population2019)

data_china <- inner_join(data_cases, data_deaths, by = c("Province", "Date")) %>%
  inner_join(china_population, by = c("Province")) %>%
  mutate(
    Date = mdy(Date),
    New_Cases = Cases - lag(Cases, default = 0),
    New_Deaths = Deaths - lag(Deaths, default = 0),
    CFR = ifelse(Cases > 0, Deaths / Cases, 0),
    Case_Rate = Cases / Population2019 * 100000,
    Death_Rate = Deaths / Population2019 * 1000000
  ) %>%
  filter(Date <= ymd("2020-08-22"))

case_rate_hubei <- filter(data_china, Date == max(Date) & Province == "Hubei")$Case_Rate
n_case_rate_below_hubei <- nrow(filter(ecdc_data, date == max(date) & case_rate < case_rate_hubei))

ggplot(data_case_plot, aes(x = case_rate)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 25) +
  geom_vline(aes(xintercept = case_rate_hubei, linetype = "hubei"), color = "red", size = 1) +
  geom_vline(aes(xintercept = median_case_rate, linetype = "average"), color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("Distribution of number of cases per 100,000 across countries (Hubei)") +
  ylab("Number of countries") +
  xlab("Number of cases per 100,000") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "hubei" = 1,
      "average" = 1
    ),
    labels = c(
      "Hubei",
      "Median"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("red", "blue"))
    )
  ) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Distribution of number of cases per 100,000 across countries (Hubei).png", width = 12, height = 6)

death_rate_hubei <- filter(data_china, Date == max(Date) & Province == "Hubei")$Death_Rate
n_death_rate_below_hubei <- nrow(filter(ecdc_data, date == max(date) & death_rate < death_rate_hubei))

ggplot(data_death_plot, aes(x = death_rate)) + 
  geom_histogram(colour = "black", fill = "white", binwidth = 5) +
  geom_vline(aes(xintercept = death_rate_hubei, linetype = "hubei"), color = "red", size = 1) +
  geom_vline(aes(xintercept = median_death_rate, linetype = "average"), color = "blue", size = 1) +
  theme_minimal() +
  ggtitle("Distribution of number of deaths per million across countries (Hubei)") +
  ylab("Number of countries") +
  xlab("Number of deaths per million") +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  ) +
  scale_linetype_manual(
    name = "lines",
    values = c(
      "hubei" = 1,
      "average" = 1
    ),
    labels = c(
      "Hubei",
      "Median"
    ),
    guide = guide_legend(
      title = "",
      override.aes = list(color = c("red", "blue"))
    )
  ) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Distribution of number of deaths per million across countries (Hubei).png", width = 12, height = 6)

training_start_cases <- ymd("2020-01-20")
training_end_cases <- ymd("2020-02-04")
training_start_deaths <- ymd("2020-01-20")
training_end_deaths <- ymd("2020-02-04")
plot_length <- 45

data_for_quadratic_fit_china <- ecdc_data %>%
  filter(country == "China" & date >= training_start_cases) %>%
  mutate(days_since_start = as.integer(date - training_start_cases))

fit_cases_china <- lm(
  cumulative_cases ~ days_since_start + I(days_since_start^2),
  filter(data_for_quadratic_fit_china, date >= training_start_cases & date <= training_end_cases)
)

fit_deaths_china <- lm(
  cumulative_deaths ~ days_since_start + I(days_since_start^2),
  filter(data_for_quadratic_fit_china, date >= training_start_deaths & date <= training_end_deaths)
)

data_for_quadratic_fit_china$predicted_cases <- round(predict(fit_cases_china, data_for_quadratic_fit_china))
data_for_quadratic_fit_china$predicted_deaths <- round(predict(fit_deaths_china, data_for_quadratic_fit_china))

data_for_plot_china <- filter(data_for_quadratic_fit_china, days_since_start >= 0 & days_since_start <= plot_length)

ggplot(data_for_plot_china, mapping = aes(x = date, y = cumulative_cases)) +
  geom_point() +
  geom_line(aes(y = predicted_cases), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    label = paste0("R^2 == ", as.character(trunc(summary(fit_cases_china)$r.squared * 100) / 100)),
    x = min(data_for_plot_china$date),
    y = 0.985 * max(data_for_plot_china$predicted_cases),
    hjust = 0,
    vjust = 1,
    parse = TRUE
  ) +
  theme_minimal() +
  ggtitle(paste0("Number of cases in China with prediction of a quadratic model")) +
  xlab("Date") +
  ylab("Number of cases") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of cases in China with prediction of a quadratic model.png", width = 12, height = 6)

ggplot(data_for_plot_china, mapping = aes(x = date, y = cumulative_deaths)) +
  geom_point() +
  geom_line(aes(y = predicted_deaths), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    label = paste0("R^2 == ", as.character(trunc(summary(fit_deaths_china)$r.squared * 100) / 100)),
    x = min(data_for_plot_china$date),
    y = 0.985 * max(data_for_plot_china$predicted_deaths),
    hjust = 0,
    vjust = 1,
    parse = TRUE
  ) +
  theme_minimal() +
  ggtitle("Number of deaths in China with prediction of a quadratic model") +
  xlab("Date") +
  ylab("Number of deaths") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of deaths in China with prediction of a quadratic model.png", width = 12, height = 6)

training_start_cases <- ymd("2020-02-15")
training_end_cases <- ymd("2020-02-29")
training_start_deaths <- ymd("2020-02-15")
training_end_deaths <- ymd("2020-02-29")
plot_length <- 45

data_for_quadratic_fit_sk <- ecdc_data %>%
  filter(country == "South Korea" & date >= training_start_cases) %>%
  mutate(days_since_start = as.integer(date - training_start_cases))

fit_cases_sk <- lm(
  cumulative_cases ~ days_since_start + I(days_since_start^2),
  filter(data_for_quadratic_fit_sk, date >= training_start_cases & date <= training_end_cases)
)

fit_deaths_sk <- lm(
  cumulative_deaths ~ days_since_start + I(days_since_start^2),
  filter(data_for_quadratic_fit_sk, date >= training_start_deaths & date <= training_end_deaths)
)

data_for_quadratic_fit_sk$predicted_cases <- round(predict(fit_cases_sk, data_for_quadratic_fit_sk))
data_for_quadratic_fit_sk$predicted_deaths <- round(predict(fit_deaths_sk, data_for_quadratic_fit_sk))

data_for_plot_sk <- filter(data_for_quadratic_fit_sk, days_since_start >= 0 & days_since_start <= plot_length)

ggplot(data_for_plot_sk, mapping = aes(x = date, y = cumulative_cases)) +
  geom_point() +
  geom_line(aes(y = predicted_cases), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    label = paste0("R^2 == ", as.character(trunc(summary(fit_cases_sk)$r.squared * 100) / 100)),
    x = min(data_for_plot_sk$date),
    y = 0.985 * max(data_for_plot_sk$predicted_cases),
    hjust = 0,
    vjust = 1,
    parse = TRUE
  ) +
  theme_minimal() +
  ggtitle(paste0("Number of cases in South Korea and prediction of a quadratic model (training set = data from ", training_start_cases, " to ", training_end_cases, ")")) +
  xlab("Date") +
  ylab("Number of cases") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of cases in South Korea with prediction of a quadratic model.png", width = 12, height = 6)

ggplot(data_for_plot_sk, mapping = aes(x = date, y = cumulative_deaths)) +
  geom_point() +
  geom_line(aes(y = predicted_deaths), size = 1, color = "steelblue") +
  annotate(
    geom = "text",
    label = paste0("R^2 == ", as.character(round(summary(fit_deaths_sk)$r.squared, 2))),
    x = min(data_for_plot_sk$date),
    y = 0.985 * max(data_for_plot_sk$predicted_deaths),
    hjust = 0,
    vjust = 1,
    parse = TRUE
  ) +
  theme_minimal() +
  ggtitle("Date") +
  xlab(paste0("Number of days since ", training_start_cases)) +
  ylab("Number of deaths") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  ggsave("Number of deaths in South Korea with prediction of a quadratic model.png", width = 12, height = 6)

benford_analysis_countries <- c(
  "China",
  "Italy",
  "France",
  "Spain",
  "Sweden",
  "South Korea",
  "Germany",
  "Japan",
  "Australia",
  "Thailand",
  "New Zealand",
  "United States",
  "United Kingdom",
  "Canada",
  "Denmark",
  "Brazil"
)

benford_analysis_cases <- ecdc_data %>%
  filter(cases > 0) %>%
  mutate(
    first_digit = str_sub(as.character(cases), 1, 1)
  ) %>%
  group_by(country, first_digit) %>%
  summarize(n = n()) %>%
  mutate(
    actual_proportion = n / sum(n)
  ) %>%
  complete(
    first_digit = as.character(1:9),
    fill = list(n = 0, actual_proportion = 0)
  ) %>%
  mutate(benford_proportion = log10(1 + 1 / as.integer(first_digit)))

benford_analysis_deaths <- ecdc_data %>%
  filter(deaths > 0) %>%
  mutate(
    first_digit = str_sub(as.character(deaths), 1, 1)
  ) %>%
  group_by(country, first_digit) %>%
  summarize(n = n()) %>%
  mutate(
    actual_proportion = n / sum(n)
  ) %>%
  complete(
    first_digit = as.character(1:9),
    fill = list(n = 0, actual_proportion = 0)
  ) %>%
  mutate(
    benford_proportion = log10(1 + 1 / as.integer(first_digit)),
    error = actual_proportion - benford_proportion
  )

chi_square_test_cases <- benford_analysis_cases %>%
  group_by(country) %>%
  summarize(
    pvalue = chisq.test(x = n, p = benford_proportion, rescale.p = TRUE)$p.value
  )

chi_square_test_deaths <- benford_analysis_deaths %>%
  group_by(country) %>%
  summarize(
    pvalue = chisq.test(x = n, p = benford_proportion)$p.value
  )

ggplot(benford_analysis_cases, mapping = aes(x = first_digit, y = benford_proportion)) +
  geom_line(size = 1, color = "steelblue", group = 1) +
  geom_point(aes(y = actual_proportion)) +
  theme_minimal() +
  ggtitle("Benford's law analysis for the daily number of new cases in a selection of countries") +
  xlab("Digit") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  facet_wrap(~ country, ncol = 3) +
  ggsave("Benford's law analysis for the daily number of new cases in a selection of countries.png", width = 12, height = 12)

ggplot(benford_analysis_deaths, mapping = aes(x = first_digit, y = benford_proportion)) +
  geom_line(size = 1, color = "steelblue", group = 1) +
  geom_point(aes(y = actual_proportion)) +
  theme_minimal() +
  ggtitle("Benford's law analysis for the daily number of new deaths in a selection of countries") +
  xlab("Digit") +
  ylab("Proportion") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme(plot.title = element_text(hjust = 0.5)) +
  labs(caption = "Source : ECDC (https://www.ecdc.europa.eu/en/publications-data/download-todays-data-geographic-distribution-covid-19-cases-worldwide) - Chart by Philippe Lemoine (@phl43) for Quillette") +
  facet_wrap(~ country, ncol = 3) +
  ggsave("Benford's law analysis for the daily number of new deaths in a selection of countries.png", width = 12, height = 12)

population_china_south_east_asia <- 1400000000 + # China
  15000000 + # Cambodia
  267000000 + # Indonesia
  7000000 + # Laos
  53000000 + # Myanmar
  32000000 + # Malaysia
  100000000 + # Philippines
  66000000 + # Thailand
  96000000 # Vietnam