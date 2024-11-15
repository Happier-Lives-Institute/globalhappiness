source("dependencies.R")

#~############################################################################~#
# Data ----
#~############################################################################~#

# World Happiness Report data
dat_WHR_SDs <- readxl::read_excel("data/WHR/WHR2018.xls") %>%
    rename(
        country = `Country name`,
        year = `Year`,
        SD = `Standard deviation of ladder by country-year`,
        cantril = `Life Ladder`
    ) %>%
    select(country, year, cantril, SD)

dat_WHR <- readxl::read_excel("data/WHR/DataForTable2.1.2024.xls") %>%
    rename(
        country = `Country name`,
        # year = `Year`,
        cantril = `Life Ladder`
    ) %>%
    select(country, year, cantril)

# UNDP data
WPP <- read_csv(
    "data/UNPD/WPP2024_Population1JanuaryBySingleAgeSex_Medium_1950-2023.csv"
    , show_col_types = F
)
# summary(WPP)

WPP <- WPP %>% rename(
    PopTotal_1000s = PopTotal,
    country = Location,
    year = Time
) %>%
    # Put the population in individuals rather than 1000s
    mutate(PopTotal = PopTotal_1000s * 1000)
# unique(WPP$Location)
# unique(WPP$Time)

# Select only countries
WPP <- WPP %>% filter(LocTypeName == "Country/Area")

# Compare names
WPP_names <- WPP %>% select(country) %>% unique() %>% arrange(country)
dat_WHR %>% filter(!(country %in% WPP$country)) %>% pull(country) %>% unique()

dat_WHR <- dat_WHR %>% mutate(
    country = case_when(
        country == "Congo (Brazzaville)" ~ "Congo",
        country == "Congo (Kinshasa)" ~ "Democratic Republic of the Congo",
        .default = country
    )
)

WPP <- WPP %>%
    # align names
    mutate(
        country = case_when(
            country == "Bolivia (Plurinational State of)" ~
                "Bolivia",
            country == "China, Hong Kong SAR" ~
                "Hong Kong S.A.R. of China",
            country == "China, Taiwan Province of China" ~
                "Taiwan Province of China",
            country == "Iran (Islamic Republic of)" ~
                "Iran",
            country == "Côte d'Ivoire" ~
                "Ivory Coast",
            country == "Kosovo (under UNSC res. 1244)" ~
                "Kosovo",
            country == "Lao People's Democratic Republic" ~
                "Laos",
            country == "Russian Federation" ~
                "Russia",
            country == "Republic of Moldova" ~
                "Moldova",
            country == "Syrian Arab Republic" ~
                "Syria",
            country == "United States of America" ~
                "United States",
            country == "Venezuela (Bolivarian Republic of)" ~
                "Venezuela",
            country == "Viet Nam" ~
                "Vietnam",
            country == "Republic of Korea" ~
                "South Korea",
            country == "United Republic of Tanzania" ~
                "Tanzania",
            .default = country
        )
    )

# Remove Somaliland for simplicity
dat_WHR <- dat_WHR %>% filter(country != "Somaliland region")

# Remove 2005 because it is strange
dat_WHR <- dat_WHR %>% filter(year > 2005)

WPP_country_pop <- WPP %>%
    group_by(country, year) %>%
    summarise(PopTotal = sum(PopTotal, na.rm = TRUE)) %>%
    ungroup() %>%
    arrange(country, year)

dat_WHR <- dat_WHR %>% left_join(WPP_country_pop, by = c("country", "year")) %>%
    left_join(dat_WHR_SDs %>% select(-cantril), by = c("country", "year"))

#~############################################################################~#
# Analysis ----
#~############################################################################~#

dat_WHR_summary <- dat_WHR %>%
    group_by(year) %>%
    summarise(
        mean = mean(cantril, na.rm = TRUE),
        pwm = weighted.mean(cantril, PopTotal, na.rm = TRUE),
        wcsd = mean(SD, na.rm = TRUE),
        bcsd = sd(cantril, na.rm = TRUE)
    ) %>%
    ungroup()

dat_WHR_summary_long <- dat_WHR_summary %>%
    pivot_longer(
        cols = c(mean, pwm, wcsd, bcsd),
        names_to = "metric",
        values_to = "value"
    )

dat_WHR_summary_long %>% filter(year == max(year))

dat_WHR_rank_2023 <- dat_WHR %>%
    filter(year == 2023) %>%
    mutate(
        rank = rank(-cantril, ties.method = "random"),
        rank_pop = rank(-PopTotal, ties.method = "random"),
        total_wellbys = cantril * PopTotal,
        rank_wellby = rank(-total_wellbys, ties.method = "random")
    ) %>% arrange(rank_pop) %>%
    select(country, rank, rank_pop, rank_wellby, cantril, PopTotal, total_wellbys)

dat_WHR_rank_2023_top10 <- dat_WHR_rank_2023 %>% filter(rank_pop < 11)
dat_WHR_rank_2023_bottom10 <- dat_WHR_rank_2023 %>% filter(rank > max(rank) - 10)

dat_WHR_countries <- dat_WHR %>%
    group_by(country, year) %>%
    summarise(
        total = cantril * PopTotal,
        mean = mean(cantril, na.rm = TRUE),
        wcsd = mean(SD, na.rm = TRUE),
    ) %>%
    ungroup()

#~############################################################################~#
# Graphs ----
#~############################################################################~#

p1 <- dat_WHR_summary_long %>%
    filter(metric %in% c("mean", "pwm")) %>%
    mutate(
        metric = ifelse(metric == "mean", "simple average",
                        "population weighted average")
    ) %>%
    ggplot(aes(x = year, y = value, color = metric)) +
    geom_vline(xintercept = 2008, linetype = 3) +
    geom_vline(xintercept = 2020, linetype = 3) +
    geom_point(
        data = dat_WHR, inherit.aes = F,
        aes(x=year, y=cantril), alpha = 0.1, shape = 16
    ) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
        ) +
    scale_x_continuous(breaks = c(seq(2006, 2023, 2))) +
    scale_y_continuous(limits = c(0, 10)); p1

ggsave(
    filename = "graph/average.png",
    plot = p1,
    width = 6,
    height = 4,
    dpi = 1200
)

p2 <- dat_WHR_summary_long %>%
    filter(metric %in% c("mean", "pwm")) %>%
    mutate(
        metric = ifelse(metric == "mean", "simple average",
                        "population weighted average")
    ) %>%
    ggplot(aes(x = year, y = value, color = metric)) +
    geom_vline(xintercept = 2008, linetype = 3) +
    geom_vline(xintercept = 2020, linetype = 3) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(4.80, 5.8), breaks = seq(4.80, 5.8, 0.2)); p2

ggsave(
    filename = "graph/average_zoom.png",
    plot = p2,
    width = 6,
    height = 4,
    dpi = 1200
)


p3 <- dat_WHR %>%
    filter(country %in% dat_WHR_rank_2023_top10$country) %>%
    ggplot(aes(x = year, y = cantril, colour = country)) +
    geom_vline(xintercept = 2008, linetype = 3) +
    geom_vline(xintercept = 2020, linetype = 3) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(3,8), breaks = seq(0, 10, 0.5)); p3

ggsave(
    filename = "graph/average_most_pop.png",
    plot = p3,
    width = 7,
    height = 4,
    dpi = 1200
)

p4 <- dat_WHR_summary_long %>%
    filter(metric %in% c("wcsd", "bcsd")) %>%
    mutate(
        metric = ifelse(metric == "wcsd", "within-country SD",
                        "between-country SD")
    ) %>%
    ggplot(aes(x = year, y = value, color = metric)) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    ylab("Spread") + xlab("Year") +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(0.75,2.75), breaks = seq(0, 3, 0.25)); p4

ggsave(
    filename = "graph/inequality.png",
    plot = p4,
    width = 6,
    height = 4,
    dpi = 1200
)

p5 <- dat_WHR_countries %>%
    filter(country %in% c("Russia", "Ukraine")) %>%
    ggplot(
        aes(x = year, y = mean, color = country)
    ) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(0, 10)); p5

ggsave(
    filename = "graph/conflict1.png",
    plot = p5,
    width = 6,
    height = 4,
    dpi = 1200
)

p6 <- dat_WHR_countries %>%
    filter(country %in% c("Israel", "State of Palestine", "Lebanon", "Jordan", "Iran")) %>%
    ggplot(
        aes(x = year, y = mean, color = country)
    ) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(0, 10)); p6

ggsave(
    filename = "graph/conflict2.png",
    plot = p6,
    width = 6,
    height = 4,
    dpi = 1200
)

p7 <- dat_WHR_countries %>%
    filter(country %in% c("Saudi Arabia", "Yemen", "Iran", "Iraq")) %>%
    ggplot(
        aes(x = year, y = mean, color = country)
    ) +
    geom_line() +
    geom_point() +
    theme_hli_wbg() +
    ylab("Cantril Ladder") + xlab("Year") +
    theme(
        legend.title = element_blank(),
        legend.position = "bottom"
    ) +
    scale_x_continuous(breaks = seq(2006, 2023, 2)) +
    scale_y_continuous(limits = c(0, 10)); p7

ggsave(
    filename = "graph/conflict3.png",
    plot = p7,
    width = 6,
    height = 4,
    dpi = 1200
)
