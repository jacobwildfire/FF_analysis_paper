############ HH Roadmap Analysis Code
#######
#######
####### Created by Dr Jacob Wildfire as part of the Fleming Fund



####### Having calculated the status for each LSHTM Roadmap subcomponent,
####### the following code produces visualisations showing the change in the 
####### proportions of sites performing each level of function over time.
#######



# Note: All path examples provide a default
#       that can be run using the files provided
#       in the "FF_analysis" GitHub repository,
#       provided appropriate modifications
#       are made (see README.txt).




#################################### Required packages

# List of required packages
required_packages <- c("ggplot2", "dplyr", "readxl", "writexl", "stringr", 
                       "tidyr", "DescTools", "shiny", "lubridate", "purrr")

# Install missing packages
missing_packages <- required_packages[!(required_packages %in% installed.packages()[,"Package"])]
if(length(missing_packages)) install.packages(missing_packages)

# Load all required packages
for(pkg in required_packages) {
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
  message(paste("Loaded package:", pkg))
}




#################################### Working Directory

wd <- "~/FF programme analysis resources paper/Paper/Paper analyses/" # Replace with your working directory. 


if (getwd() != wd) { 
  setwd(wd)
}



#################################### Loading dataframes

## Load up the LSHTM subcomponent source information files
df <- read_xlsx("Data/1.a HH LSHTM Roadmap status non-uploaded_version.xlsx")


## Load in site masterlist to obtain site information:
site_info <- read_xlsx("~/M&E/2025Q3/On-going updates_Master List of Sentinel Sites_July 2025.xlsx", sheet = "Country_List of Sentinel Sites")

#################################### Graph output location
## Choose the location into which you would like your HH LSHTM Roadmap graphs
## to go into.
file_save <- "Figures/"


#################################### Sector
## As we are looking at human health sites, we are going to set this to 
## "Human Health" so that we can isolate these sites.

sector <- "Human Health"

#################################### Prepare relevant dataframes

## convert each subcomponent "tier" into the subcomponent's actual name
custom_labels <- c("tier1a" = "1a. Clinical admission assessment", "tier1b" = "1b. Clinical data",
                   "tier1c" = "1c. Clinical investigation", "tier1d" = "1d. Clinical training & QA",
                   "tier2a" = "2a. Sample transport", "tier2b" = "2b. Sample registration",
                   "tier2c" = "2c. Culture & identification", "tier2d" = "2d. Susceptibility testing",
                   "tier2e" = "2e. Testing training & QA", "tier3a" = "3a. Storage of isolates",
                   "tier3b" = "3b. Transport to AMR laboratory", "tier3c" = "3c. Isolate storage training & QA", 
                   "tier4a" = "4a. Data use", "tier4b" = "4b. Data linkage", "tier4c" = "4c. Data governance")


## Convert to a long format
df_long <- df %>%
  select(!c("clinical_care","core", "extended", "advanced", "core or above")) %>%
  pivot_longer(
    cols = c("tier1a", "tier1b", "tier1c", "tier1d", "tier2a", "tier2b", "tier2c",
             "tier2d", "tier2e", "tier3a", "tier3b", "tier3c", "tier4a", "tier4b",
             "tier4c"), 
    names_to = "LSHTM subcomponent",
    values_to = "value"
  )

## Determine the actual dates of the df_long dataframe
date_conversion <- function(df) {
  df<-df %>% mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `Start date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-01-01")),
      period == "Q2" ~ as.Date(paste0(year, "-04-01")),
      period == "Q3" ~ as.Date(paste0(year, "-07-01")),
      period == "Q4" ~ as.Date(paste0(year, "-10-01")),
      period == "S1" ~ as.Date(paste0(year, "-01-01")),
      period == "S2" ~ as.Date(paste0(year, "-07-01"))
    ),
    `End date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-03-31")),
      period == "Q2" ~ as.Date(paste0(year, "-06-30")),
      period == "Q3" ~ as.Date(paste0(year, "-09-30")),
      period == "Q4" ~ as.Date(paste0(year, "-12-31")),
      period == "S1" ~ as.Date(paste0(year, "-06-30")),
      period == "S2" ~ as.Date(paste0(year, "-12-31"))
    )
  )%>%
    select(!c("year", "period"))
  
  return(df)
  
}

# Apply this function to the df_surv_long dataset.
df_long <- date_conversion(df_long)%>%
  mutate(value = factor(value, levels = c("Precore",
                                          "Core",
                                          "Extended",
                                          "Advanced",
                                          "Not applicable"))) %>%
  arrange(`End date`) %>%
  group_by(sitecode) %>%
  mutate(Baseline = first(`Start date`),
         `Months in programme` = as.numeric(round((`End date` - `Baseline`)/30.417, digit=0))) %>%
  ungroup()






# Generate summary dataframes, calculating the proportion of "Precore", "Core", 
## "Extended" and "Advanced" by date and type.
df_surv <- df_long %>%
  filter(type == "Surveillance") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )


## Proportion of precore, core, extended and advanced by date, reference
df_ref <- df_long %>%
  filter(type == "Reference") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )



## Produce a longer form of the datasets, producing a long version of the number of sites
## and then the proportion of sites
df_surv_long <- df_surv %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_surv_long_sites <- df_surv %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)


## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_surv_long$reporting.month == df_surv_long_sites$reporting.month) & 
    all(df_surv_long$`LSHTM subcomponent` == df_surv_long_sites$`LSHTM subcomponent`) & 
    all(df_surv_long$Level == df_surv_long_sites$Level)) {
df_surv_long <- data.frame(df_surv_long_sites, Proportion = df_surv_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_surv_long and df_surv_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

## Repeat for reference sites.
df_ref_long <- df_ref %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_ref_long_sites <- df_ref %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable")))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)



if (all(df_ref_long$reporting.month == df_ref_long_sites$reporting.month) & 
    all(df_ref_long$`LSHTM subcomponent` == df_ref_long_sites$`LSHTM subcomponent`) & 
    all(df_ref_long$Level == df_ref_long_sites$Level)) {
  df_ref_long <- data.frame(df_ref_long_sites, Proportion = df_ref_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_ref_long and df_surv_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

rm(df_surv_long_sites, df_ref_long_sites)

## Use the YYYYS# reporting.month date format to produce a function that produces
## date columns with the report date start and end:

date_conversion <- function(df) {
  df<-df %>% mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `Start date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-01-01")),
      period == "Q2" ~ as.Date(paste0(year, "-04-01")),
      period == "Q3" ~ as.Date(paste0(year, "-07-01")),
      period == "Q4" ~ as.Date(paste0(year, "-10-01")),
      period == "S1" ~ as.Date(paste0(year, "-01-01")),
      period == "S2" ~ as.Date(paste0(year, "-07-01"))
    ),
    `End date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-03-31")),
      period == "Q2" ~ as.Date(paste0(year, "-06-30")),
      period == "Q3" ~ as.Date(paste0(year, "-09-30")),
      period == "Q4" ~ as.Date(paste0(year, "-12-31")),
      period == "S1" ~ as.Date(paste0(year, "-06-30")),
      period == "S2" ~ as.Date(paste0(year, "-12-31"))
    )
  )%>%
    select(!c("year", "period"))

  return(df)
  
}

# Apply this function to the df_surv_long dataset.
df_surv_long <- date_conversion(df_surv_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))


# Conversion of ref dataset.
df_ref_long <- date_conversion(df_ref_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))

longer_surv_dates <- subset(df_surv_long, as.Date(`End date`) > "2024-01-01")
longer_surv_dates[as.Date(longer_surv_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_surv_dates[as.Date(longer_surv_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_surv_dates[as.Date(longer_surv_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")


df_surv_long_col <- rbind(df_surv_long, longer_surv_dates)


longer_ref_dates <- subset(df_ref_long, as.Date(`End date`) > "2024-01-01")
longer_ref_dates[as.Date(longer_ref_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_ref_dates[as.Date(longer_ref_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_ref_dates[as.Date(longer_ref_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")

df_ref_long_col <- rbind(df_ref_long, longer_ref_dates)



## Plots demonstrating percentage of sites over time

plot_surv <- df_surv_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_inverse <- ggplot(df_surv_long_col, aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))


plot_ref <- df_ref_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_ref_inverse <- ggplot(df_ref_long_col, aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



## Plots demonstrating numbers of sites over time

plot_surv_count <- df_surv_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_inverse_count <- ggplot(df_surv_long_col, aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))


plot_ref_count <- df_ref_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_ref_inverse_count <- ggplot(df_ref_long_col, aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))





########################################### Analyses by time spent in programme:

# Generate summary dataframes, calculating the proportion of "Precore", "Core", 
## "Extended" and "Advanced" by date and type.
df_surv_impl.time <- df_long %>%
  filter(type == "Surveillance") %>%
  group_by(`Months in programme`, `LSHTM subcomponent`) %>%
  summarise(
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  )


## Proportion of precore, core, extended and advanced by date, reference
df_ref_impl.time <- df_long %>%
  filter(type == "Reference") %>%
  group_by(`Months in programme`, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  )

## Produce a longer form of the datasets, producing a long version of the number of sites
df_surv_long_impl.time <- df_surv_impl.time %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(`Months in programme`,`LSHTM subcomponent`, Level)


## Repeat for reference sites
df_ref_long_impl.time <- df_ref_impl.time %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(`Months in programme`,`LSHTM subcomponent`, Level)


############################################ Plots using time in programme

plot_surv_impl.time <- df_surv_long_impl.time %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=`Months in programme`, y = Sites, fill = Level))+
  geom_col(width = 3)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Months of implementation")+
  ylab("Number of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))

############### Note, this plot is not that useful as sites will reach a final 
############### implementation month, and then drop off, which increases more and 
############### more towards later implementation months. Therefore, it's 
############### difficult to see whether decreases in functional levels are because
############### sites are changing their function over time (or not), or because
############### fewer sites make it to later implementation months.

## Plots using the baseline and endline values

df_surv_impl.time_baseline <- df_long %>%
  filter(type == "Surveillance") %>%
  arrange(`Start date`) %>%
  group_by(`LSHTM subcomponent`,sitecode) %>%
  filter(row_number() == 1) %>%
  ungroup() %>%
  group_by(`LSHTM subcomponent`) %>%
  summarise(
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  ) %>%
  ungroup() %>%
  mutate(`Months in programme` = 3) %>%
  select("Months in programme", "LSHTM subcomponent", "core", "extended", 
         "advanced", "precore", "not_applicable")
  


df_surv_impl.time_baseline_long <- df_surv_impl.time_baseline %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(`LSHTM subcomponent`, Level) %>%
  group_by(`LSHTM subcomponent`) %>%
  mutate(Proportion = Sites/sum(Sites),
         grouping2 = "0") %>%
  select(c("grouping2","LSHTM subcomponent","Level", "Sites", "Proportion"))




df_surv_impl.time_endline <- df_long %>%
  filter(type == "Surveillance") %>%
  arrange(`Start date`) %>%
  group_by(`LSHTM subcomponent`,sitecode) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(grouping1 = ifelse(`Months in programme`<20, "<20", ifelse(
    `Months in programme`>=20 & `Months in programme`<40, "20-39", ifelse(
      `Months in programme`>=40 & `Months in programme`<60, "40-59", "≥60"
    )
  )),
         grouping2 = ifelse(`Months in programme`<30, "<30", ifelse(
           `Months in programme`>=30 & `Months in programme`<60, "30-59", "≥60"))
  )%>%
  group_by(grouping2, `LSHTM subcomponent`) %>%
  summarise(
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable")
  ) %>%
  ungroup()

df_surv_impl.time_endline_long <- df_surv_impl.time_endline %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(`grouping2`,`LSHTM subcomponent`, Level) %>%
  group_by(`grouping2`,`LSHTM subcomponent`) %>%
  mutate(Proportion = Sites/sum(Sites))


surv_baseline_endline <- rbind(df_surv_impl.time_baseline_long, df_surv_impl.time_endline_long)


df_surv_impl.time_endline_long %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore")),
         grouping2 = factor(grouping2, levels = c("<30", "30-59", "≥60"))) %>%
  ggplot(aes(x=`grouping2`, y = Sites, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Months of implementation")+
  ylab("Number of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



df_surv_impl.time_endline_long %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore")),
         grouping2 = factor(grouping2, levels = c("<30", "30-59", "≥60"))) %>%
  ggplot(aes(x=`grouping2`, y = Proportion, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Months of implementation")+
  ylab("Number of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))

full_surv_plot_sites <- surv_baseline_endline %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore")),
         grouping2 = factor(grouping2, levels = c("0","<30", "30-59", "≥60"))) %>%
  ggplot(aes(x=`grouping2`, y = Sites, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Months of implementation")+
  ylab("Number of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))

full_surv_plot_proportion <- surv_baseline_endline %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore")),
         grouping2 = factor(grouping2, levels = c("0","<30", "30-59", "≥60"))) %>%
  ggplot(aes(x=`grouping2`, y = Proportion, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Months of implementation")+
  ylab("Proportion of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))




baseline_vs_endline_surv_plot_sites <- rbind(df_surv_impl.time_baseline_long, df_surv_impl.time_endline_long) %>%
  mutate(grouping2 = ifelse(grouping2 == "0", "First", "Final"),
         grouping2 = factor(grouping2, levels = c("First", "Final")),
         `Level` = factor(Level, levels = c("not_applicable", "advanced",
                                                "extended", "core", "precore"))) %>%
  group_by(`grouping2`,`LSHTM subcomponent`, Level) %>%
  summarize(Sites = sum(Sites)) %>%
  arrange(`grouping2`,`LSHTM subcomponent`, Level) %>%
  group_by(`grouping2`,`LSHTM subcomponent`) %>%
  mutate(Proportion = Sites/sum(Sites)) %>%
  ungroup() %>%
  ggplot(aes(x=`grouping2`, y = Sites, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Measurement")+
  ylab("Number of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))


baseline_vs_endline_surv_plot_prop <- rbind(df_surv_impl.time_baseline_long, df_surv_impl.time_endline_long) %>%
  mutate(grouping2 = ifelse(grouping2 == "0", "First", "Final"),
         grouping2 = factor(grouping2, levels = c("First", "Final")),
         `Level` = factor(Level, levels = c("not_applicable", "advanced",
                                            "extended", "core", "precore"))) %>%
  group_by(`grouping2`,`LSHTM subcomponent`, Level) %>%
  summarize(Sites = sum(Sites)) %>%
  arrange(`grouping2`,`LSHTM subcomponent`, Level) %>%
  group_by(`grouping2`,`LSHTM subcomponent`) %>%
  mutate(Proportion = Sites/sum(Sites)) %>%
  ungroup() %>%
  ggplot(aes(x=`grouping2`, y = Proportion, fill = Level))+
  geom_col()+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("Measurement")+
  ylab("Proportion of sites")+
  scale_y_continuous()+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))






############################################ Plots separating sites joining in phase 1 vs phase 2

# Generate summary dataframes, calculating the proportion of "Precore", "Core", 
## "Extended" and "Advanced" by date and type.
df_surv_phase1 <- df_long %>%
  subset(Baseline < "2024-01-01") %>%
  filter(type == "Surveillance") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )

df_surv_phase2 <- df_long %>%
  subset(Baseline >= "2024-01-01") %>%
  filter(type == "Surveillance") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )



## Proportion of precore, core, extended and advanced by date, reference
df_ref_phase1 <- df_long %>%
  subset(Baseline < "2024-01-01") %>%
  filter(type == "Reference") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )

## Proportion of precore, core, extended and advanced by date, reference
df_ref_phase2 <- df_long %>%
  subset(Baseline >= "2024-01-01") %>%
  filter(type == "Reference") %>%
  group_by(reporting.month, `LSHTM subcomponent`) %>%
  summarise(
    at_least_core = sum(value %in% c("Core", "Extended", "Advanced")),
    core = sum(value == "Core"),
    extended = sum(value == "Extended"),
    advanced = sum(value == "Advanced"),
    precore = sum(value == "Precore"),
    not_applicable = sum(value == "Not applicable"),
    active_site_count = n()
  ) %>%
  mutate(
    prop_core_above = at_least_core / active_site_count,
    prop_core = core / active_site_count,
    prop_extended = extended / active_site_count,
    prop_advanced = advanced / active_site_count,
    prop_precore = precore / active_site_count,
    prop_not_applicable = not_applicable / active_site_count,
  )



## Produce a longer form of the datasets, producing a long version of the number of sites
## and then the proportion of sites
df_surv_phase1_long <- df_surv_phase1 %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_surv_phase2_long <- df_surv_phase2 %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_surv_phase1_long_sites <- df_surv_phase1 %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_surv_phase2_long_sites <- df_surv_phase2 %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

#### Repeat for ref sites

## Produce a longer form of the datasets, producing a long version of the number of sites
## and then the proportion of sites
df_ref_phase1_long <- df_ref_phase1 %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_ref_phase2_long <- df_ref_phase2 %>%
  pivot_longer(cols = c(prop_precore, prop_core, prop_extended, prop_advanced, prop_not_applicable), names_to = "Level", values_to = "Proportion") %>%
  select(1:2,11:12) %>%
  mutate(Level = ifelse(
    Level == "prop_not_applicable", "not_applicable", ifelse(
      Level == "prop_advanced", "advanced", ifelse(
        Level == "prop_extended", "extended", ifelse(
          Level == "prop_core", "core", "precore"
        )
      )
    )
  ))%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_ref_phase1_long_sites <- df_ref_phase1 %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)

df_ref_phase2_long_sites <- df_ref_phase2 %>%
  pivot_longer(cols = c("precore",
                        "core",
                        "extended",
                        "advanced",
                        "not_applicable"), names_to = "Level", values_to = "Sites") %>%
  select(1:2,11:12) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable"))) %>%
  arrange(reporting.month,`LSHTM subcomponent`, Level)


## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_surv_phase1_long$reporting.month == df_surv_phase1_long_sites$reporting.month) & 
    all(df_surv_phase1_long$`LSHTM subcomponent` == df_surv_phase1_long_sites$`LSHTM subcomponent`) & 
    all(df_surv_phase1_long$Level == df_surv_phase1_long_sites$Level)) {
  df_surv_phase1_long <- data.frame(df_surv_phase1_long_sites, Proportion = df_surv_phase1_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_surv_phase1_long and df_surv_phase1_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_surv_phase2_long$reporting.month == df_surv_phase2_long_sites$reporting.month) & 
    all(df_surv_phase2_long$`LSHTM subcomponent` == df_surv_phase2_long_sites$`LSHTM subcomponent`) & 
    all(df_surv_phase2_long$Level == df_surv_phase2_long_sites$Level)) {
  df_surv_phase2_long <- data.frame(df_surv_phase2_long_sites, Proportion = df_surv_phase2_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_surv_phase2_long and df_surv_phase2_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}


## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_ref_phase1_long$reporting.month == df_ref_phase1_long_sites$reporting.month) & 
    all(df_ref_phase1_long$`LSHTM subcomponent` == df_ref_phase1_long_sites$`LSHTM subcomponent`) & 
    all(df_ref_phase1_long$Level == df_ref_phase1_long_sites$Level)) {
  df_ref_phase1_long <- data.frame(df_ref_phase1_long_sites, Proportion = df_ref_phase1_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_ref_phase1_long and df_ref_phase1_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}

## Then, combine these two datasets, ensuring that they are sorted the same way and
## therefore that the columns are being matched correctly.
if (all(df_ref_phase2_long$reporting.month == df_ref_phase2_long_sites$reporting.month) & 
    all(df_ref_phase2_long$`LSHTM subcomponent` == df_ref_phase2_long_sites$`LSHTM subcomponent`) & 
    all(df_ref_phase2_long$Level == df_ref_phase2_long_sites$Level)) {
  df_ref_phase2_long <- data.frame(df_ref_phase2_long_sites, Proportion = df_ref_phase2_long$Proportion, check.names = FALSE)
} else {
  warning("Error: mismatch in the column order of df_ref_phase2_long and df_ref_phase2_long_sites. 
One or more of the columns of reporting.month, `LSHTM subcomponent` or Level are not in the same order between the datasets.")
}





rm(df_surv_phase1_long_sites, df_surv_phase2_long_sites, df_ref_phase1_long_sites,df_ref_phase2_long_sites)

## Use the YYYYS# reporting.month date format to produce a function that produces
## date columns with the report date start and end:

date_conversion <- function(df) {
  df<-df %>% mutate(
    year = as.numeric(substr(reporting.month, 1, 4)),
    period = substr(reporting.month, 5, 6),
    `Start date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-01-01")),
      period == "Q2" ~ as.Date(paste0(year, "-04-01")),
      period == "Q3" ~ as.Date(paste0(year, "-07-01")),
      period == "Q4" ~ as.Date(paste0(year, "-10-01")),
      period == "S1" ~ as.Date(paste0(year, "-01-01")),
      period == "S2" ~ as.Date(paste0(year, "-07-01"))
    ),
    `End date` = case_when(
      period == "Q1" ~ as.Date(paste0(year, "-03-31")),
      period == "Q2" ~ as.Date(paste0(year, "-06-30")),
      period == "Q3" ~ as.Date(paste0(year, "-09-30")),
      period == "Q4" ~ as.Date(paste0(year, "-12-31")),
      period == "S1" ~ as.Date(paste0(year, "-06-30")),
      period == "S2" ~ as.Date(paste0(year, "-12-31"))
    )
  )%>%
    select(!c("year", "period"))
  
  return(df)
  
}

# Apply this function to the df_surv_long dataset.
df_surv_phase1_long <- date_conversion(df_surv_phase1_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))

df_surv_phase2_long <- date_conversion(df_surv_phase2_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))


# Conversion of ref dataset.
df_ref_phase1_long <- date_conversion(df_ref_phase1_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))

df_ref_phase2_long <- date_conversion(df_ref_phase2_long)%>%
  mutate(Level = factor(Level, levels = c("precore",
                                          "core",
                                          "extended",
                                          "advanced",
                                          "not_applicable",
                                          "no_answer")))


longer_surv_phase1_dates <- subset(df_surv_phase1_long, as.Date(`End date`) > "2024-01-01")
longer_surv_phase1_dates[as.Date(longer_surv_phase1_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_surv_phase1_dates[as.Date(longer_surv_phase1_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_surv_phase1_dates[as.Date(longer_surv_phase1_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")


df_surv_phase1_long_col <- rbind(df_surv_phase1_long, longer_surv_phase1_dates)

longer_surv_phase2_dates <- subset(df_surv_phase2_long, as.Date(`End date`) > "2024-01-01")
longer_surv_phase2_dates[as.Date(longer_surv_phase2_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_surv_phase2_dates[as.Date(longer_surv_phase2_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_surv_phase2_dates[as.Date(longer_surv_phase2_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")


df_surv_phase2_long_col <- rbind(df_surv_phase2_long, longer_surv_phase2_dates)



longer_ref_phase1_dates <- subset(df_ref_phase1_long, as.Date(`End date`) > "2024-01-01")
longer_ref_phase1_dates[as.Date(longer_ref_phase1_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_ref_phase1_dates[as.Date(longer_ref_phase1_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_ref_phase1_dates[as.Date(longer_ref_phase1_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")


df_ref_phase1_long_col <- rbind(df_ref_phase1_long, longer_ref_phase1_dates)

longer_ref_phase2_dates <- subset(df_ref_phase2_long, as.Date(`End date`) > "2024-01-01")
longer_ref_phase2_dates[as.Date(longer_ref_phase2_dates$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
longer_ref_phase2_dates[as.Date(longer_ref_phase2_dates$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
longer_ref_phase2_dates[as.Date(longer_ref_phase2_dates$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")


df_ref_phase2_long_col <- rbind(df_ref_phase2_long, longer_ref_phase2_dates)




## Plots demonstrating percentage of sites over time - surv phase 1

plot_surv_phase1 <- df_surv_phase1_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))

## Plots demonstrating numbers of sites over time

plot_surv_phase1_count <- df_surv_phase1_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_phase1_inverse <- ggplot(df_surv_phase1_long_col, aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_phase1_inverse_count <- ggplot(df_surv_phase1_long_col, aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



## Plots demonstrating percentage of sites over time - surv phase 1

plot_surv_phase2 <- df_surv_phase2_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))

## Plots demonstrating numbers of sites over time

plot_surv_phase2_count <- df_surv_phase2_long_col %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_phase2_inverse <- ggplot(df_surv_phase2_long_col, aes(x=as.Date(`End date`), y = Proportion, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Proportion of sites")+
  scale_y_continuous(labels = scales::percent)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))



plot_surv_phase2_inverse_count <- ggplot(df_surv_phase2_long_col, aes(x=as.Date(`End date`), y = Sites, fill = Level))+
  geom_col(width = 120)+
  facet_wrap(~ `LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  scale_fill_manual(values = c(advanced = "#FDE725FF",
                               extended = "#B6D443",
                               core = "#70C261",
                               precore = "#440154FF",
                               not_applicable = "grey"
  ),
  breaks=c('not_applicable','advanced', 'extended', 'core', 'precore'),
  labels = c(advanced = "Advanced",
             extended = "Extended",
             core = "Core",
             precore = "Precore",
             not_applicable = "Not applicable"
  ))+
  xlab("")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.y.title = element_text(size = 12))


## This graph was a step in the right direction, demonstrating how sites have collectively changed over time.
## However, since sites joined at different times, and reported at different times, and didn't all have
## the same final reporting date, there are too many data points to reasonably
## make sense of the trends and changes.

df_long %>%
  subset(type == "Surveillance" & value != "Not applicable" & total_time >= 60) %>%
  mutate(value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
  ggplot(aes(x=`Months in programme`, y = value)) +
  facet_wrap(~`LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  geom_count() +
  geom_line(aes(group = sitecode), alpha=0.2)


## Therefore, the following code simplifies this process, by distilling it down into
## three timepoints: start of phase 1, end of phase 1, end of phase 2. 
## This probably (definitely) needs sites who started in phase 2 to be filtered out and analysed
## separately.

df_long_phase1_start <- df_long %>%
  subset(Baseline < "2024-01-01") %>%
  arrange(`Start date`) %>%
  group_by(sitecode,`LSHTM subcomponent`) %>%
  filter(row_number() == 1) %>%
  mutate(timepoint = "Baseline")

df_long_phase1_middle <- df_long %>%
  subset(Baseline  < "2024-01-01") %>%
  arrange(`Start date`) %>%
  group_by(sitecode,`LSHTM subcomponent`) %>%
  filter(`End date` == "2023-12-31") %>%
  mutate(timepoint = "Middle")

df_long_phase1_end <- df_long %>%
  subset(Baseline < "2024-01-01") %>%
  arrange(`Start date`) %>%
  group_by(sitecode,`LSHTM subcomponent`) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(timepoint = "End")

df_long_phase1 <- rbind(df_long_phase1_start, df_long_phase1_middle, df_long_phase1_end)

plot_surv_change_site_level_phase1 <- df_long_phase1 %>%
  subset(type == "Surveillance" & value != "Not applicable" & timepoint != "Middle") %>%
  mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
         value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
  ggplot(aes(x= timepoint, y = value)) +
  facet_wrap(~`LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  geom_count() +
  geom_line(aes(group = sitecode), alpha=0.2)

  
## Looking at Phase 2 sites separately:
## These are the initial plots which informed the final product.
df_long_phase2_start <- df_long %>%
  subset(Baseline >= "2024-01-01") %>%
  arrange(`Start date`) %>%
  group_by(sitecode,`LSHTM subcomponent`) %>%
  filter(row_number() == 1) %>%
  mutate(timepoint = "Baseline")

df_long_phase2_end <- df_long %>%
  subset(Baseline >= "2024-01-01") %>%
  arrange(`Start date`) %>%
  group_by(sitecode,`LSHTM subcomponent`) %>%
  slice(n()) %>%
  ungroup() %>%
  mutate(timepoint = "End")

df_long_phase2 <- rbind(df_long_phase2_start, df_long_phase2_end)

plot_surv_change_site_level_phase2 <- df_long_phase2 %>%
  subset(type == "Surveillance" & value != "Not applicable") %>%
  mutate(timepoint = factor(timepoint, levels = c("Baseline", "End")),
         value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
  ggplot(aes(x= timepoint, y = value)) +
  facet_wrap(~`LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3)+
  geom_count() +
  geom_line(aes(group = sitecode), alpha=0.2)






# Compute segment counts for Baseline → End transitions
#### These are the more final product

segments_phase1 <- df_long_phase1 %>%
  subset(type == "Surveillance" & value != "Not applicable" & timepoint != "Middle") %>%
  mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
         value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
  arrange(sitecode, timepoint) %>%
  group_by(sitecode, `LSHTM subcomponent`) %>%
  mutate(next_timepoint = lead(timepoint),
         next_value = lead(value)) %>%
  ungroup() %>%
  filter(timepoint == "Baseline" & next_timepoint == "End") %>%
  group_by(`LSHTM subcomponent`, timepoint, value, next_timepoint, next_value) %>%
  summarise(count = n(), .groups = "drop") %>%
  mutate(direction = factor(case_when(
    as.numeric(next_value) > as.numeric(value) ~ "Increase",
    as.numeric(next_value) < as.numeric(value) ~ "Decrease",
    TRUE ~ "No change"
  ), levels = c("No change", "Decrease", "Increase")),
  timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
  nudge = ifelse(timepoint == "Baseline", -0.2, 0.2))  # Order for plotting


text <- df_long_phase1%>%
  subset(type == "Surveillance" & value != "Not applicable" & timepoint != "Middle") %>%
  mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
         value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
  arrange(sitecode, timepoint) %>%
  group_by(timepoint, `LSHTM subcomponent`, value) %>%
  summarise(count = n())

# Plot
plot_surv_change_site_level_phase1 <- ggplot() +
  facet_wrap(~`LSHTM subcomponent`,
             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3) +
  geom_segment(data = segments_phase1 %>% arrange(direction),
               aes(x = timepoint, y = value,
                   xend = next_timepoint, yend = next_value,
                   size = count,color = direction), alpha = 0.6,
               lineend = "round") +
  geom_count(data = df_long_phase1 %>%
               subset(type == "Surveillance" & value != "Not applicable" & timepoint != "Middle") %>%
               mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
                      value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))),
             aes(x = timepoint, y = value)) +
  scale_size_continuous(range = c(1, 7), name = "Number\nof sites") +
  scale_color_manual(values = c("Increase" = "lightblue", "Decrease" = "lightpink", "No change" = "grey80"), name = "Change\ndirection") +
  scale_y_discrete(name = c("Level"))+
  theme_bw()+
  scale_x_discrete(
    limits = c("Baseline", "End"),
    expand = c(0.2, 0.1),
    name = "Reporting timepoint",
    labels = c("Baseline" = "Phase 1\nbaseline", "End" = "Phase 2\nendline")
  )+
  geom_text(aes(label = count, y = value, x = timepoint),position = position_nudge(x = -0.21), data = subset(text, timepoint == "Baseline"),
            hjust = 0, size = 4)+
  geom_text(aes(label = count, y = value, x = timepoint),position = position_nudge(x = +0.1), data = subset(text, timepoint == "End"),
            hjust = 0, size = 4)+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title = element_text(size = 12),
        legend.title = element_text(size = 12))+
  guides(
    color = guide_legend(override.aes = list(linewidth = 3))   # For size legend
  )







######################## Graphs with phase 1 endline!!! 


###### ABANDONED BECAUSE IT IS MORE CONFUSING RATHER THAN HELPFUL


# Compute segments for Baseline → Middle and Middle → End
#segments_phase1_ver2 <- df_long_phase1 %>%
#  subset(type == "Surveillance" & value != "Not applicable") %>%
#  mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
#         value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))) %>%
#  arrange(sitecode, timepoint) %>%
#  group_by(sitecode, `LSHTM subcomponent`) %>%
#  mutate(next_timepoint = lead(timepoint),
#         next_value = lead(value)) %>%
#  ungroup() %>%
#  filter(!is.na(next_timepoint)) %>%  # Keep both Baseline→Middle and Middle→End
#  group_by(`LSHTM subcomponent`, timepoint, value, next_timepoint, next_value) %>%
#  summarise(count = n(), .groups = "drop") %>%
#  mutate(direction = factor(case_when(
#    as.numeric(next_value) > as.numeric(value) ~ "Increase",
#    as.numeric(next_value) < as.numeric(value) ~ "Decrease",
#    TRUE ~ "No change"
#  ), levels = c("No change", "Decrease", "Increase")))  # Order for plotting

# Plot with both segments
#plot_surv_change_site_level_phase1_ver2 <- ggplot() +
#  facet_wrap(~`LSHTM subcomponent`,
#             labeller = labeller(`LSHTM subcomponent` = custom_labels), nrow = 3) +
#  geom_segment(data = segments_phase1_ver2 %>% arrange(direction),
#               aes(x = timepoint, y = value,
#                   xend = next_timepoint, yend = next_value,
#                   size = count, color = direction), alpha = 0.6,
#               lineend = "round") +
#  geom_count(data = df_long_phase1 %>%
#               subset(type == "Surveillance" & value != "Not applicable") %>%
#               mutate(timepoint = factor(timepoint, levels = c("Baseline", "Middle", "End")),
#                      value = factor(value, levels = c("Precore", "Core", "Extended", "Advanced"))),
#             aes(x = timepoint, y = value)) +
#  scale_size_continuous(range = c(1, 7)) +
#  scale_color_manual(values = c("Increase" = "lightblue", "Decrease" = "lightpink", "No change" = "grey80"))+
#  scale_y_discrete(name = c("Level"))+
#  theme_bw()+
#  scale_x_discrete(expand = c(0.2,0.1),name = "Reporting timepoint", labels = c("Baseline" = "Phase 1\nbaseline", "Middle" = "Phase 1\nendline",
#                                                               "End" = "Phase 2\nendline"))+
#  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
#        legend.text = element_text(size = 12), axis.title = element_text(size = 12),
#        legend.title = element_text(size = 12))
  

