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
df <- read_xlsx("Data/1.a HH LSHTM Roadmap status non-uploaded_version.xlsx") %>%
  mutate(Country = case_when(
    sitecode %like% c("BDL%") ~ "Bangladesh",
    sitecode %like% c("BTL%") ~ "Bhutan",
    sitecode %like% c("SZL%") ~ "Eswatini",
    sitecode %like% c("GHL%") ~ "Ghana",
    sitecode %like% c("IDL%") ~ "Indonesia",
    sitecode %like% c("KEL%") ~ "Kenya",
    sitecode %like% c("LAL%") ~ "Laos",
    sitecode %like% c("MWL%") ~ "Malawi",
    sitecode %like% c("NPL%") ~ "Nepal",
    sitecode %like% c("NGL%") ~ "Nigeria",
    sitecode %like% c("PKL%") ~ "Pakistan",
    sitecode %like% c("PGL%") ~ "PNG",
    sitecode %like% c("RWL%") ~ "Rwanda",
    sitecode %like% c("SNL%") ~ "Senegal",
    sitecode %like% c("SLL%") ~ "Sierra Leone",
    sitecode %like% c("LKL%") ~ "Sri Lanka",
    sitecode %like% c("TZL%") ~ "Tanzania",
    sitecode %like% c("TLL%") ~ "Timor-Leste",
    sitecode %like% c("UGL%") ~ "Uganda",
    sitecode %like% c("VNL%") ~ "Vietnam",
    sitecode %like% c("ZML%") ~ "Zambia",
    sitecode %like% c("ZWL%") ~ "Zimbabwe"
  )) %>%
  subset(Country == "Pakistan")


## Load in site masterlist to obtain site information:
site_info <- read_xlsx("~/M&E/2025Q3/On-going updates_Master List of Sentinel Sites_July 2025.xlsx", sheet = "Country_List of Sentinel Sites")

#################################### Graph output location
## Choose the location into which you would like your HH LSHTM Roadmap graphs
## to go into.
file_save <- "Figures/Malawi"


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



########################### Attempt 1: Calibrated dataset with noise added at each timepoint

## First, generate a version of the datatable that records the total number of sites
## in each quarter for each subcomponent

site_variation <- 0.25
prop_variation <- 0.25
synthetic_site_total <- 30
synthetic_site_range <- 2

## This step was chosen to produce a dynamic number of sites based on the
## total number of sites from each country, but instead I think I will choose a
## synthetic total to disguise totals.
#df1 <- df_surv_phase1_long %>%
#  group_by(`LSHTM subcomponent`, reporting.month) %>%
#  summarize(sites = sum(Sites))


## Prepare a column for the noise
#df1$noise <- NA

## Next, generate a random noise matrix, where each row has random noise that is +/- 35%
#for (i in 1:nrow(df1)){
#  ## Set a random seed for each row
#  set.seed(runif(n=1, min=1, max=1e+09))
#  
#  ## Generate a random number for that row that is between -0.35 and 0.35
#  df1[i, "noise"] <- runif(n=1, min=-site_variation, max=site_variation)
#}

## Calculate the adjusted values.
#df1$adj.sites <- abs(df1$sites * (1+df1$noise))

## Adjust to be integers
#df1$adj.sites.int <- as.integer(df1$adj.sites)

df1 <- df_surv_phase1_long 

set.seed(runif(n=1, min=1, max=1e+09))

df1 <- df1 %>%
  group_by(reporting.month) %>%
  mutate(adj.sites.int = round(runif(n=1, min=synthetic_site_total-synthetic_site_range, max=synthetic_site_total+synthetic_site_range)))


## Generate a dataframe for the proportions of each subcomponent level
df2 <- df_surv_phase1_long

df2$noise <- NA

## Next, generate a random noise matrix, where each row has random noise that is +/- 15%
for (i in 1:nrow(df2)){
  
  if(df2[i, "Proportion"] == 0 & df2[i, "Level"] != "not_applicable") {
    
    ## If the proportion is 0 for that level, set a random chance that the
    ## proportion is randomly increased
    x <- round(runif(n=1, min=1, max=10))
    if (x == 10){
      df2[i, "Proportion"] <- runif(n=1, min=0, max=0.25)
    } else {
      df2[i, "Proportion"] <- runif(n=1, min=0, max=0.1)
    }
  }
  
  if(df2[i, "Proportion"] == 1) {
    df2[i, "Proportion"] <- runif(n=1, min=0.9, max=1)
  }
  
  ## Generate a random number for that row that is between -0.15 and 0.15
  df2[i, "noise"] <- runif(n=1, min=-prop_variation, max=prop_variation)
}

## Calculate the adjusted values.
df2$adj.prop <- abs(df2$Proportion * (1+df2$noise))
df2$total_prop <- NA

## Work out the total proportion per subcomponent per reporting date
for (date in unique(df2$reporting.month)) {
  for (subcomponent in unique(df2$`LSHTM subcomponent`)) {
    df2[df2$reporting.month == date & df2$`LSHTM subcomponent` == subcomponent, "total_prop"] <- sum(df2[df2$reporting.month == date & df2$`LSHTM subcomponent` == subcomponent, "adj.prop"])
  
    ## Also, take the opportunity to include the adj.total sites
    df2[df2$reporting.month == date & df2$`LSHTM subcomponent` == subcomponent, "adj.tot.sites.int"] <- df1[df1$reporting.month == date & df1$`LSHTM subcomponent` == subcomponent, "adj.sites.int"]
    
    }
}

## Standardise the adjusted proportions to this new total to get it back to a sum of 1
df2$std.adj.prop <- df2$adj.prop/df2$total_prop

## Now work out the integer of the total sites for each level for each subcomponent and quarter
df3 <- df2 %>%
  mutate(adj.sites.int = as.integer(std.adj.prop*adj.tot.sites.int)) %>%
  select(1:3,10) %>%
  date_conversion()


df3_col <-subset(df3, as.Date(`End date`) > "2024-01-01")
df3_col[as.Date(df3_col$`End date`) == "2024-06-30", "End date"] <- as.Date("2024-03-31")
df3_col[as.Date(df3_col$`End date`) == "2024-12-31", "End date"] <- as.Date("2024-09-30")
df3_col[as.Date(df3_col$`End date`) == "2025-06-30", "End date"] <- as.Date("2025-03-31")
df3_col[as.Date(df3_col$`End date`) == "2025-12-31", "End date"] <- as.Date("2025-09-30")


df3_col_extra <- rbind(df3, df3_col)

plot <- df3_col_extra %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x=as.Date(`End date`), y = adj.sites.int, fill = Level))+
  geom_col(width = 92)+
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











########################### Attempt 2: Calibrated dataset with noise added to the difference between timepoints

## Set the degree of proportional difference variation you would like to see.

dif_variation <- 1
synthetic_site_total <- 30
synthetic_site_range <- 2

set.seed(runif(n=1, min=1, max=1e+09))

## In preparation to obscure the reporting month, convert the reporting months
## to time values.
## Also, create a new dataframe.

df_dif1 <- df_surv_phase1_long %>%
  mutate(reporting.month = factor(reporting.month, levels = c(#"2020Q2","2020Q3",
                                                              "2020Q4",
                                   "2021Q1", "2021Q2", "2021Q3","2021Q4","2022Q1","2022Q2",
                                   "2022Q3","2022Q4","2023Q1","2023Q2","2023Q3","2023Q4",
                                   "2024S1", "2024S2", "2025S1","2025S2"))) %>%
  mutate(reporting.month = as.numeric(reporting.month))

## Create a new one (in case we need df_dif1 values at any point). Add new columns
df_dif2 <- df_dif1 %>%
  mutate(prev_prop = NA,
         dif = NA)

## For all values after timepoint 1, work out what the previous proportion for each level was.
for (i in unique(df_dif2$reporting.month)) {
  if (i - 1 != 0){
    for (component in unique(df_dif2$`LSHTM subcomponent`)){
      for (level in unique(df_dif2$Level)) {
        
        df_dif2[df_dif2$reporting.month == i &
                  df_dif2$`LSHTM subcomponent` == component &
                  df_dif2$Level == level,"prev_prop"] <- df_dif2[df_dif2$reporting.month == (i-1) &
                                                                   df_dif2$`LSHTM subcomponent` == component &
                                                                   df_dif2$Level == level,"Proportion"]
        
      }
    }
  }
}

## Calculate the difference between this timepoint's proportion and this one.
df_dif2$dif <-  df_dif2$Proportion - df_dif2$prev_prop

## Prep the noise column.
df_dif2$noise <- NA

## Set a random noise value for each row, within the range of the previously set dif_variation.
for (i in 1:nrow(df_dif2)){
  
  ## Generate a random number for that row that is between -0.35 and 0.35
  df_dif2[i, "noise"] <- runif(n=1, min=-dif_variation, max=dif_variation)
}

## Multiply the difference between the proportions of t and t-1 by the noise factor.
df_dif2$modified_dif <- df_dif2$dif * (1+df_dif2$noise)

## Change the value(t) (proportiong at current timepoint) according to the modified difference,
## using the value of the previous quarter.

df_dif2$modified_Proportion <- df_dif2$prev_prop + df_dif2$modified_dif

## As t0 is so far not modified, manually modify it to make it at least have a chance of
## being unrecognisable, as in many instances, t0 is 100% precore, which changes totally in
## the next timepoint.
for (i in 1:nrow(df_dif2)){
  if (df_dif2[i, "reporting.month"] == 1 ){
    df_dif2[i, "modified_Proportion"] <- df_dif2[i, "Proportion"] * (1+df_dif2[i, "noise"])
  }
}

## Ensure no modified_Proportions go below 0
df_dif2[df_dif2$modified_Proportion < 0, "modified_Proportion"] <- 0

## Work out total modified_Proportion per timepoint
df_dif2$total_Prop <- NA
for (date in unique(df_dif2$reporting.month)) {
  for (subcomponent in unique(df_dif2$`LSHTM subcomponent`)) {
    df_dif2[df_dif2$reporting.month == date & df_dif2$`LSHTM subcomponent` == subcomponent, "total_Prop"] <- sum(df_dif2[df_dif2$reporting.month == date & df_dif2$`LSHTM subcomponent` == subcomponent, "modified_Proportion"])
    
  }
}

df_dif2$normalised_Prop <- df_dif2$modified_Proportion/df_dif2$total_Prop


df_dif2 <- df_dif2 %>%
  group_by(reporting.month) %>%
  mutate(adj.sites.int = round(runif(n=1, min=synthetic_site_total-synthetic_site_range, max=synthetic_site_total+synthetic_site_range)))

## Now work out the integer of the total sites for each level for each subcomponent and quarter
df_dif3 <- df_dif2 %>%
  mutate(adj.sites.int = as.integer(normalised_Prop*adj.sites.int))

#extra1 <- df_dif3[df_dif3$reporting.month %in% c(16,17,18,19),]
#extra2 <- df_dif3[df_dif3$reporting.month %in% c(16,17,18,19),]

#extra1[extra1$reporting.month == 19,"reporting.month"] <- 23
#extra2[extra2$reporting.month == 19,"reporting.month"] <- 22
#extra1[extra1$reporting.month == 18,"reporting.month"] <- 21
#extra2[extra2$reporting.month == 18,"reporting.month"] <- 20
#extra1[extra1$reporting.month == 17,"reporting.month"] <- 19
#extra2[extra2$reporting.month == 17,"reporting.month"] <- 18
#extra1[extra1$reporting.month == 16,"reporting.month"] <- 17

#df_dif4 <- df_dif3 %>%
#  subset(reporting.month<=15)

#df_dif5 <- rbind(df_dif4, extra1,extra2)

plot_dif_pakistan_collection <- df_dif3 %>%
  subset(`LSHTM subcomponent` %in% c("tier1a","tier2d","tier3a","tier4a")) %>%
  mutate(Level = factor(Level, levels = c("not_applicable","advanced","extended","core","precore"))) %>%
  ggplot(aes(x= reporting.month, y = adj.sites.int, fill = Level))+
  geom_col(width = 1)+
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
  xlab("Reporting timepoint")+
  ylab("Number of sites")+
  theme(axis.text = element_text(size = 12), strip.text = element_text(size = 12),
        legend.title = element_text(size = 12),
        legend.text = element_text(size = 12), axis.title = element_text(size = 12))

plotx <- plot_dif_malawi + plot_dif_pakistan + plot_layout(axis_titles = "collect", guides = "collect")
plot4a <- plot_dif_malawi_4a + plot_dif_pakistan_4a + plot_layout(axis_titles = "collect", guides = "collect")+
  plot_annotation(tag_levels = list(c("a.","b.")))
plotcollection <- plot_dif_malawi_collection + plot_dif_pakistan_collection + plot_layout(axis_titles = "collect", guides = "collect")+
  plot_annotation(tag_levels = list(c("a.","b.")))

ggsave("Figures/Drafts/Figure2_4a.png", plot4a, height = 3, width = 8)
ggsave("Figures/Drafts/Figure2_2d.png", plotx, height = 3, width = 8)
ggsave("Figures/Drafts/Figure2_collection.png", plotcollection, height = 5.5, width = 14)
