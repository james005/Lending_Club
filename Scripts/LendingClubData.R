# ###################################################################
# Libraries and constants
# ###################################################################
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyr)
library(rpart)
library(rpart.plot)
library(gridExtra)



WORKING_DIR <- "/home/rstudio/"

setwd(WORKING_DIR)
options(stringsAsFactors = F)

source(paste0(getwd(), "/Lending_Club/Scripts/LendingClubFunctions.R"))
# ###################################################################
# Loading data
# ###################################################################
project_files <- list.files("Data/Lending_Club/")

# If there is already RData file then omit reading csv
if("loan.RData" %in% project_files){
	load("Data/Lending_Club/loan.RData")
	gc(reset = T)
} else {
	# Read csv in chunks
	data <- loading_data("Data/loan.csv", rows_number = 5 * 10^4)
	save(file = "Data/loan.RData", data)
	gc(reset = T)
}

# ###################################################################
# Modification of data
# ###################################################################
data <- data %>%
	mutate_all(funs(ifelse(. %in% c("<NA>", ""), NA, .))) %>%
	mutate(default = ifelse(loan_status %in% c("Charged Off", "Default", 
											   "Does not meet the credit policy. Status:Charged Off",
											   "Late (31-120 days)"), 1, 0)) %>%
	separate(issue_d, into = c("issue_m", "issue_y"), sep = "-", remove = F) %>%
	mutate(issue_y = issue_y %>% as.character())

# ###################################################################
# Statistics and descriptive analysis
# ###################################################################
# Theme
theme_main <- theme_bw()+
	theme(plot.title = element_text(size=14, face='bold'))

plots_list <- list()

# Main statistics
stats_default <- data %>%
	dplyr::select(default) %>%
	group_by(default) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2),
		   default = default %>% as.factor())

plot_main <- ggplot(data = stats_default)+
	geom_col(aes(x = default, y = perc), fill = "gray70")+
	geom_text(aes(x = default, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Default")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 100))+
	ggtitle(label = "Default ratio")+
	theme_main

ggsave(plot_main, file = "Lending_Club/Plots/default_ratio.png", dpi = 100, width = 6, height = 4)


# Loans by year
stats_year <- data %>%
	group_by(issue_y, default) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2),
		   default = default %>% as.factor())

plot_1 <- ggplot(data = subset(stats_year, default == "1"))+
	geom_col(aes(x = issue_y, y = perc), fill = "red3")+
	geom_text(aes(x = issue_y, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Issue year")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 30))+
	ggtitle(label = "Default ratio by year")+
	theme_main

stats_year <- data %>%
	group_by(issue_y) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

plot_2 <- ggplot(data = stats_year)+
	geom_col(aes(x = issue_y, y = perc), fill = "gray70")+
	geom_text(aes(x = issue_y, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Issue year")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 25))+
	ggtitle(label = "Loans by year")+
	theme_main

plot_years <- gridExtra::grid.arrange(plot_1, plot_2, ncol = 1, nrow = 2)
ggsave(plot_years, file = "Lending_Club/Plots/years.png", dpi = 100, width = 8, height = 6)

# Purpose
stats_purpose <- data %>%
	group_by(purpose, default) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

plot_1 <- ggplot(data = subset(stats_purpose, default == "1"))+
	geom_col(aes(x = purpose, y = perc), fill = "red3")+
	geom_text(aes(x = purpose, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Purpose")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 28))+
	ggtitle(label = "Default ratio by purpose")+
	theme_main+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

stats_purpose <- data %>%
	group_by(purpose) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

plot_2 <- ggplot(data = stats_purpose)+
	geom_col(aes(x = purpose, y = perc), fill = "gray70")+
	geom_text(aes(x = purpose, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Purpose")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 70))+
	ggtitle(label = "Loans by purpose")+
	theme_main+
	theme(axis.text.x = element_text(angle = 45, hjust = 1))

plot_purpose <- gridExtra::grid.arrange(plot_1, plot_2, ncol = 1, nrow = 2)
ggsave(plot_purpose, file = "Lending_Club/Plots/purpose.png", dpi = 100, width = 8, height = 6)

# Hardship directly signifies that a person does not pay debt
stats_hardship <- data %>%
	group_by(hardship_flag, default) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

# States
stats_states <- data %>%
	group_by(addr_state, default) %>%
	summarise(counts = n()) %>%
	mutate(perc_def = round((counts / sum(counts)) * 100, 2)) %>%
	ungroup() %>%
	filter(default == 1) %>%
	arrange(desc(perc_def)) %>%
	left_join(data %>%
			  	group_by(addr_state) %>%
			  	summarise(counts = n()) %>%
			  	mutate(perc_all = round((counts / sum(counts)) * 100, 2)) %>%
			  	dplyr::select(-counts), by = "addr_state") %>%
	filter(perc_all > 1)

# I select 5 states with lowest default ratio and 5 with highest
# I filtered out states where was issued less than 1% of loans
selected_states <- stats_states[["addr_state"]][c(seq(1, 5), seq(nrow(stats_states), nrow(stats_states)-4))]

# Grades
stats_grade <- data %>%
	group_by(grade, default) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

plot_1 <- ggplot(data = subset(stats_grade, default == "1"))+
	geom_col(aes(x = grade, y = perc), fill = "red3")+
	geom_text(aes(x = grade, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Grade")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 45))+
	ggtitle(label = "Default ratio by grade")+
	theme_main

stats_grade <- data %>%
	group_by(grade) %>%
	summarise(counts = n()) %>%
	mutate(perc = round((counts / sum(counts)) * 100, 2))

plot_2 <- ggplot(data = stats_grade)+
	geom_col(aes(x = grade, y = perc), fill = "gray70")+
	geom_text(aes(x = grade, y = perc, label = perc), vjust = -1)+
	scale_x_discrete(name = "Grade")+
	scale_y_continuous(name = "Percentage (%)", limits = c(0, 35))+
	ggtitle(label = "Loans by grade")+
	theme_main

plot_grades <- gridExtra::grid.arrange(plot_1, plot_2, ncol = 1, nrow = 2)
ggsave(plot_grades, file = "Lending_Club/Plots/grades.png", dpi = 100, width = 8, height = 6)

# ###################################################################
# Filtering out NA columns from data
# ###################################################################
count_na <- function(x) sum(is.na(x)) / length(x)
stats_na <- data %>%
	summarise_all(funs(count_na)) %>%
	gather(key = "column", value = "NA_perc") %>%
	arrange(desc(NA_perc))
gc(reset = T)

# I filter out columns which have more than 90% of NAs - it is case of having
# a hardship settlement or a co-borrower. 
# There are also 3 columns fully empty: id, member_id and url.
columns_na <- stats_na %>%
	filter(NA_perc > 0.9) %>%
	.[, 1] %>%
	as.character()
columns_na <- which(colnames(data) %in% columns_na)

data <- data[, -columns_na]
gc(reset = T)

# ###################################################################
# Adding new variables
# ###################################################################
# Extract 10 most popular employment titles
emp_titles_top <- data[["emp_title"]] %>% table() %>% sort(., decreasing = T) 
emp_titles_top <- emp_titles_top %>% head(n = 10) %>% names()
data <- extract_given_categories(data, "emp_title", emp_titles_top)

# Add dummy variables about selected states
data <- extract_given_categories(data, "addr_state", selected_states)

# Calculate number of months from issue to the last month
data <- data %>%
	mutate(issue_d = issue_d %>% paste0(., "-01") %>% as.Date(., format = "%b-%Y-%d") %>% zoo::as.yearmon(),
		   last_pymnt_d = last_pymnt_d %>% paste0(., "-01") %>% as.Date(., format = "%b-%Y-%d") %>% zoo::as.yearmon(),
		   last_month = "Feb-2019" %>% paste0(., "-01") %>% as.Date(., format = "%b-%Y-%d") %>% zoo::as.yearmon(),
		   time_from_issue = round((last_month - issue_d)*12, 0),
		   time_from_issue = ifelse(time_from_issue > 36 & term == " 36 months", 36,
		   						 ifelse(time_from_issue > 60 & term == " 60 months", 60, time_from_issue)))

# Add nonlinearity and modify dti
data <- data %>%
	mutate(dti = ifelse(dti <= 0, 0.01, dti),
		   dti_log = log(dti),
		   dti_pow_2 = dti^2,
		   time_from_issue_log = log(time_from_issue),
		   time_from_issue_pow_2 = time_from_issue^2,
		   int_rate_log = log(int_rate),
		   int_rate_pow_2 = int_rate^2)

# ###################################################################
# Distribution plots
# ###################################################################
# DTI
plot_dti <- ggplot(data = data)+
	geom_density(aes(x = dti, y = ..density.., color = as.factor(default)))+
	scale_x_continuous(name = "DTI", limits = c(0, 50))+
	scale_y_continuous(name = "Density")+
	scale_color_manual(name = "Default", values = c("gray70", "red3"))+
	ggtitle(label = "Distribution of DTI")+
	theme_main

# Time from issue
plot_time <- ggplot(data = data)+
	geom_density(aes(x = time_from_issue, y = ..density.., color = as.factor(default)))+
	scale_x_continuous(name = "Time from issue")+
	scale_y_continuous(name = "Density")+
	scale_color_manual(name = "Default", values = c("gray70", "red3"))+
	ggtitle(label = "Distribution of time from issue")+
	theme_main

plot_dist <- grid.arrange(plot_dti, plot_time, ncol = 1, nrow = 2)
ggsave(plot_dist, file = "Lending_Club/Plots/distributions.png", dpi = 100, width = 8, height = 6)

# ###################################################################
# Dropping variables
# ###################################################################
# I filter out columns about last payment, last credit pull, hardship,
# next payment date, second applicant, settlement,
# loan status, zip code, funded amount, 
data <- data %>%
	dplyr::select(-contains("last_credit"), -contains("last_pymnt"), -contains("hardship"), 
				  # -contains("mths_since"), -contains("mo_sin"), 
				  -contains("sec_app"), -contains("last_month"),
				  -contains("settlement"), -contains("next_pymnt"), -contains("loan_status"),
				  -contains("zip_code"), -contains("funded_amnt"), -contains("collection_recovery_fee"),
				  -contains("earliest_cr_line"), -title, 
				  -contains("issue_d"), -contains("issue_m"), -contains("total_rec"),
				  -contains("out_prncp"), -contains("recoveries"), -contains("total_pymnt"),
				  -contains("policy_code"), -grade)

# ###################################################################
# Spliting data into train and test sets
# ###################################################################
set.seed(1)
train_rows <- sample(x = seq(1, nrow(data)), size = ceiling(nrow(data) * 0.7))
train_data <- data[train_rows, ]
test_data <- data[-train_rows, ]

# ###################################################################
# Decision Tree
# ###################################################################
set.seed(1)
train_data_subsample <- train_data %>%
	sample_frac(size = 0.2)
tree <- rpart(default~.,
			  data = train_data_subsample,
			  control = rpart.control(cp = 0.005))
png("Lending_Club/Plots/rpart.png", width = 800, height = 500)
rpart.plot(tree)
dev.off()

# ###################################################################
# Modify data - transform all variables to numeric, impute NAs and delete colinearity
# ###################################################################
set.seed(2)
train_data <- train_data %>%
	filter(!is.na(default)) %>%
	impute_na() %>%
	split_categorical(., min_occurrence = 0.01, max_representation = 0.95) %>%
	.[base::sample(nrow(.)),] # Shuffle DF so that it is suitable for bigglm

# Delete colinearity
train_data <- cor_filter(train_data, max_cor = 0.95)

# Modify test data
test_data <- test_data %>%
	filter(!is.na(default)) %>%
	impute_na() %>%
	split_categorical_test_data(., colnames(train_data))

test_data <- test_data[, colnames(train_data)] # Append columns' order

# ###################################################################
# Save data
# ###################################################################
save(train_data, file = "Data/Lending_Club/train_data.RData")
save(test_data, file = "Data/Lending_Club/test_data.RData")
save(plots_list, file = "Data/Lending_Club/Plots.RData")

