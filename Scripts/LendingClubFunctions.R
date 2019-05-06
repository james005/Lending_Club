# Loading csv data
loading_data <- function(directory = "Data/loan.csv", rows_number = 10^3){
	#' This function loads data in chunks
	#' 
	#' @param directory String with directory of file to load
	#' @param rows_number Number of rows to load in one chunk
	iter <- 1
	con <- file(directory, "r")
	col_names <- read.csv(con, nrows = 1, encoding = "UTF-8", header = F) %>% 
		.[1,] %>%
		as.character()
	
	df <- read.csv(con, nrows = rows_number, encoding = "UTF-8", header = F, 
				   col.names = col_names, sep = ",")
	
	new_lines <- nrow(df) == rows_number
	while(new_lines){
		df_new <- read.csv(con, nrows = rows_number, encoding = "UTF-8", header = F, 
						   col.names = col_names, sep = ",")
		df <- bind_rows(df, df_new)
		iter <- iter + 1
		if(!(iter %% 10)) print(iter)
		new_lines <- nrow(df_new) == rows_number
	}
	
	close(con)
	return(df)
}

# ###################################################################
# Function to impute NAs
# ###################################################################
# TO DO 
# Do a function that in place of NA put mean or median, depending whether
# a column is numeric or factor/string

impute_na <- function(data){
	col_names <- colnames(data)
	col_types <- sapply(data, typeof)
	for(col in col_names){
		# Impute only if there is any NA
		if(any(is.na(data[[col]]))){
			# If column is of double type then inpute mean
			if(col_types[col] == "double"){
				na_index <- which(is.na(data[[col]]))
				new_col <- paste0(col, "_NA")
				data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = T), data[[col]])
				data[[new_col]] <- 0L
				data[[new_col]][na_index] <- 1L
				
				# If column is of integer type then inpute median
			} else if(col_types[col] == "integer"){
				na_index <- which(is.na(data[[col]]))
				new_col <- paste0(col, "_NA")
				# data[[col]] <- ifelse(is.na(data[[col]]), median(data[[col]], na.rm = T), data[[col]])
				data[[col]] <- ifelse(is.na(data[[col]]), mean(data[[col]], na.rm = T), data[[col]])
				data[[new_col]] <- 0L
				data[[new_col]][na_index] <- 1L
			} else {
				data[[col]] <- ifelse(is.na(data[[col]]), "NA", data[[col]])
			}
		}
	}
	return(data)
}

new_dummy_col <- function(data, category_name, column_name){
	#' This function creates a new dummy variable for given category
	#' @param data Data Frame with data
	#' @param cat_name String with name of category
	#' @param column_name String with name of column which contains categories
	new_name <- paste0(column_name, ".", category_name) %>%
		gsub(., pattern = " ", replacement = ".") %>%
		gsub(., pattern = "/", replacement = "") %>%
		gsub(., pattern = "<", replacement = "lt")
	data[[new_name]] <- ifelse(data[[column_name]] == category_name, 1, 0)
	return(data)
}

extract_given_categories <- function(data, col_name, categories){
	#' This function extracts given categories from a particular column
	#' @param data Data Frame of data
	#' @param col_name String with name of column
	#' @param categories Character vector of categories to extract to dummmy variables
	if(length(categories)){
		for(cat_name in categories){
			data <- new_dummy_col(data, cat_name, col_name)
		}
	} else {
		stop("There are no categories in the given vector!")
	}
	# Drop original column
	data[[col_name]] <- NULL
	return(data)
}


split_categorical <- function(data, min_occurrence = 0.001, max_representation = 0.99){
	#' This function changes factor/strings column into binary columns, but ommit too
	#' rare levels (default minimum presence in set is 0.001)
	#' @param data Data Frame which should be modified
	#' @param min_occurence Minimum frequency that allow to include level
	#' @param min_representation Maximum sum of frequencies of selected categories. If the sum is larger than maximum
	#' then the most frequent category is dropped.
	col_names <- colnames(data)
	col_types <- sapply(data, typeof)
	categoricals <- which(!col_types %in% c("double", "integer"))
	min_occurrence <- min_occurrence * nrow(data)
	max_representation <- max_representation * nrow(data)
	
	for(col in col_names[categoricals]){
		levels_tab <- table(data[[col]])
		# Include all levels and drop variables alphabetically
		# levels_tab <- levels_tab[-1]
		levels_tab <- levels_tab[which(levels_tab > min_occurrence)]
		# If the sum of frequencies is larger than limit then drop the most frequent category
		# This is nested loop -algorithm iteratively drop the most frequent levels - it can be
		# checked what would be in the other way round - with dropping the least frequent levels
		while(max_representation < sum(levels_tab)){
			levels_tab <- levels_tab[-which.max(levels_tab)]
		}
		
		# If there left a least 1 category then create dummy variable for selected levels 
		if(length(levels_tab)){
			for(cat_name in names(levels_tab)){
				data <- new_dummy_col(data, cat_name, col)
			}
		}
		# Drop original column
		data[[col]] <- NULL
	}
	return(data)
}

split_names <- function(x){
	x <- strsplit(x, split = ".", fixed = T)
	return(x[[1]][[1]])
}

split_categorical_test_data <- function(data, train_cols){
	#' This function split categorical variables of test data.
	#' Here all levels are extracted
	#' @param data Data Frame of test data
	#' @param train_cols Vector of strings with names of train data
	col_names <- colnames(data)
	raw_train_cols <- sapply(train_cols, split_names) %>%
		unique()
	used_cols <- which(col_names %in% raw_train_cols)
	data <- data[used_cols]
	
	col_names <- colnames(data)
	col_types <- sapply(data, typeof)
	categoricals <- which(!col_types %in% c("double", "integer"))
	col_names <- col_names[categoricals]
	col_names <- col_names[which(col_names %in% raw_train_cols)]
	
	for(col in col_names){
		levels_tab <- table(data[[col]])
		for(cat_name in names(levels_tab)){
			new_name <- paste0(col, ".", cat_name) %>%
				gsub(., pattern = " ", replacement = ".") %>%
				gsub(., pattern = "/", replacement = "") %>%
				gsub(., pattern = "<", replacement = "lt")
			if(new_name %in% train_cols){
				data[[new_name]] <- ifelse(data[[col]] == cat_name, 1, 0)
			}
		}
		# Drop original column
		data[[col]] <- NULL
	}
	# Check if anything was omitted if yes then add 0 column, because
	# column can be omitted only if there is no observation in particular category
	missing_cols <- train_cols[-which(train_cols %in% colnames(data))]
	if(length(missing_cols)){
		for(miss in missing_cols){
			data[[miss]] <- 0
		}
	}
	return(data)
}

# ###################################################################
# Function to avoid colinearity by dropping one of correlated variables
# ###################################################################

cor_filter <- function(data, max_cor = 0.99){
	#' This function filter out variables that are idealy linear to each other
	#' @param data Data Frame of numeric variables
	#' @param max_cor Maximum value of correlation that is accepted
	cor_mat <- cor(data)
	# ut <- upper.tri(cor_mat)
	ut <- upper.tri(cor_mat) | lower.tri(cor_mat)
	cor_df <- data.frame(
		row = rownames(cor_mat)[row(cor_mat)[ut]],
		column = rownames(cor_mat)[col(cor_mat)[ut]],
		cor  = abs((cor_mat)[ut])
	) %>%
		mutate(cor = ifelse(is.na(cor), 1, cor)) %>%
		# arrange(desc(cor), row, column) %>%
		arrange(row, column) %>%
		filter(cor >= max_cor)
	
	# Drop correleted variables
	iter <- 1
	while(iter < nrow(cor_df)){
		v_name <- cor_df[["column"]][iter]
		cor_df <- cor_df %>%
			filter(row != v_name)
		data[[v_name]] <- NULL
		iter <- iter + 1
	}
	return(data)
}

# ###################################################################
# Eliminating outliers
# ###################################################################

eliminate_outliers <- function(data, max_std = 3, show_logs = FALSE){
	#' This function eliminates outliers by droping observations which at least in one
	#' variable exceeds 3 standard deviation. This is done sequentialy, so the results
	#' depends on the order of columns.
	#' @param data Data Frame with numeric variables
	#' @param max_std Numeric value of maximum standard deviance
	#' @param show_logs Logic variables, when TRUE then function prints logs
	col_names <- colnames(data)
	rows_num_0 <- nrow(data)
	for(col in col_names){
		if(min(data[[col]]) == 0 & max(data[[col]]) == 1) next
		if(sum(data[[col]] == 0) > 0.3*nrow(data)) next
		col_mean <- mean(data[[col]], na.rm = T)
		col_std <- mean(data[[col]], na.rm = T)
		col_min <- col_mean - col_std*max_std
		col_max <- col_mean + col_std*max_std
		if(show_logs){
			print(paste0("Variable: ", col))
			print(paste0(col_min, " - ", col_max))
			print(paste0("Min: ", min(data[[col]]), "; Max: ", max(data[[col]])))
		}
		data <- data %>%
			filter(((!!sym(col)) >= col_min) & ((!!sym(col)) <= col_max))
	}
	num_dropped <- rows_num_0 - nrow(data)
	if(num_dropped){
		message(paste0("Number of dropped observations: ", num_dropped))
	} else {
		message("None of observation was dropped!")
	}
	return(data)
}

# ###################################################################
# Function for parameters tunning in XGBoost
# ###################################################################
tune_xgb <- function(data, 
					 alphas = c(0.0, 0.5, 1.0), 
					 etas = c(0.1, 0.3, 0.5), 
					 lambdas = c(0.0, 0.5, 1.0), 
					 gammas = c(0, 5, 10),
					 max_depths = c(6),
					 nrounds = 100,
					 nthread = 2,
					 early_stopping_rounds = 10,
					 nfold = 5){
	#' This function performs parameters' tunning of XGB
	
	# Data Frame
	xgb_performance <- data.frame(eta=numeric(0), alpha=numeric(0), lambda=numeric(0), 
								  gamma=numeric(0), auc=numeric(0), max_depth=numeric(0), 
								  iteration=numeric(0))
	
	for(max_depth in max_depths){
		for(eta in etas){
			for(alpha in alphas){
				for(lambda in lambdas){
					for(gamma in gammas){
						model_cv <- xgb.cv(params = list(objective = "binary:logistic",
														 eval_metric = "auc",
														 early_stopping_rounds = early_stopping_rounds,
														 alpha = alpha,
														 lambda = lambda,
														 eta = eta,
														 gamma = gamma,
														 max_depth = max_depth,
														 nthread = nthread),
										   nrounds = nrounds,
										   metrics = list("auc"),
										   data = data,
										   nfold = nfold,
										   verbose = 0)
						auc <- max(model_cv$evaluation_log$test_auc_mean)
						iter <- which(model_cv$evaluation_log$test_auc_mean == auc)
						xgb_performance[nrow(xgb_performance)+1,] <- c(eta, alpha, lambda, gamma, auc, max_depth, iter)
						print(xgb_performance[nrow(xgb_performance),])
					}
				}
			}
		}
	}
	xgb_performance <- xgb_performance %>%
		arrange(desc(auc))
	return(xgb_performance)
}












