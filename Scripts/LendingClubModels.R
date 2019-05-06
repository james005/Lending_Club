#' TO DO
#' 1. Tree for subsate of data with grades and without them
#' 2. Logistic regression
#' 3. Logistic regression with elastic net or Lasso
#' 4. Random Forest
#' 5. SVM
#' 6. Ensemble model of RF, SVM and best logistic regression

# ###################################################################
# Libraries and constants
# ###################################################################
library(caret)
library(dplyr)
library(tidyr)
library(ggplot2)
library(tidyr)
library(pROC)
library(glmnet)
library(biglm)
library(random)
library(randomForest)
library(kernlab)
library(xgboost)
library(Matrix)

WORKING_DIR <- "/home/rstudio/"

setwd(WORKING_DIR)
options(stringsAsFactors = F)

source(paste0(getwd(), "/Lending_Club/Scripts/LendingClubFunctions.R"))
tuning <- FALSE
# ###################################################################
# Loading data
# ###################################################################
load("Data/Lending_Club/train_data.RData")
load("Data/Lending_Club/test_data.RData")

# ###################################################################
# Models
# ###################################################################

# ###################################################################
# Logistic regression
# ###################################################################
# Downsampling
set.seed(100)
samp <- downSample(train_data[-which(colnames(train_data) == "default")],
				   as.factor(train_data[["default"]]),
				   yname = "default") %>%
	mutate(default = (default %>% as.integer()) - 1) %>%
	.[base::sample(nrow(.)),] # Shuffle DF so that it is suitable for bigglm
	

# Formula
col_names <- names(train_data)
v_formula <- as.formula(paste("default ~", paste(paste0("`", col_names[!col_names == "default"], "`"), collapse = " + ")))

# Model
model_logit <- bigglm(v_formula, data = samp, family = binomial(link = 'logit'),
			   chunksize = 10^5, maxit = 20)
model_logit %>% summary()

# Predictions
pred_logit <- predict(model_logit, test_data, type = "response", family = binomial(link = 'logit'))
pred_logit <- pred_logit %>% as.numeric()
save(pred_logit, file = "Predictions/Pred_Logit.RData")

# Distribution of prediction
ggplot(data = data.frame(pred = pred_logit))+
	geom_density(aes(x = pred, y = ..density..))+
	theme_bw()

# ROC curve
rocCurve_logit <- pROC::roc(response = test_data$default,
					 predictor = pred_logit)
auc_curve_logit <- pROC::auc(rocCurve_logit)
plot(rocCurve_logit, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Logistic Regression)")

# Prediction measures 
pred_status <- ifelse(pred_logit > 0.5, 1, 0)
conf_mat <- caret::confusionMatrix(as.factor(pred_status), as.factor(test_data$default),
								   positive = "1")

# Data frame that collects models' performance
metrics_table <- data.frame(Model = character(0),
						AUC = numeric(0),
						Accuracy = numeric(0),
						Precision = numeric(0),
						Recall = numeric(0),
						Specificity = numeric(0),
						F1 = numeric(0),
						Kappa = numeric(0),
						stringsAsFactors = FALSE
)

metrics_table[1,] <- c("Logistic regression",
				   round(auc_curve_logit, 3),
				   as.numeric(round(conf_mat$overall["Accuracy"], 3)),
				   as.numeric(round(conf_mat$byClass["Precision"], 3)),
				   as.numeric(round(conf_mat$byClass["Recall"], 3)),
				   as.numeric(round(conf_mat$byClass["Specificity"], 3)),
				   as.numeric(round(conf_mat$byClass["F1"], 3)),
				   as.numeric(round(conf_mat$overall["Kappa"], 3))
)

# ###################################################################
# Logistic regression - Elastic Net
# ###################################################################
# Downsampling and parameters' tuning
set.seed(100)
samp <- downSample(train_data[-which(colnames(train_data) == "default")],
				  as.factor(train_data[["default"]]), 
				  yname = "default") %>%
	mutate(default = ifelse(default == 0, "not_default", "default") %>% as.factor())
# Subsample for tunning
tuning_index <- createDataPartition(samp$default, p = 0.1, list=FALSE, times=1)

ctrl <- trainControl(method = "cv",
					 summaryFunction = twoClassSummary,
					 classProbs = TRUE,
					 number = 3
)

if(tuning){
	glmnGrid <- expand.grid(.alpha = seq(0, 1, length = 11), .lambda = c(0.001, 0.005, 0.01))
	
	glmnTuned <- train(samp[tuning_index, -which(colnames(samp) == "default")],
					  y = as.factor(samp[tuning_index, "default"]),
					  method = "glmnet",
					  tuneGrid = glmnGrid,
					  metric = "ROC",
					  trControl = ctrl)
	
	png("Lending_Club/Plots/glmnTuned.png", width = 800, height = 500)
	plot(glmnTuned)
	dev.off()
	
	glmnTuned
}

# Run Model 
model_net <- glmnet(y = samp[["default"]],
					x = as.matrix(samp[, -which(colnames(samp) == "default")]),
					family = "binomial",
					alpha = 0.7, 
					lambda = 0.001)
model_net$beta 

# Select variables to drop according to Elastic Net 
betas <- model_net$beta 
variables_to_drop <- betas@Dimnames[[1]]
betas <- betas %>% as.vector()
variables_to_drop <- variables_to_drop[which(betas == 0)]

# Prediction
pred_net <- predict(model_net, 
				newx = as.matrix(test_data[, -which(colnames(test_data) == "default")]),
				type = "response")
pred_net <- pred_net %>% as.numeric()
pred_net <- 1 - pred_net 

# Distribution of prediction
ggplot(data = data.frame(pred = pred_net))+
	geom_density(aes(x = pred, y = ..density..))+
	theme_bw()

# ROC curve
rocCurve_net <- pROC::roc(response = test_data$default,
							predictor = pred_net)
auc_curve_net <- pROC::auc(rocCurve_net)
plot(rocCurve_net, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Elastic Net)")

# Prediction measures
pred_status <- ifelse(pred_net > 0.5, 1, 0)
conf_mat <- caret::confusionMatrix(as.factor(pred_status), as.factor(test_data$default),
								   positive = "1")

# Data frame that collects models' performance
metrics_table[2,] <- c("Elastic Net",
				   round(auc_curve_net, 3),
				   as.numeric(round(conf_mat$overall["Accuracy"], 3)),
				   as.numeric(round(conf_mat$byClass["Precision"], 3)),
				   as.numeric(round(conf_mat$byClass["Recall"], 3)),
				   as.numeric(round(conf_mat$byClass["Specificity"], 3)),
				   as.numeric(round(conf_mat$byClass["F1"], 3)),
				   as.numeric(round(conf_mat$overall["Kappa"], 3))
)

# ###################################################################
# Random Forest
# ###################################################################
# Downsampling
set.seed(100)
samp <- downSample(train_data[-which(colnames(train_data) == "default")],
				   as.factor(train_data[["default"]]), 
				   yname = "default") %>%
	mutate(default = ifelse(default == 0, "not_default", "default") %>% as.factor())
train_index <- createDataPartition(samp$default, p=0.1, list=FALSE, times=1)
tuning_index <- createDataPartition(samp$default, p=0.05, list=FALSE, times=1)

# Tuning
if(tuning){
	rfGrid <- expand.grid(.mtry = c(2, 4, 8, 16, 24, 32, 40))
	
	rfTuned <- train(
		samp[tuning_index, -which(colnames(samp) == "default")],
		y = samp[tuning_index, "default"],
		method = "rf",
		tuneGrid = rfGrid,
		metric = "ROC",
		trControl = ctrl,
		preProcess = NULL,
		ntree = 100
	)
	
	png("Lending_Club/Plots/rfTuned.png", width = 800, height = 500)
	plot(rfTuned)
	dev.off()
	
	rfTuned
}

# Build model with ranger package
set.seed(1001)
model_rf <- ranger::ranger(default ~., data = samp, mtry = 32, num.trees = 500, probability = T)

# Prediction
pred_rf <- predict(model_rf, test_data, type = "response")
pred_rf <- pred_rf$predictions[,1]

# Distribution of prediction
ggplot(data = data.frame(pred = pred_rf))+
	geom_density(aes(x = pred, y = ..density..))+
	theme_bw()

# ROC curve
rocCurve_rf <- pROC::roc(response = test_data$default,
							predictor = pred_rf)
auc_curve_rf <- pROC::auc(rocCurve_rf)
plot(rocCurve_rf, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Random Forest)")

# Prediction measures 
pred_status <- ifelse(pred_rf > 0.5, 1, 0)
conf_mat <- caret::confusionMatrix(as.factor(pred_status), as.factor(test_data$default),
								   positive = "1")

# Data frame that collects models' performance
metrics_table[3,] <- c("Random Forest",
					   round(auc_curve_rf, 3),
					   as.numeric(round(conf_mat$overall["Accuracy"], 3)),
					   as.numeric(round(conf_mat$byClass["Precision"], 3)),
					   as.numeric(round(conf_mat$byClass["Recall"], 3)),
					   as.numeric(round(conf_mat$byClass["Specificity"], 3)),
					   as.numeric(round(conf_mat$byClass["F1"], 3)),
					   as.numeric(round(conf_mat$overall["Kappa"], 3))
)

# ###################################################################
# XGBoost
# ###################################################################
# Parameters tunning and subsampling
set.seed(100)
samp <- downSample(train_data[-which(colnames(train_data) == "default")],
				   as.factor(train_data[["default"]]),
				   yname = "default") %>%
	mutate(default = (default %>% as.integer()) - 1)
	
train_index_tuning <- createDataPartition(samp$default, p = 0.1, list=FALSE, times=1)

# Sparse Matrix - tunning
samp_matrix <- as.matrix(samp[train_index_tuning, ][-which(colnames(samp[train_index_tuning, ]) == "default")])
X_spm <- as(samp_matrix, "dgCMatrix")
xgb_matrix_tun <- xgb.DMatrix(label = samp[train_index_tuning, ][["default"]], data = X_spm)

# Sparse Matrix - training
samp_matrix <- as.matrix(samp[-train_index_tuning, ][-which(colnames(samp[-train_index_tuning, ]) == "default")])
X_spm <- as(samp_matrix, "dgCMatrix")
xgb_matrix_train <- xgb.DMatrix(label = samp[-train_index_tuning, ][["default"]], data = X_spm)
rm(samp_matrix, X_spm)
gc(reset = T)

if(tuning){
	# Parametrs tunning
	set.seed(1001)
	xgb_performance <- tune_xgb(xgb_matrix_tun, 
								alphas = c(0.0, 0.5, 1.0), 
								etas = c(0.1), 
								lambdas = c(0.0, 0.5, 1.0), 
								gammas = c(0, 5, 10),
								max_depths = c(6),
								nrounds = 100,
								nthread = 4,
								early_stopping_rounds = 10,
								nfold = 5)
	
	xgb_performance
	
	set.seed(1001)
	xgb_performance_2 <- tune_xgb(xgb_matrix_tun, 
								 alphas = c(1.0), 
								 etas = c(0.08, 0.1, 0.12), 
								 lambdas = c(0.0, 0.3, 0.5), 
								 gammas = c(10, 15),
								 max_depths = c(6),
								 nrounds = 100,
								 nthread = 4,
								 early_stopping_rounds = 10,
								 nfold = 5)
	
	xgb_performance_2
	
	set.seed(1001)
	xgb_performance_3 <- tune_xgb(xgb_matrix_tun, 
								  alphas = c(1.0), 
								  etas = c(0.08, 0.1, 0.12), 
								  lambdas = c(0.0), 
								  gammas = c(10),
								  max_depths = c(5, 6, 7),
								  nrounds = 100,
								  nthread = 4,
								  early_stopping_rounds = 10,
								  nfold = 5)
	
	xgb_performance_3
	
	xgb_performance <- xgb_performance %>%
		union_all(xgb_performance_2) %>%
		union_all(xgb_performance_3) %>%
		distinct(alpha, eta, lambda, gamma, max_depth, .keep_all = T) %>%
		arrange(desc(auc))
	save(xgb_performance, file = "Lending_Club/Results/xgb_tunning.RData")
}

# XGBoost
set.seed(1001)
test_watchlist <- list(test = xgb_matrix_tun)
model_xgb <- xgb.train(data = xgb_matrix_train, 
					   objective = "binary:logistic",
					   nrounds = 350,
					   watchlist = test_watchlist,
					   eval_metric = "auc",
					   early_stopping_rounds = 10,
					   alpha = 1.0,
					   lambda = 0,
					   eta = 0.08,
					   gamma = 10,
					   max_depth = 7,
					   nthread = 4)

save(model_xgb, file = "Lending_Club/Results/Model_XGB.RData")

# Sparse Matrix - test
test_matrix <- as.matrix(test_data[-which(colnames(test_data) == "default")]) 
test_matrix <- test_matrix[, colnames(train_data[-which(colnames(train_data) == "default")])] # Test matrix has to have the same order as train matrix
X_spm <- as(test_matrix, "dgCMatrix")
xgb_matrix_test <- xgb.DMatrix(label = test_data[["default"]], data = X_spm)
rm(test_matrix, X_spm)
gc(reset = T)

# Prediction
pred_xgb <- predict(model_xgb, newdata = xgb_matrix_test)
save(pred_xgb, file = "Predictions/Pred_XGB.RData")

# Distribution of prediction
ggplot(data = data.frame(pred = pred_xgb))+
	geom_density(aes(x = pred, y = ..density..))+
	theme_bw()

# ROC curve
rocCurve_xgb <- pROC::roc(response = test_data$default,
						  predictor = pred_xgb)
auc_curve_xgb <- pROC::auc(rocCurve_xgb)
plot(rocCurve_xgb, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (XGBoost)")

# Prediction measures 
pred_status <- ifelse(pred_xgb > 0.5, 1, 0)
conf_mat <- caret::confusionMatrix(as.factor(pred_status), as.factor(test_data$default),
								   positive = "1")

# Data frame that collects models' performance
metrics_table[4,] <- c("XGBoost",
					   round(auc_curve_xgb, 3),
					   as.numeric(round(conf_mat$overall["Accuracy"], 3)),
					   as.numeric(round(conf_mat$byClass["Precision"], 3)),
					   as.numeric(round(conf_mat$byClass["Recall"], 3)),
					   as.numeric(round(conf_mat$byClass["Specificity"], 3)),
					   as.numeric(round(conf_mat$byClass["F1"], 3)),
					   as.numeric(round(conf_mat$overall["Kappa"], 3))
)

# ###################################################################
# Ensemble
# ###################################################################
# Prediction
head(data.frame(pred_xgb = pred_xgb, pred_rf = pred_rf, pred_logit = pred_logit), n = 100)
pred_ensemble <- (pred_xgb + pred_logit + pred_rf) / 3

# ROC curve
rocCurve_ensemble <- pROC::roc(response = test_data$default,
						  predictor = pred_ensemble)
auc_curve_ensemble <- pROC::auc(rocCurve_ensemble)
plot(rocCurve_ensemble, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (ensemble)")

# Prediction measures 
pred_status <- ifelse(pred_ensemble > 0.5, 1, 0)
conf_mat <- caret::confusionMatrix(as.factor(pred_status), as.factor(test_data$default),
								   positive = "1")

# Data frame that collects models' performance
metrics_table[5,] <- c("Ensemble",
					   round(auc_curve_ensemble, 3),
					   as.numeric(round(conf_mat$overall["Accuracy"], 3)),
					   as.numeric(round(conf_mat$byClass["Precision"], 3)),
					   as.numeric(round(conf_mat$byClass["Recall"], 3)),
					   as.numeric(round(conf_mat$byClass["Specificity"], 3)),
					   as.numeric(round(conf_mat$byClass["F1"], 3)),
					   as.numeric(round(conf_mat$overall["Kappa"], 3))
)

# Save data
save(metrics_table, file = "Lending_Club/Results/Metrics.RData")
save(pred_logit, pred_net, pred_rf, pred_xgb, file = "Predictions/Pred_all.RData")

# ###################################################################
# Save AUC plots
# ###################################################################
# 4 plots on same pane
png("Lending_Club/Plots/AUC_plots.png", width = 800, height = 800)
par(mfrow = c(2, 2))
plot(rocCurve_logit, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Logistic Regression)")
plot(rocCurve_net, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Elastic Net)")
plot(rocCurve_rf, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Random Forest)")
plot(rocCurve_xgb, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (XGBoost)")
dev.off()

# AUC of ensemble
png("Lending_Club/Plots/AUC_ensemble.png", width = 500, height = 500)
par(mfrow = c(1, 1))
plot(rocCurve_ensemble, legacy.axes = TRUE, print.auc = TRUE,
	 col="red", main="ROC (Ensemble)")
dev.off()



