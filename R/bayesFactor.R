library(BayesFactor)
library(BayesVarSel)
library(ggm)

bayes_factor <- function(data, response, no_prior_information = FALSE,
                         desired_sparsity = NULL, desired_prior_effect = NULL,
                         covariate_probabilities = NULL,
                         predict = FALSE, range_false_predicts = 10,
                         standardize = FALSE,
                         train = FALSE, train_percent=NULL){
  ### CHECKS ###
  if (nrow(data) != length(response)){
    print("Error! Please ensure that the number of elements in the response vector is the same
           as the number of rows in the data data.frame.")
    break
  }

  # Question for Emma: Will the return end the function or do I need a break above?

  if (standardize == TRUE){
    for (predictor in 1:ncol(data)){
      data[, predictor] <- (data[, predictor] - mean(data[, predictor]))/sd(data[, predictor])
    }
  }

  if (train == TRUE) {
    if (is.null(train_percent) == TRUE){
      print("Please input a percent to be set aside for training")
      break
    }
    train_data_length <- round(train_percent*nrow(data), digits = 0)
    train_data_ind <- sample(1:nrow(data), size = train_data_length, replace = FALSE)
    train <- data[train_data_ind]
    test <- data[-train_data_ind]
    data <- train
    y <- y[train_data_ind]
    test_y <- y[-train_data_ind]
  }

  ### First layer of Decision Tree (BIC Bayes Factor)
  if (no_prior_information == TRUE){
    return(BIC_function(data, response))
    break
  }

  ### Second Layer of Decision Tree ()
  if (is.null(covariate_probabilities) == FALSE){
    #for (prior in c("Robust", "gZellner", "Liangetal", "ZellnerSiow", "FLS")){
    # Small p
    if (ncol(data) <= 25){
      # paste0("bf_obj", prior)
      a <- Bvs(formula = response ~ .,
               data=data.frame(data,response),
               prior.betas = "Robust",
               prior.models = "User",
               priorprobs = covariate_probabilities)
      return(a)
      coef <- BMAcoeff(a)
      for (col in colnames(data)){
        return(histBMA(coef, covariate = col))
      }
      if (train == TRUE){
        return(predict.Bvs(a), test)
      }
      # }
      # Large p
      else{
        paste0("bf_obj", prior) <- GibbsBvs(formula = response ~ .,
                                            data=data.frame(data,response),
                                            prior.betas = prior,
                                            prior.models = "User",
                                            priorprobs = covariate_probabilities)
        return(final_output(paste0("bf_obj", prior)), predict)
      }
    }
  }

  ### Third Layer of Decision Tree (BayesFactor)
  if ((desired_sparsity <= 0.4 && desired_prior_effect > 0.5) ||
      (desired_sparsity <= 0.4 && is.null(desired_prior_effect) == TRUE) ||
      (is.null(desired_sparsity) == TRUE && desired_prior_effect > 0.5)){
    return(regressionBF(formula = response~.,
                        data= data.frame(data, response),
                        rscaleCont = "medium",
                        whichModels = "all"
    ))
  }
  if ((desired_sparsity >= 0.6 && desired_prior_effect < 0.5) ||
      (desired_sparsity >= 0.6 && is.null(desired_prior_effect) == TRUE) ||
      (is.null(desired_sparsity) == TRUE && desired_prior_effect < 0.5)){
    return(regressionBF(formula = response~.,
                        data= data.frame(data, response),
                        rscaleCont = "ultrawide",
                        whichModels = "all"
    ))
  }
  else {
    return(regressionBF(formula = response~.,
                        data= data.frame(data, response),
                        rscaleCont = "wide",
                        whichModels = "all"
    ))
  }
}

BIC_function <- function(data, response){
  BIC_BFs <- c()
  for (elem in powerset(colnames(data))){
    full_lm = lm(y~., data.frame(response, data[,elem]))
    null_lm = lm(response ~ 0, data.frame(response,data))
    BF_BIC = exp(-0.5*(BIC(null_lm)-BIC(full_lm))) # convert BIC to Bayes Factor
    BIC_BFs <- c(BIC_BFs, c(elem, BF_BIC))
  }
  print("Please be careful when using these results because this is a data based prior, so there can be issues associated with double-dipping because both the prior and likelihood are from the same sample. See object documentation for alternatives such as hierarchial modeling.")
  return(c(BIC_BFs, "Lowest Bayes Factor Corresponding to Most Significant Model:", min(BIC_BFs), BIC_BFs[which(BIC_BFs == min(BIC_BFs))-1]))
}
