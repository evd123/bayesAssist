require(BoomSpikeSlab)

spikeAndSlab <- function(design_matrix = NULL, # predictors and design_matrix are the same thing just in different formats
                         predictors = NULL,
                         xdim = dim(design_matrix),
                         response = NULL,
                         expected.r2 = .5,
                         prior.df = .01,
                         expected_percent_nonzero = 1,
                         zellner_g = FALSE,
                         p = 100,
                         prior.information.weight = .01,
                         diagonal.shrinkage = .5,
                         optional.coefficient.estimate = NULL,
                         max.flips = -1,
                         prior.inclusion.probabilities = NULL,
                         mean.y = mean(response, na.rm = TRUE),
                         sdy = sd(as.numeric(response), na.rm = TRUE),
                         sigma.upper.limit = Inf) {
  if (!is.null(predictors) && is.null(design_matrix) && !zellner_g && !is.null(xdim)) { # don't need to update xdim at this step b/c we will use else case (line 63)
    design_matrix <- model.matrix(predictors)
  }

  if (zellner_g || (is.null(predictors) && is.null(design_matrix))) {
    if (is.null(predictors) && is.null(design_matrix)) { # no information on predictors, must use xdim
      if (is.na(xdim)) {
        print("Please enter the expected dimensions of the predictor matrix or the values of the predictors")
        return()
      }

      # calculate the prior
      prior <- ConditionalZellnerPrior(xdim,
                                       optional.coefficient.estimate = optional.coefficient.estimate,
                                       expected.model.size = expected_percent_nonzero*p,
                                       prior.information.weight = prior.information.weight,
                                       diagonal.shrinkage = diagonal.shrinkage,
                                       max.flips = max.flips,
                                       prior.inclusion.probabilities = prior.inclusion.probabilities) # use if predictor matrix is unknown
      prior
      print("A model cannot be fit without a dataset")
      return(prior)
    } else {
      # calculate the prior
      prior <- SpikeSlabPrior(x = design_matrix,
                              y = response,
                              expected.r2 = expected.r2,
                              prior.df = prior.df,
                              expected.model.size = expected_percent_nonzero*p,
                              prior.information.weight = prior.information.weight,
                              diagonal.shrinkage = 0,
                              optional.coefficient.estimate = optional.coefficient.estimate,
                              max.flips = max.flips,
                              mean.y = mean.y,
                              sdy = sdy,
                              prior.inclusion.probabilities = prior.inclusion.probabilities,
                              sigma.upper.limit = sigma.upper.limit)
      prior
      # fit the model
      fit <- lm.spike(response~design_matrix-1, niter = 1000, prior = prior)
      fit
      return(list(prior, fit))
    }

  } else {
    # calculate the prior
    prior <- SpikeSlabPrior(x = design_matrix,
                            y = response,
                            expected.r2 = expected.r2,
                            prior.df = prior.df,
                            expected.model.size = expected_percent_nonzero*p,
                            prior.information.weight = prior.information.weight,
                            diagonal.shrinkage = diagonal.shrinkage,
                            optional.coefficient.estimate = optional.coefficient.estimate,
                            max.flips = max.flips,
                            mean.y = mean.y,
                            sdy = sdy,
                            prior.inclusion.probabilities = prior.inclusion.probabilities,
                            sigma.upper.limit = sigma.upper.limit)
    prior
    # fit the model
    fit <- lm.spike(response~design_matrix-1, niter = 1000, prior = prior)
    fit
    return(list(prior, fit))
  }
}
