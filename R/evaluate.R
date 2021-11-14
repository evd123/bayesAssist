evaluate <- function(p, prior_knowledge, response, goal) {
  if (p < 80) {
    # Bayes Factor
    print("Because the number of predictors is relatively low, we recommend using Bayes Factor. Run bayes_factor")
  } else {
    if (prior_knowledge) {
      # Spike and Slab
      print ("Because you have prior knowledge on the data (?), we recommend using Spike and Slab. Run spike_and_slab")
    } else {
      #
      if (response == "indicator") {
        print ("Because you want an indicator response, we recommend using Spike and Slab. Run spike_and_slab")
      } else if (response == "continuous") {
        if (goal == "exploratory") {
          print ("Because your goal is data exploration, we recommend using Spike and Slab. Run spike_and_slab")
        } else if (goal == "prediction") {
          print ("Because your goal is prediction, we recommend using Continuous Shrinkage. Run continuous_shrinkage")
        }
      }
    }
  }
}
