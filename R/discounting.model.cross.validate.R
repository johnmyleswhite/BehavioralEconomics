discounting.model.hierarchical.cross.validate <- function()
{
  discounting.model.hierarchical.cross.validate <- function(choices,
                                                            model = 'exponential',
                                                            iterations = 1,
                                                            training.proportion = 2 / 3,
                                                            control = list(n.adapt = 200,
                                                                           n.iter = 100,
                                                                           raw.samples = FALSE,
                                                                           median = FALSE,
                                                                           mode = FALSE,
                                                                           hierarchical = TRUE))
  {
    n <- nrow(choices)
    training.size <- round(training.proportion * n)
  
    performance <- data.frame()
  
    for (i in 1:iterations)
    {
      training.indices <- sample(1:n, training.size)
    
      training.choices <- choices[training.indices, ]
      test.choices <- choices[-training.indices, ]
    
      parameters <- discounting.model.fit(training.choices,
                                          model = model,
                                          method = 'bayes',
                                          control = control)
    
      ll <- discounting.model.hierarchical.log.likelihood(test.choices, model, parameters)
    
      predicted.choices <- discounting.model.hierarchical.predict(test.choices, model, parameters)
    
      rmse <- sqrt(mean((predicted.choices$C - test.choices$C) ^ 2))
    
      performance <- rbind(performance,
                           data.frame(Model = model,
                                      Iteration = i,
                                      LogLikelihood = ll,
                                      RMSE = rmse))
    }
  
    return(performance)
  }
}
