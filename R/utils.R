## NNet object

HyperparametersBase <- R6::R6Class("HyperparametersBase",
                                   public = list(

                                       tuning = NULL,
                                       hyperparams_constant = NULL,
                                       hyperparams_ranges = NULL,

                                       initialize = function(hyperparams = NULL){

                                           self$tuning = FALSE
                                           self$hyperparams_constant = NULL

                                           self$check_hyperparams(hyperparams)

                                           hyperparameters <- self$set_hyperparams(hyperparams)

                                           # List values are converted to dials::value_set, else a single value

                                           # Change hyperparams to dials::parameters

                                           hyperparams_ranges <- base::Filter(function(x) inherits(x, "param"), hyperparameters)

                                           hyperparams_constant <- base::Filter(function(x) !inherits(x, "param"), hyperparameters)

                                           self$hyperparams_ranges <- hyperparams_ranges

                                           self$hyperparams_constant = hyperparams_constant

                                           if (length(hyperparams_ranges) > 0){self$tuning <- TRUE}

                                       },


                                       set_hyperparams = function(hyperparams){
                                           stop("Must be implemented in the subclass")
                                       },

                                       default_hyperparams = function() {
                                           stop("Must be implemented in the subclass")
                                       },

                                       check_hyperparams = function(hyperparams){
                                           stop("Must be implemented in the subclass")
                                       }

                                   )
)

HyperparamsNN_nnet <- R6::R6Class("Neural Network Hyperparameters",
                                  inherit = HyperparametersBase,
                                  public = list(

                                      hidden_units_tune = TRUE,
                                      penalty_tune = TRUE,
                                      epochs_tune = FALSE,

                                      default_hyperparams = function() {
                                          list(penalty = dials::penalty(range = c(-3, -1.5)),
                                               hidden_units = dials::hidden_units(range = c(5, 20)),
                                               epochs = 100)

                                      },

                                      check_hyperparams = function(hyperparams){

                                          valid_hparams <- c("penalty", "hidden_units", "epochs")

                                          if (!is.null(hyperparams)){

                                              if (all(names(hyperparams) %in% valid_hparams)){



                                              }

                                              else {

                                                  stop(paste0("Incorrect hyperparameter list. Valid hyperparameters are:",

                                                              paste(valid_hparams, collapse = ",")))

                                              }

                                          }


                                      },

                                      set_hyperparams = function(hyperparams = NULL) {

                                          default_hyperparameters <- self$default_hyperparams()

                                          # Actualizar solo los valores proporcionados

                                          if (!is.null(hyperparams)) {

                                              if ("penalty" %in% names(hyperparams)) {

                                                  if (length(hyperparams$penalty) > 1){

                                                      default_hyperparameters$penalty <- dials::penalty(range = hyperparams$penalty)

                                                  } else if (!is.null(hyperparams$penalty)){

                                                      default_hyperparameters$penalty <- hyperparams$penalty

                                                      self$penalty_tune = F

                                                  }

                                              }

                                              if ("hidden_units" %in% names(hyperparams)) {

                                                  if (length(hyperparams$hidden_units) > 1){

                                                      default_hyperparameters$hidden_units <- dials::hidden_units(range = hyperparams$hidden_units)

                                                  } else if (!is.null(hyperparams$hidden_units)){

                                                      default_hyperparameters$hidden_units <- hyperparams$hidden_units

                                                      self$hidden_units_tune = F
                                                  }

                                              }

                                              ### if epochs

                                          }
                                          return(default_hyperparameters)
                                      }
                                  )
)
