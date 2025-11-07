
# This file is a generated template, your changes will not be overwritten

BinaryClassificationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "BinaryClassificationClass",
    inherit = BinaryClassificationBase,

    private = list(

        ## Member Variables?

        .dat = NA,
        .form = NA,
        .sobol_jansen = NA,
        .feature_names = NA,
        .class_names = NA,

        ## Init

        .init = function(){

            private$.dat <- NULL
            private$.form <- NULL
            private$.sobol_jansen <- FALSE
            private$.feature_names <- NULL

            #private$.checkArguments()

            if (!is.null(self$options$dep)){

                all_levels <- levels(as.factor(self$data[[self$options$dep]]))

                if (length(all_levels) > 2){

                    stop("Dependant Variable has more than 2 classes.
                         Please use 'Multiclass Classification'")

                }

                levels <- c(setdiff(all_levels, as.character(self$options$dep_level))
                            , as.character(self$options$dep_level))

                private$.class_names <- levels

            }

            private$.initSummaryTable()
            private$.initResultsPlots()
            private$.initSensitivityPlots()
            private$.initSensitivityTables()

        },

        .initSummaryTable = function(){

            metrics_set <- private$.getSummaryMetrics(self$options)

            table_summary <- self$results$summary

            for (metric in metrics_set){

                table_summary$addColumn(name = metric)

            }
        },

        .initResultsPlots = function(){

            if (self$options$plot_roc_curve){

                key = 'ROC Curve'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_pr_curve){

                key = 'Precision-Recall Curve'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_gain_curve){

                key = 'Gain Curve'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_lift_curve){

                key = 'Lift Curve'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_dist_by_class){

                key = 'Output Distribution By Class'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_reliability_plot){

                key = 'Reliability Plot'

                self$results$plots_results$addItem(key = key)

            }

            if (self$options$plot_confusion_matrix){

                key = 'Confusion Matrix'

                self$results$plots_results$addItem(key = key)

            }

        },

        .initSensitivityPlots = function(){

            if (self$options$pfi == TRUE){

                key = 'Permutation Feature Importance'

                self$results$plots_sensitivity$addItem(key = key)

            }

            if (self$options$shap_mean == TRUE){

                key = 'Mean Absolute SHAP'

                self$results$plots_sensitivity$addItem(key = key)

            }

            if (self$options$shap_dir){

                key = "Directional SHAP"

                self$results$plots_sensitivity$addItem(key = key)

            }

            if (self$options$shap_box){

                key = "SHAP Boxplot"

                self$results$plots_sensitivity$addItem(key = key)


            }

            if (self$options$shap_swarm){

                key = 'SHAP Swarm Plot'

                self$results$plots_sensitivity$addItem(key = key)
            }


            if (self$options$olden == TRUE){

                key = "Olden Importance"

                self$results$plots_sensitivity$addItem(key = key)

            }

            if (self$options$sobol == TRUE){

                key = "Sobol Importance"

                self$results$plots_sensitivity$addItem(key = key)

            }

        },

        .initSensitivityTables = function(){

            if (self$options$pfi){

                key = "Permutation Feature Importance"

                self$results$table_sensitivity$addItem(key = key)

            }

            if (any(c(self$options$shap_mean, self$options$shap_dir,
                      self$options$shap_box, self$options$shap_swarm))){

                key = 'SHAP Summary Importance'

                self$results$table_sensitivity$addItem(key = key)

            }

            if (self$options$olden){

                key = "Olden Importance"

                self$results$table_sensitivity$addItem(key = key)

            }

            if (self$options$sobol){

                key = "Sobol-Jansen Sensitivity Importance"

                self$results$table_sensitivity$addItem(key = key)

            }

        },

        .getSummaryMetrics = function(options, add_dataset = FALSE){

            if (add_dataset){

                metrics_set <- c("Dataset")

            } else {

                metrics_set = c()

            }

            if (options$accuracy){metrics_set = c(metrics_set, "Accuracy")}

            if (options$bal_accuracy){metrics_set = c(metrics_set, "Balanced_Accuracy")}

            if (options$precision){metrics_set = c(metrics_set, "Precision")}

            if (options$recall){metrics_set = c(metrics_set, "Recall")}

            if (options$specificity){metrics_set = c(metrics_set, "Specificity")}

            if (options$sensitivity){metrics_set = c(metrics_set, "Sensitivity")}

            if (options$kap){ metrics_set = c(metrics_set, "Kappa")}

            if (options$f_meas){metrics_set = c(metrics_set, "F1_score")}

            if (options$mcc){metrics_set = c(metrics_set, "MCC")}

            if (options$j_index){metrics_set = c(metrics_set, "J_index")}

            if (options$detection_prevalence){metrics_set = c(metrics_set, "Detection_Prevalence")}

            if (options$auc_roc){metrics_set = c(metrics_set, "AUC_ROC")}

            if (options$pr_auc){metrics_set = c(metrics_set, "AUC_PR")}

            if (options$gain_capture){metrics_set = c(metrics_set, "Gain_Capture")}

            if (options$brier_score){metrics_set = c(metrics_set, "Brier_Score")}

            return(metrics_set)

        },

        .setInfo = function(){

            if (self$options$mode == "neural_network"){

                model_name = "Neural Network"

            }

            else if (self$options$mode == "random_forest"){

                model_name = "Random Forest"

            }

            else if (self$options$mode == "xgboost"){

                model_name = "XGBoost"
            }

            else {

                if (self$options$kernels == "linear"){

                    model_name = "Support Vector Machine (Linear Kernel)"

                } else if (self$options$kernels == "rbf"){

                    model_name = "Support Vector Machine (RBF Kernel)"

                } else{

                    model_name = "Support Vector Machine (Polynomial Kernel)"

                }

            }

            positive_class <- private$.class_names[2]

            if (!is.null(self$options$dep)){

                dep_col <- self$options$dep

                imbalance_text <- self$data |>
                    dplyr::count(.data[[dep_col]], name = "n") |>
                    dplyr::rename(class = 1) |>
                    dplyr::mutate(pct = n / sum(n) * 100) |>
                    glue::glue_data("- {class} : {n} ({round(pct, 1)}%)") |>
                    glue::glue_collapse(sep = "\n") |>
                    (\(x) glue::glue("Class Imbalance:\n\n{x}"))()
            }

            else {imbalance_text <- ""}


            content = glue::glue("

  Model: {model_name}

  Random Seed: {self$options$seed}

  Positive class: {self$options$dep_level}

  {imbalance_text}

                    ")

            self$results$text$setContent(content)

        },

        ## Run

        .run = function(){

            private$.setInfo()

            set.seed(self$options$seed)

            if (!self$options$disable) {
                print("Análisis desactivado por el usuario (‘Don’t run’ marcado).")
                return()
            }

            private$.finalData()

            if (private$.compare_hash()){

                model_path <- file.path(tempdir(), "analysis_object.rds")

                analysis_object <- readRDS(model_path)

                self$results$.setModel(analysis_object)

            }

            else{

                private$.compute()

            }

            private$.feature_names <- private$.getFeatureNames(self$results$model)

            ## populate

             private$.populateSummaryMetrics()

             if (self$results$model$hyperparameters$tuning){

                 private$.populateHyperparametersTable(self$results$model)

                 if (self$options$plot_tuner_results){

                     private$.populateTunerPlots()

                 }


            }

             private$.populateResultsPlots()
             private$.populateSensitivityPlots()
             private$.populateSensitivityTables()
             private$.populateOutputs()

        },


        .compute = function(){

            # Preprocessing

            analysis_object <- MLwrap::preprocessing(private$.dat,
                                                     private$.form,
                                                     num_vars = self$options$covs,
                                                     cat_vars = self$options$factors,
                                                     norm_num_vars = self$options$stdCovs,
                                                     encode_cat_vars = self$options$encCat,
                                                     task = "classification",
                                                     y_levels = private$.class_names
                                                    )

            private$.feature_names <- private$.getFeatureNames(analysis_object)

            # Build Model

            names_and_hyperparameters <- private$.getHyperparameters()

            model_name <- names_and_hyperparameters$model_name

            hyperparameters <- names_and_hyperparameters$hyp_list

            if (model_name == "Neural Network"){

                # A bit of a hack

                analysis_object <- private$.createNN(analysis_object, hyp_list = hyperparameters)

            } else {

                analysis_object <- MLwrap::build_model(analysis_object, model_name,
                                                       hyperparameters = hyperparameters)

            }

            # Fine Tuning

            tuner <- private$.getTuner()

            analysis_object <- MLwrap::fine_tuning(analysis_object, tuner = tuner,
                                                   metrics = self$options$metrics)

            # Sensitivity Analysis

            sensitivity_methods <- private$.getSensitivityMethods(model_name)

            analysis_object <- MLwrap::sensitivity_analysis(analysis_object, methods = sensitivity_methods)

            # Calculate Olden for Neural Networks

            if (model_name == "Neural Network"){

                olden_results <- private$.calculateOlden(analysis_object$final_model)

                sensitivity_results <- analysis_object$sensitivity_analysis

                sensitivity_results$Olden <- olden_results

                analysis_object$modify("sensitivity_analysis", sensitivity_results)

            }

            # Save Analysis Object

            self$results$.setModel(analysis_object)

            hash_list <- c(self$options$seed, self$options$dep, self$options$covs, self$options$factors,
                           self$options$stdCovs, self$options$encCat, self$options$metrics, private$.class_names,
                           model_name, hyperparameters, tuner)

            new_hash <- digest::digest(hash_list, serialize = TRUE, seed = 0)

            # Crear ruta del archivo donde guardar el hash
            hash_file <- file.path(tempdir(), "hash.txt")

            # Guardar el hash en el archivo
            writeLines(new_hash, hash_file)

            model_file <- file.path(tempdir(), "analysis_object.rds")

            saveRDS(analysis_object, model_file)

        },

        .compare_hash = function(){

            hash_file <- file.path(tempdir(), "hash.txt")

            if (!file.exists(hash_file)){return(FALSE)}

            model_name_and_hyp <- private$.getHyperparameters()

            model_name <- model_name_and_hyp$model_name

            hyperparameters <- model_name_and_hyp$hyp_list

            tuner <- private$.getTuner()

            new_hash <- digest::digest(c(self$options$seed, self$options$dep, self$options$covs, self$options$factors,
                                         self$options$stdCovs, self$options$encCat,self$options$metrics, private$.class_names,
                                         model_name, hyperparameters, tuner))

            old_hash <- readLines(hash_file)

            return(old_hash == new_hash)

        },

        .getFeatureNames = function(analysis_object){

            y = all.vars(analysis_object$formula)[1]

            rec =  analysis_object$transformer |>
                recipes::prep(training = analysis_object$train_data)

            bake_test = recipes::bake(rec, new_data = analysis_object$test_data)

            feat_names <- names(bake_test)[which(names(bake_test) != y)]

            return(feat_names)

        },

        .getHyperparameters = function(){

            if (self$options$mode == "neural_network"){

                model_name = "Neural Network" # A bit of a hack


                ######### hidden_units ###########

                if (self$options$hidden_neurons_tune == "fix_neurons"){

                    hidden_units = self$options$fix_n_neurons

                } else {

                    hidden_units = c(self$options$min_n_neurons, self$options$max_n_neurons)

                }

                ######### penalty ###########

                if (self$options$penalty_tune == "fix_penalty"){

                    penalty = self$options$fix_penalty

                } else {

                    penalty = c(self$options$min_penalty, self$options$max_penalty)

                }

                hyp_list = list(
                    hidden_units = hidden_units,
                    penalty = penalty
                )


            } else if (self$options$mode == "random_forest"){

                model_name = "Random Forest"

                ##### mtry ######

                if (self$options$mtry_tune == "fix_mtry"){

                    mtry = self$options$fix_mtry

                } else {

                    mtry = c(self$options$min_mtry, self$options$max_mtry)

                }

                ##### trees ######

                if (self$options$trees_tune == "fix_trees"){

                    trees = self$options$fix_trees

                } else {

                    trees = c(self$options$min_trees, self$options$max_trees)

                }

                ##### min_n ######

                if (self$options$min_n_tune == "fix_min_n"){

                    min_n = self$options$fix_min_n

                } else {

                    min_n = c(self$options$min_min_n, self$options$max_min_n)

                }

                hyp_list = list(
                    trees = trees,
                    mtry = mtry,
                    min_n = min_n
                )


            } else if (self$options$mode == "xgboost"){

                model_name = "XGBOOST"

                ##### mtry ######

                if (self$options$mtryx_tune == "fix_mtryx"){

                    mtry = self$options$fix_mtryx

                } else {

                    mtry = c(self$options$min_mtryx, self$options$max_mtryx)

                }

                ##### trees ######

                if (self$options$treesx_tune == "fix_treesx"){

                    trees = self$options$fix_treesx

                } else {

                    trees = c(self$options$min_treesx, self$options$max_treesx)

                }

                ##### min_n ######

                if (self$options$min_nx_tune == "fix_min_nx"){

                    min_n = self$options$fix_min_nx

                } else {

                    min_n = c(self$options$min_min_nx, self$options$max_min_nx)

                }

                ##### tree_depth #######

                if (self$options$tree_depth_tune == "fix_tree_depth"){

                    tree_depth = self$options$fix_tree_depth

                } else {

                    tree_depth = c(self$options$min_tree_depth, self$options$max_tree_depth)

                }

                ##### learn_rate #######

                if (self$options$learn_ratex_tune == "fix_learn_ratex"){

                    learn_rate = self$options$fix_learn_ratex

                } else {

                    learn_rate = c(self$options$min_learn_ratex, self$options$max_learn_ratex)

                }

                ##### loss_reduction ######

                if (self$options$loss_reduction_tune == "fix_loss_reduction"){

                    loss_reduction = self$options$fix_loss_reduction

                } else {

                    loss_reduction = c(self$options$min_loss_reduction, self$options$max_loss_reduction)

                }

                hyp_list = list(
                    trees = trees,
                    mtry = mtry,
                    min_n = min_n,
                    tree_depth = tree_depth,
                    learn_rate = learn_rate,
                    loss_reduction = loss_reduction
                )


            } else {

                model_name = "SVM"

                ##### cost #####

                if (self$options$cost_tune == "fix_cost"){

                    cost = self$options$fix_cost

                } else {

                    cost = c(self$options$min_cost, self$options$max_cost)

                }

                #### rbf_sigma #####

                if (self$options$rbf_sigma_tune == "fix_rbf_sigma"){

                    rbf_sigma = self$options$fix_rbf_sigma

                } else {

                    rbf_sigma = c(self$options$min_rbf_sigma, self$options$max_rbf_sigma)

                }

                #### degree ######

                if (self$options$degree_tune == "fix_degree"){

                    degree = self$options$fix_degree

                } else {

                    degree = c(as.numeric(self$options$min_degree),
                               as.numeric(self$options$max_degree)
                    )

                }

                #### scale_factor #####

                if (self$options$scale_factor_tune == "fix_scale_factor"){

                    scale_factor = self$options$fix_scale_factor

                } else {

                    scale_factor = c(self$options$min_scale_factor, self$options$max_scale_factor)

                }

                ##### kernel #####

                if (self$options$kernels == "linear"){

                    type = "linear"

                    hyp_list = list(
                        type = type,
                        cost = cost
                    )

                } else if (self$options$kernels == "rbf"){

                    type = "rbf"

                    hyp_list = list(
                        type = type,
                        cost = cost,
                        rbf_sigma = rbf_sigma
                    )

                } else {

                    type = "poly"

                    hyp_list = list(
                        type = type,
                        cost = cost,
                        degree = degree,
                        scale_factor = scale_factor
                    )

                }

            }

            return(list(model_name = model_name, hyp_list = hyp_list))

        },

        .createNN = function(analysis_object, hyp_list){

            analysis_object <- MLwrap::build_model(analysis_object,"SVM")

            hyp_nn <- HyperparamsNN_nnet$new(hyp_list)

            hidden_units <- if (hyp_nn$hidden_units_tune) tune::tune() else as.integer(hyp_nn$hyperparams_constant$hidden_units)
            penalty <- if (hyp_nn$penalty_tune) tune::tune() else hyp_nn$hyperparams_constant$penalty

            nn_model <- parsnip::mlp(
                hidden_units = !!hidden_units,
                epochs = 100,
                penalty = !!penalty
            )

            nn_model <- parsnip::set_engine(nn_model, "nnet")

            nn_model <- parsnip::set_mode(nn_model, "classification")

            analysis_object$modify("model", nn_model)

            analysis_object$modify("hyperparameters", hyp_nn)

            analysis_object$modify("model_name", "Neural Network")

            return(analysis_object)
        },

        .getTuner = function(){

            if (self$options$tuners == "none"){

                tuner = "Bayesian Optimization"

            } else{

                tuner = "Grid Search CV"

            }

            return(tuner)
        },

        .getSensitivityMethods = function(model_name){

            if (model_name == "SVM" && self$options$kernels != "linear"){

                methods = c("PFI")

            } else{

                methods = c("PFI", "SHAP")

            }

            if (private$.sobol_jansen){

                methods = c(methods, "Sobol_Jansen")

            }

            return(methods)

        },

        .calculateOlden = function(final_model){

            n_in = final_model$fit$fit$fit$n[1]

            n_hidd = final_model$fit$fit$fit$n[2]

            wts = final_model$fit$fit$fit$wts

            w_in = matrix(,nrow = n_in, ncol = n_hidd)

            off_set = 0

            for (h in 1:n_hidd){

                idx <- off_set + 2:(n_in + 1)

                w_in[, h] <- wts[idx]

                off_set <- off_set + n_in + 1
            }

            w_out <- wts[off_set + 2:(n_hidd + 1)]

            olden_imp <- as.data.frame(t(w_in %*% w_out))

            colnames(olden_imp) <- private$.feature_names

            return (olden_imp)

        },


        .finalData = function(){

            dat <- self$data

            formula <- jmvcore::constructFormula(self$options$dep,
                                                 c(self$options$covs, self$options$factors))
            formula <- as.formula(formula)

            private$.form = formula

            ################ Preprocessing #####################

            for (cov in self$options$covs){

                dat[[cov]] <- base::as.numeric(dat[[cov]])

            }

            for (fac in self$options$factors){

                dat[[fac]] <- base::as.factor(dat[[fac]])

            }

            private$.dat = dat

            if (length(self$options$factors) == 0){

                private$.sobol_jansen = TRUE

            }

        },

        .populateSummaryMetrics = function(){

            table <- self$results$summary

            table_res <- MLwrap::table_evaluation_results(self$results$model)

            metrics_set <- private$.getSummaryMetrics(self$options, add_dataset = TRUE)

            table$setRow(rowNo = 1, values = as.list(table_res[metrics_set][1,]))

            table$setRow(rowNo = 2, values = as.list(table_res[metrics_set][2,]))

        },

        .populateHyperparametersTable = function(analysis_object){

                 key = "Best Hyperparameters"

                 self$results$tuner_table$addItem(key = key)

                 table <- self$results$tuner_table[["\"Best Hyperparameters\""]]

                 hyp_names_tune <- names(analysis_object$hyperparameters$hyperparams_ranges)

                 best_hyp <- analysis_object$hyperparameters$hyperparams_constant

                 best_hyp <- c(best_hyp, as.list(tune::show_best(analysis_object$tuner_fit, n = 1))[hyp_names_tune])

                 mean_best_hyp <- tune::show_best(analysis_object$tuner_fit, n = 1)$mean

                 std_best_hyp <- tune::show_best(analysis_object$tuner_fit, n = 1)$std_err

                 for (name in names(best_hyp)){

                     table$addColumn(name = name)

                 }

                 best_hyp$metric <- self$options$metrics

                 best_hyp$estimate <- mean_best_hyp

                 best_hyp$std <- std_best_hyp

                 table$setRow(rowNo = 1, values = best_hyp)

             },

        .populateTunerPlots = function(){

            key = 'Tuner Search Results'

            self$results$plot_tuner$addItem(key = key)

            self$results$plot_tuner[["\"Tuner Search Results\""]]$setState("tuner_search_results")

        },

        .populateResultsPlots = function(){

            if (self$options$plot_roc_curve){

                self$results$plots_results[["\"ROC Curve\""]]$setState("roc_curve")

            }

            if (self$options$plot_pr_curve){

                self$results$plots_results[["\"Precision-Recall Curve\""]]$setState("pr_curve")

            }

            if (self$options$plot_gain_curve){

                self$results$plots_results[["\"Gain Curve\""]]$setState("gain_curve")

            }

            if (self$options$plot_lift_curve){

                self$results$plots_results[["\"Lift Curve\""]]$setState("lift_curve")

            }

            if (self$options$plot_dist_by_class){

                self$results$plots_results[["\"Output Distribution By Class\""]]$setState("dist_by_class")

            }

            if (self$options$plot_reliability_plot){

                self$results$plots_results[["\"Reliability Plot\""]]$setState("reliability_plot")

            }

            if (self$options$plot_confusion_matrix){

                self$results$plots_results[["\"Confusion Matrix\""]]$setState("confusion_matrix")

            }

        },

        .populateSensitivityPlots = function(){

            if (self$options$pfi){

                self$results$plots_sensitivity[["\"Permutation Feature Importance\""]]$setState("PFI_barplot")

            }

            if (self$options$shap_mean){

                self$results$plots_sensitivity[["\"Mean Absolute SHAP\""]]$setState("SHAP_barplot")

            }

            if (self$options$shap_dir){

                self$results$plots_sensitivity[["\"Directional SHAP\""]]$setState("SHAP_directional")

            }

            if (self$options$shap_box){

                self$results$plots_sensitivity[["\"SHAP Boxplot\""]]$setState("SHAP_boxplot")

            }

            if (self$options$shap_swarm){

                self$results$plots_sensitivity[["\"SHAP Swarm Plot\""]]$setState("SHAP_swarmplot")
            }

            if (self$options$olden){

                self$results$plots_sensitivity[["\"Olden Importance\""]]$setState("Olden_barplot")

            }

            if (self$options$sobol){

                self$results$plots_sensitivity[["\"Sobol Importance\""]]$setState("Sobol_Jansen")

            }

        },

        .populateSensitivityTables = function(){

            if (self$options$pfi){

                pfi_res <- self$results$model$sensitivity_analysis$PFI

                table <- self$results$table_sensitivity[["\"Permutation Feature Importance\""]]

                table$addColumn(name = "feature_importance", title = "Mean Importance")

                table$addColumn(name = "std", title = "StDev")

                for (i in 1:length(private$.feature_names)){

                    var_name = pfi_res$Feature[[i]]

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                        var = var_name,
                        feature_importance = pfi_res$Importance[[i]],
                        std = pfi_res$StDev[[i]]

                    ))

                }

            }

            if (any(self$options$shap_mean, self$options$shap_dir, self$options$shap_box, self$options$shap_swarm)){

                shap_res <- MLwrap::table_shap_results(self$results$model)$SHAP

                table <- self$results$table_sensitivity[["\"SHAP Summary Importance\""]]

                table$addColumn(name = "feature_importance", title = "Mean Abs SHAP")

                table$addColumn(name = "std", title = "StDev")

                table$addColumn(name = "dir_shap", title = "Directional SHAP Importance")

                for (i in 1:length(private$.feature_names)){

                    var_name = shap_res$Feature[[i]]

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                        var = var_name,
                        feature_importance = shap_res$Mean_Abs_Importance[[i]],
                        std = shap_res$StDev[[i]],
                        dir_shap = shap_res$Directional_Importance[[i]]

                    ))

                }
            }

            if (self$options$olden){

                olden_imp <- self$results$model$sensitivity_analysis$Olden

                table <- self$results$table_sensitivity[["\"Olden Importance\""]]

                table$addColumn(name = "feature_importance", title = "Importance")

                for (i in 1:length(private$.feature_names)){

                    var_name = colnames(olden_imp)[[i]]

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                        var = var_name,
                        feature_importance = olden_imp[[var_name]]

                    ))

                }

            }

            if (self$options$sobol){

                sobol_res <- MLwrap::table_sobol_jansen_results(self$results$model)

                table <- self$results$table_sensitivity[["\"Sobol-Jansen Sensitivity Importance\""]]

                table$addColumn(name = "first_order", title = "First Order")

                table$addColumn(name = "first_order_std", title = "StDev")

                table$addColumn(name = "first_order_min", title = "Min CI")

                table$addColumn(name = "first_order_max", title = "Max CI")

                table$addColumn(name = "total_order", title = "Total Order")

                table$addColumn(name = "total_order_std", title = "StDev")

                table$addColumn(name = "total_order_min", title = "Min CI")

                table$addColumn(name = "total_order_max", title = "Max CI")

                for (i in 1:length(private$.feature_names)){

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                        var = sobol_res$Feature[[i]],
                        first_order = sobol_res$`First Order (S)`[[i]],
                        first_order_std = sobol_res$`S StErr`[[i]],
                        first_order_min = sobol_res$`S Min CI`[[i]],
                        first_order_max = sobol_res$`S Max CI`[[i]],
                        total_order = sobol_res$`Total Order (T)`[[i]],
                        total_order_std = sobol_res$`T StErr`[[i]],
                        total_order_min = sobol_res$`T Min CI`[[i]],
                        total_order_max = sobol_res$`T Max CI`[[i]]

                    ))

                }

            }

        },
        .populateOutputs = function(){

            if (self$options$pred_class && self$results$pred_class$isNotFilled()) {

                predictions <- private$.make_predictions_class()

                self$results$pred_class$setRowNums(rownames(self$data))
                self$results$pred_class$setValues(predictions)

            }

            if (self$options$pred_prob && self$results$pred_prob$isNotFilled()) {

                predictions <- private$.make_predictions_prob()

                self$results$pred_prob$setRowNums(rownames(self$data))
                self$results$pred_prob$setValues(predictions)

            }

            if (self$options$dataset_id && self$results$dataset_id$isNotFilled()){

                test_id <- self$results$model$data_id$test_data_id

                dataset_id <- rep("Train", nrow(self$data))

                dataset_id[test_id] <- "Test"

                self$results$dataset_id$setRowNums(rownames(self$data))
                self$results$dataset_id$setValues(dataset_id)

            }

        },



        ## Plots

        .plot_tuner = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            p <- self$results$model$plots[[plot_key]]

            print(p)

        },

        .plot_result = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            # Patchwork plots, combine train and test

            if (plot_key %in% c("dist_by_class", "reliability_plot", "confusion_matrix")){

                plot_train <- self$results$model$plots[[paste0(plot_key, "_train")]]

                plot_test <- self$results$model$plots[[paste0(plot_key, "_test")]]

                if (plot_key == "reliability_plot"){

                    plot_train <- plot_train + ggplot2::ggtitle("Reliability Plot (train data)")

                    plot_test <- plot_test + ggplot2::ggtitle("Reliability Plot (test data)")

                }

                p <- patchwork::wrap_plots(plot_train, plot_test, nrow = 2)

            }

            else{

                p <- self$results$model$plots[[plot_key]]
            }

            print(p)

        },

        .plot_sensitivity = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            if (plot_key == "Olden_barplot"){

                olden_imp <- self$results$model$sensitivity_analysis$Olden

                df <- data.frame(
                    variable   = colnames(olden_imp),
                    importance = as.numeric(olden_imp)
                )


                # Order decreasing
                df$variable <- factor(df$variable, levels = df$variable[order(df$importance, decreasing = T)])

                # Plot
                p <- ggplot2::ggplot(df, ggplot2::aes(x = variable, y = importance, fill = importance > 0)) +
                    ggplot2::geom_col(show.legend = FALSE) +
                    ggplot2::geom_text(ggplot2::aes(label = round(importance, 3)),
                                       vjust = ifelse(df$importance >= 0, -0.5, 1.2)) +
                    ggplot2::scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "firebrick")) +
                    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
                    ggplot2::labs(
                        title = "Olden Feature Importance",
                        x = "Feature",
                        y = "Olden Feature Importance"
                    ) +
                    ggplot2::theme_grey() +
                    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

            }

            else {

                p <- self$results$model$plots[[plot_key]]

            }

            print(p)

        },

        # Utilities

        .checkArguments = function(){

            if (self$options$sobol){

                if (length(self$options$factors) > 0){

                    stop("Sobol-Jansen does not allow categorical variables!")

                }

            }

            if ((self$options$mode == "svm") && (self$options$kernels != "linear") &&
                any(self$options$shap_mean, self$options$shap_dir, self$options$shap_box, self$options$shap_swarm)){

                stop("SHAP values for Support Vector Machines are only implemented for the linear kernel!")

            }

            if ((self$options$mode != "neural_network") && (self$options$olden)){

                stop("Olden's method is only implemented for Neural Networks!")

            }

            # Check all stdCovs are in covs and all encCat are in factors

            missing_covs <- setdiff(self$options$stdCovs, self$options$covs)

            missing_factors <- setdiff(self$options$encCat, self$options$factors)

            if (length(missing_covs) > 0) {
                stop(sprintf("Covariate(s) %s is/are not being used. Add it to covariates or remove it from scaling.", paste(missing_covs, collapse = ", ")))
            }

            if (length(missing_factors) > 0) {
                stop(sprintf("Factor(s) %s is/are not being used. Add it to factors or remove it from factor encoding.", paste(missing_factors, collapse = ", ")))
            }

        },

        .make_predictions_class = function(){

            # Extract final model workflow

            final_model <- self$results$model$final_model

            # Make prediction

            predictions <- predict(final_model, private$.dat)

            return(predictions$.pred_class)

        },

        .make_predictions_prob = function(){

            # Extract final model workflow

            final_model <- self$results$model$final_model

            # Make prediction

            predictions <- predict(final_model, private$.dat, type = "prob")

            # Select Positive Class (second element)

            predictions_positive <- predictions[,2][[1]]

            return(predictions_positive)
        }



    )
)
