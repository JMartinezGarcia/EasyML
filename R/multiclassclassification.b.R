
# This file is a generated template, your changes will not be overwritten

MulticlassClassificationClass <- if (requireNamespace('jmvcore', quietly=TRUE)) R6::R6Class(
    "MulticlassClassificationClass",
    inherit = MulticlassClassificationBase,
    private = list(

        ## Member Variables?

        .dat = NA,
        .form = NA,
        .feature_names = NA,
        .class_names = NA,

        ## Init

        .init = function(){

            private$.dat <- NULL
            private$.form <- NULL
            private$.feature_names <- NULL

            private$.checkArguments()

            # Get output class names

            if (!is.null(self$options$dep_level)){

                # Remove quotations and trim white spaces

                dep_lvls <- base::scan(text = self$options$dep_level,
                                       what = "", sep = ",", quote = "\"", quiet = TRUE)
                dep_lvls <- base::trimws(dep_lvls)

                private$.check_levels(dep_lvls)

                private$.class_names <- dep_lvls

            }

            else {

                if (!is.null(self$options$dep)){

                    private$.class_names <- levels(as.factor(self$data[[self$options$dep]]))

                }

            }

            private$.initSummaryTable()
            private$.initResultsPlots()
            private$.initSensitivityPlots()
            private$.initSensitivityTables()
            private$.initOutputs()


        },

        .initSummaryTable = function(){

            metrics_set <- private$.getSummaryMetrics(self$options)

            #self$results$summary$addItem(key = "Train Evaluation Summary")

            #self$results$summary$addItem(key = "Test Evaluation Summary")

            train_summary <- self$results$summary_train

            test_summary <- self$results$summary_test

            for (metric in metrics_set){

                train_summary$addColumn(name = metric)

                test_summary$addColumn(name = metric)

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

            if (self$options$plot_confusion_matrix){

                key = 'Confusion Matrix'

                self$results$plots_results$addItem(key = key)

            }

        },

        .initSensitivityPlots = function(){

            if (self$options$pfi){

                for (class_name in private$.class_names){

                    key = paste0("Permutation Feature Importance (", class_name, ")")

                    self$results$plots_pfi$addItem(key = key)

                }

            }

            if (self$options$shap_mean){

                for (class_name in private$.class_names){

                    key = paste0("Mean Absolute SHAP (", class_name, ")")

                    self$results$plots_shap$addItem(key = key)

                }

            }

            if (self$options$shap_dir){

                for (class_name in private$.class_names){

                    key = paste0("Directional SHAP (", class_name, ")")

                    self$results$plots_shap$addItem(key = key)

                }

            }

            if (self$options$shap_box){

                for (class_name in private$.class_names){

                    key = paste0("SHAP Boxplot (", class_name, ")")

                    self$results$plots_shap$addItem(key = key)

                }

            }

            if (self$options$shap_swarm){

                for (class_name in private$.class_names){

                    key = paste0("SHAP Swarm Plot (", class_name, ")")

                    self$results$plots_shap$addItem(key = key)

                }

            }

            if (self$options$olden){

                for (class_name in private$.class_names){

                    key = paste0("Olden Importance Plot (", class_name, ")")

                    self$results$plots_olden$addItem(key = key)

                }

            }

        },

        .initSensitivityTables = function(){

            # Create a table for each (method x class)

            if (self$options$pfi){

                for (class_name in private$.class_names){

                key = paste0("Permutation Feature Importance (", class_name, ")")

                self$results$table_pfi$addItem(key = key)

                }
            }

            if (any(c(self$options$shap_mean, self$options$shap_dir,
                      self$options$shap_box, self$options$shap_swarm))){

                for (class_name in private$.class_names){

                key = paste0('SHAP Summary Importance (', class_name, ")")

                self$results$table_shap$addItem(key = key)

                }

            }

            if (self$options$olden){

                key = "Olden Importance"

                self$results$table_olden$addItem(key = key)

                }


        },

        .initOutputs = function(){

            n_classes <- length(private$.class_names)

            titles <- c()

            descriptions <- c()

            for (class_name in private$.class_names){

                titles <- c(titles, paste0("Probability (", class_name, ")"))

                descriptions <- c(descriptions, paste0("Output Probabilities for class ", class_name))

            }

            self$results$pred_prob$set(seq(n_classes), titles, descriptions, rep("continuous", n_classes))

        },

        .getSummaryMetrics = function(options, add_class = FALSE){

            if (add_class){

                metrics_set <- c("Class")

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

        ## Run

        .run = function(){

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

                private$.feature_names <- private$.getFeatureNames(self$results$model)

            }

            else{

                private$.compute()

            }

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

                olden_results <- private$.calculateOlden(analysis_object)

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

                if (self$options$hidden_neurons_tune == "none"){

                    hidden_units = NULL

                } else if (self$options$hidden_neurons_tune == "fix_neurons"){

                    hidden_units = self$options$fix_n_neurons

                } else {

                    hidden_units = c(self$options$min_n_neurons, self$options$max_n_neurons)

                }

                ######### penalty ###########

                if (self$options$penalty_tune == "none"){

                    penalty = NULL

                } else if (self$options$penalty_tune == "fix_penalty"){

                    penalty = self$options$fix_penalty

                } else {

                    penalty = c(self$options$min_penalty, self$options$max_penalty)

                }

                ######### activation ###########

                hyp_list = list(
                    hidden_units = hidden_units,
                    penalty = penalty
                )


            } else if (self$options$mode == "random_forest"){

                model_name = "Random Forest"

                ##### mtry ######

                if (self$options$mtry_tune == "none"){

                    mtry = NULL

                } else if (self$options$mtry_tune == "fix_mtry"){

                    mtry = self$options$fix_mtry

                } else {

                    mtry = c(self$options$min_mtry, self$options$max_mtry)

                }

                ##### trees ######

                if (self$options$trees_tune == "none"){

                    trees = NULL

                } else if (self$options$trees_tune == "fix_trees"){

                    trees = self$options$fix_trees

                } else {

                    tress = c(self$options$min_trees, self$options$max_trees)

                }

                ##### min_n ######

                if (self$options$min_n_tune == "none"){

                    min_n = NULL

                } else if (self$options$min_n_tune == "fix_min_n"){

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

                if (self$options$mtryx_tune == "none"){

                    mtry = NULL

                } else if (self$options$mtryx_tune == "fix_mtryx"){

                    mtry = self$options$fix_mtryx

                } else {

                    mtry = c(self$options$min_mtryx, self$options$max_mtryx)

                }

                ##### trees ######

                if (self$options$treesx_tune == "none"){

                    trees = NULL

                } else if (self$options$treesx_tune == "fix_treesx"){

                    trees = self$options$fix_treesx

                } else {

                    tress = c(self$options$min_treesx, self$options$max_treesx)

                }

                ##### min_n ######

                if (self$options$min_nx_tune == "none"){

                    min_n = NULL

                } else if (self$options$min_nx_tune == "fix_min_nx"){

                    min_n = self$options$fix_min_nx

                } else {

                    min_n = c(self$options$min_min_nx, self$options$max_min_nx)

                }

                ##### tree_depth #######

                if (self$options$tree_depth_tune == "none"){

                    tree_depth = NULL

                } else if (self$options$tree_depth_tune == "fix_tree_depth"){

                    tree_depth = self$options$fix_tree_depth

                } else {

                    tree_depth = c(self$options$min_tree_depth, self$options$max_tree_depth)

                }

                ##### learn_rate #######

                if (self$options$learn_ratex_tune == "none"){

                    learn_rate = NULL

                } else if (self$options$learn_ratex_tune == "fix_learn_ratex"){

                    learn_rate = self$options$fix_learn_ratex

                } else {

                    learn_rate = c(self$options$min_learn_ratex, self$options$max_learn_ratex)

                }

                ##### loss_reduction ######

                if (self$options$loss_reduction_tune == "none"){

                    loss_reduction = NULL

                } else if (self$options$loss_reduction_tune == "fix_loss_reduction"){

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

                if (self$options$cost_tune == "none"){

                    cost = NULL

                } else if (self$options$cost_tune == "fix_cost"){

                    cost = self$options$fix_cost

                } else {

                    cost = c(self$options$min_cost, self$options$max_cost)

                }

                #### margin #####

                if (self$options$margin_tune == "none"){

                    margin = NULL

                } else if (self$options$margin_tune == "fix_margin"){

                    margin = self$options$fix_margin

                } else {

                    margin = c(self$options$min_margin, self$options$max_margin)

                }

                #### rbf_sigma #####

                if (self$options$rbf_sigma_tune == "none"){

                    rbf_sigma = NULL

                } else if (self$options$rbf_sigma_tune == "fix_rbf_sigma"){

                    rbf_sigma = self$options$fix_rbf_sigma

                } else {

                    rbf_sigma = c(self$options$min_rbf_sigma, self$options$max_rbf_sigma)

                }

                #### degree ######

                if (self$options$degree_tune == "none"){

                    degree = NULL

                } else if (self$options$degree_tune == "fix_degree"){

                    degree = self$options$fix_degree

                } else {

                    degree = c(self$options$min_degree, self$options$max_degree)

                }

                #### scale_factor #####

                if (self$options$scale_factor_tune == "none"){

                    scale_factor = NULL

                } else if (self$options$scale_factor_tune == "fix_scale_factor"){

                    scale_factor = self$options$fix_scale_factor

                } else {

                    scale_factor = c(self$options$min_scale_factor, self$options$max_scale_factor)

                }

                ##### kernel #####

                if (self$options$kernels == "linear"){

                    type = "linear"

                    hyp_list = list(
                        type = type,
                        cost = cost,
                        margin = margin
                    )

                } else if (self$options$kernels == "rbf"){

                    type = "rbf"

                    hyp_list = list(
                        type = type,
                        cost = cost,
                        margin = margin,
                        rbf_sigma = rbf_sigma
                    )

                } else {

                    type = "poly"

                    hyp_list = list(
                        type = type,
                        cost = cost,
                        margin = margin,
                        degree = degree,
                        scale_factor = scale_factor
                    )

                }

            }

            return(list(model_name = model_name, hyp_list = hyp_list))

        },

        .createNN = function(analysis_object, hyp_list){

            analysis_object <- MLwrap::build_model(analysis_object,"Random Forest")

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

            return(methods)

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


        },

        .calculateOlden = function(analysis_object){

            # n_input, n_hidden, n_classes, weights

            n_inp <- analysis_object$final_model$fit$fit$fit$n[1]
            n_hidd <- analysis_object$final_model$fit$fit$fit$n[2]
            n_classes <- analysis_object$final_model$fit$fit$fit$n[3]
            wts <- analysis_object$final_model$fit$fit$fit$wts

            # W_in (n_inp * n_hidd)

            W_in <- matrix(data = NA, nrow = n_inp, ncol = n_hidd)

            # W_out (n_hidd * n_classes)

            W_out <- matrix(data = NA, nrow = n_hidd, ncol = n_classes)

            # Iterate W_in (skip bias)

            for (hid in seq_len(n_hidd)){

                idx <- (hid - 1) * (n_inp + 1)

                W_in[,hid] <- wts[(idx + 2):(idx + n_inp + 1)]

            }


            # Iterate W_out (skip bias)

            for (c in seq_len(n_classes)){

                idx <- (c - 1) * (n_hidd + 1) + ((n_inp + 1) * n_hidd)

                W_out[,c] <- wts[(idx + 2):(idx + n_hidd + 1)]

            }

            # Compute values (n_class * n_feat)

            olden_res <- as.data.frame(t(W_in %*% W_out))

            # Add Feature names and class names

            colnames(olden_res) <- private$.feature_names
            olden_res$class <- private$.class_names

            #olden_res <- olden_res %>% dplyr::relocate("class")

            return(olden_res)

        },

        .populateSummaryMetrics = function(){

            # Train And Test Summary Tables

            table_train <- self$results$summary_train

            table_test <- self$results$summary_test

            # Evaluation Results

            eval_res <- MLwrap::table_evaluation_results(self$results$model)

            eval_train <- eval_res$summary_train

            eval_test <- eval_res$summary_test

            # Add evaluation results for each class

            metrics_set <- private$.getSummaryMetrics(self$options, add_class = TRUE)

            for (i in seq(self$results$model$outcome_levels)){

                table_train$addRow(rowKey = i)

                table_test$addRow(rowKey = i)

                table_train$setRow(rowNo = i, values = as.list(eval_train[metrics_set][i,]))

                table_test$setRow(rowNo = i, values = as.list(eval_test[metrics_set][i,]))

            }

        },

        .populateHyperparametersTable = function(analysis_object){

            key = "Best Hyperparameters"

            self$results$tuner_table$addItem(key = key)

            table <- self$results$tuner_table[["\"Best Hyperparameters\""]]

            hyp_names_tune <- names(analysis_object$hyperparameters$hyperparams_ranges)

            best_hyp <- analysis_object$hyperparameters$hyperparams_constant

            ### WHAT IF TUNER IS NULL??

            best_hyp <- c(best_hyp, as.list(tune::show_best(analysis_object$tuner_fit, n = 1))[hyp_names_tune])

            mean_best_hyp <- tune::show_best(analysis_object$tuner_fit, n = 1)$mean

            std_best_hyp <- tune::show_best(analysis_object$tuner_fit, n = 1)$std_err

            for (name in names(best_hyp)){

                table$addColumn(name = name)

            }

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

            if (self$options$plot_confusion_matrix){

                self$results$plots_results[["\"Confusion Matrix\""]]$setState("confusion_matrix")

            }

        },

        .populateSensitivityTables = function(){

          # Populate PFI

          if (self$options$pfi){

              private$.populate_pfi_tables()

          }

          # Populate SHAP

            if (any(self$options$shap_mean, self$options$shap_dir,
                    self$options$shap_box, self$options$shap_swarm)){

                private$.populate_shap_tables()

            }

            # Populate Olden

            if (self$options$olden){

                private$.populate_olden_tables()

            }

        },

        .populate_pfi_tables = function(){

            pfi_res <- MLwrap::table_pfi_results(self$results$model)

            for (class_name in private$.class_names){

                pfi_class <- pfi_res[[paste0("PFI_", class_name)]]

                table_key <- paste0("Permutation Feature Importance (", class_name, ")")

                table <- self$results$table_pfi[[paste0("\"", table_key, "\"")]]

                table$addColumn(name = "feature_importance", title = "Mean Importance")

                table$addColumn(name = "std", title = "StDev")

                for (i in 1:length(private$.feature_names)){

                    var_name = pfi_class$Feature[[i]]

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                    var = var_name,
                    feature_importance = pfi_class$Importance[[i]],
                    std = pfi_class$StDev[[i]]

                ))

                }

            }

        },

        .populate_shap_tables = function(){

            shap_res <- MLwrap::table_shap_results(self$results$model)

            for (class_name in private$.class_names){

                shap_class <- shap_res[[paste0("SHAP_", class_name)]]

                table_key <- paste0("SHAP Summary Importance (", class_name, ")")

                table <- self$results$table_shap[[paste0("\"", table_key, "\"")]]

                table$addColumn(name = "mean_abs", title = "Mean Absolute SHAP")

                table$addColumn(name = "std", title = "StDev")

                table$addColumn(name = "dir_shap", title = "Directional SHAP Importance")

                for (i in 1:length(private$.feature_names)){

                    var_name = shap_class$Feature[[i]]

                    table$addRow(rowKey = i)

                    table$setRow(rowNo = i, values = list(

                        var = var_name,
                        mean_abs = shap_class$Mean_Abs_Importance[[i]],
                        std = shap_class$StDev[[i]],
                        dir_shap = shap_class$Directional_Importance[[i]]

                    ))

                }

            }

        },

        .populate_olden_tables = function(){

        # Retrieve olden results

        olden_res <- self$results$model$sensitivity_analysis$Olden

        # Add feature columns to table

        table_key <- "Olden Importance"

        table <- self$results$table_olden[[paste0("\"", table_key, "\"")]]

        for (feature in private$.feature_names){

            table$addColumn(name = feature, title = feature)

        }

        # Add rows to table


        for (i in 1:length(private$.class_names)){

            table$addRow(rowKey = i)

            table$setRow(rowNo = i, values = olden_res[i,])

        }



        },

        .populateSensitivityPlots = function(){

            if (self$options$pfi){

                for (class_name in private$.class_names){

                    class_key <- paste0("Permutation Feature Importance (", class_name, ")")

                    self$results$plots_pfi[[paste0("\"", class_key, "\"")]]$setState(paste0("PFI_", class_name, "_barplot"))

                }
            }

            if (self$options$shap_mean){

                for (class_name in private$.class_names){

                    class_key <- paste0("Mean Absolute SHAP (", class_name, ")")

                    self$results$plots_shap[[paste0("\"", class_key, "\"")]]$setState(paste0("SHAP_", class_name, "_barplot"))

                }

            }

            if (self$options$shap_dir){

                for (class_name in private$.class_names){

                    class_key <- paste0("Directional SHAP (", class_name, ")")

                    self$results$plots_shap[[paste0("\"", class_key, "\"")]]$setState(paste0("SHAP_", class_name, "_directional"))

                }

            }

            if (self$options$shap_box){

                for (class_name in private$.class_names){

                    class_key <- paste0("SHAP Boxplot (", class_name, ")")

                    self$results$plots_shap[[paste0("\"", class_key, "\"")]]$setState(paste0("SHAP_", class_name, "_boxplot"))

                }

            }

            if (self$options$shap_swarm){

                for (class_name in private$.class_names){

                    class_key <- paste0("SHAP Swarm Plot (", class_name, ")")

                    self$results$plots_shap[[paste0("\"", class_key, "\"")]]$setState(paste0("SHAP_", class_name, "_swarmplot"))

                }

            }

            if (self$options$olden){

                for (class_name in private$.class_names){

                    class_key = paste0("Olden Importance Plot (", class_name, ")")

                    self$results$plots_olden[[paste0("\"", class_key, "\"")]]$setState(class_name)

                }

            }

        },

        .populateOutputs = function(){

            # Predicted Class

            if (self$options$pred_class && self$results$pred_class$isNotFilled()) {

                predictions_class <- private$.make_predictions_class()

                self$results$pred_class$setRowNums(rownames(self$data))
                self$results$pred_class$setValues(predictions_class)

            }

            # Output Probability for each class

            if (self$options$pred_prob && self$results$pred_prob$isNotFilled()) {

                predictions_prob <- private$.make_predictions_prob()

                self$results$pred_prob$setRowNums(rownames(self$data))

                for (i in seq_along(private$.class_names)){

                    pred_key = paste0(".pred_", private$.class_names[[i]])

                    self$results$pred_prob$setValues(index = i, predictions_prob[[pred_key]])
                }

            }

            if (self$options$dataset_id && self$results$dataset_id$isNotFilled()){

                test_id <- self$results$model$data_id$test_data_id

                dataset_id <- rep("Train", nrow(self$data))

                dataset_id[test_id] <- "Test"

                self$results$dataset_id$setRowNums(rownames(self$data))
                self$results$dataset_id$setValues(dataset_id)


            }

        },

        .plot_results = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            # Patchwork plots, combine train and test

            if (plot_key %in% c("dist_by_class", "confusion_matrix")){

                plot_train <- self$results$model$plots[[paste0(plot_key, "_train")]]

                plot_test <- self$results$model$plots[[paste0(plot_key, "_test")]]

                p <- patchwork::wrap_plots(plot_train, plot_test, nrow = 2)

            }

            else{

                p <- self$results$model$plots[[plot_key]]
            }

            print(p)

        },

        .plot_tuner = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            p <- self$results$model$plots[[plot_key]]

            print(p)

        },

        .plot_pfi = function(image, ...){

          plot_key <- image$state

          if (is.null(plot_key)){return(FALSE)}

          p <- self$results$model$plots[[plot_key]]

          print(p)

        },

        .plot_shap = function(image, ...){

            plot_key <- image$state

            if (is.null(plot_key)){return(FALSE)}

            p <- self$results$model$plots[[plot_key]]

            print(p)

        },

        .plot_olden = function(image, ...){

            # Retrieve class key

            class_key <- image$state

            if (is.null(class_key)){return(FALSE)}

            # Retrieve olden importance

            olden_imp <- self$results$model$sensitivity_analysis$Olden

            # Select class form class_key

            olden_imp <- olden_imp %>% dplyr::filter(class == class_key) %>%
                          dplyr::select(-class)

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
                    title = paste0("Olden Feature Importance (", class_key, ")"),
                    x = "Feature",
                    y = "Olden Feature Importance"
                ) +
                ggplot2::theme_grey() +
                ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))

            print(p)

        },

        ### Utilities

        .check_levels = function(dep_levels){

            if (length(dep_levels) > 1){

                if (!base::setequal(levels(as.factor(self$data[[self$options$dep]])), dep_levels)){

                    stop(paste0(
                        "Specified dependent variable's levels do not match true levels. ",
                        "Accepted levels: ",
                        paste(levels(as.factor(self$data[[self$options$dep]])), collapse = ", ")
                    ))

                }
            }
        },

        .checkArguments = function(){

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

            # Extract final model

            final_model <- self$results$model$final_model

            # Make prediction

            predictions <- predict(final_model, private$.dat)

            return(predictions$.pred_class)

        },

        .make_predictions_prob = function(){

            # Extract final model

            final_model <- self$results$model$final_model

            # Make prediction (type = prob)

            predictions <- predict(final_model, private$.dat, type = "prob")

            return(predictions)
        }


        )
)
