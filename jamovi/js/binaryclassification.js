'use strict';

module.exports = {

    covsSupplier_updated: function(ui){

        let covsList = utils.clone(ui.covs.value(), []);
        ui.covsSupplier.setValue(utils.valuesToItems(covsList, FormatDef.variable));

    },

    covs_changed: function(ui) {
        var covsList = utils.clone(ui.covs.value(), []);

        ui.covsSupplier.setValue(utils.valuesToItems(covsList, FormatDef.variable));

    },

    factorsSupplier_updated: function(ui){

        let factorsList = utils.clone(ui.factors.value(), []);

        ui.factorsSupplier.setValue(utils.valuesToItems(factorsList, FormatDef.variable));

    },

    factors_changed: function(ui) {
        var factorsList = utils.clone(ui.factors.value(), []);

        ui.factorsSupplier.setValue(utils.valuesToItems(factorsList, FormatDef.variable));

        if (factorsList.length === 0)
            ui.show_sobol.setValue(true);
        else
            ui.show_sobol.setValue(false);

    },

    mode_changed: function(ui) {

        const currentMode = ui.mode.value();

        if (currentMode === 'neural_network'){
            ui.show_shap.setValue(true);
            ui.show_olden.setValue(true);
        }
        else if (currentMode === 'random_forest'){
            ui.show_shap.setValue(true);
            ui.show_olden.setValue(false);
        }
        else if (currentMode === 'xgboost'){
            ui.show_shap.setValue(true);
            ui.show_olden.setValue(false);
        }
        else if (currentMode === 'svm'){
            ui.show_olden.setValue(false);
            const currentKernel = ui.kernels.value()
            if (currentKernel === "linear")
                ui.show_shap.setValue(true);
            else
                ui.show_shap.setValue(false);
        }
    },

    kernels_changed: function(ui){

        const currentKernel = ui.kernels.value()

        if (currentKernel !== "linear")
            ui.show_shap.setValue(false);
        else
            ui.show_shap.setValue(true);

    },

    show_shap_changed: function(ui){

        const currentSHAP = ui.show_shap.value()

        if (currentSHAP === false){

            ui.shap_mean.setValue(currentSHAP);
            ui.shap_dir.setValue(currentSHAP);
            ui.shap_box.setValue(currentSHAP);
            ui.shap_swarm.setValue(currentSHAP);

        }
    },

    show_olden_changed: function(ui){

        const currentOlden = ui.show_olden.value()

        if (currentOlden === false){

            ui.olden.setValue(false);

        }

    },

    show_sobol_changed: function(ui){

        const currentSobol = ui.show_sobol.value()

        if (currentSobol === false)
            ui.sobol.setValue(false);

    }

};
