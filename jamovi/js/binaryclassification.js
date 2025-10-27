'use strict';

module.exports = {

    covsSupplier_updated: function(ui){

        let covsList = utils.clone(ui.covs.value(), []);
        ui.covsSupplier.setValue(utils.valuesToItems(covsList, FormatDef.variable));

    },

    covs_changed: function(ui) {
        var covsList = utils.clone(ui.covs.value(), []);

        // actualizar el supplier de covs
        ui.covsSupplier.setValue(utils.valuesToItems(covsList, FormatDef.variable));

    },

    factorsSupplier_updated: function(ui){

        let factorsList = utils.clone(ui.factors.value(), []);
        ui.factorsSupplier.setValue(utils.valuesToItems(factorsList, FormatDef.variable));

    },

    factors_changed: function(ui) {
        var factorsList = utils.clone(ui.factors.value(), []);

        // actualizar el supplier de covs
        ui.factorsSupplier.setValue(utils.valuesToItems(factorsList, FormatDef.variable));

    }

};
