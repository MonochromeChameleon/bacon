define(["knockout/knockout-min"], function(ko) {
    "use strict";

    /**
     * RETURN key binding for Knockout.js
     */
    ko.bindingHandlers.returnKey = {
        init: function(element, valueAccessor, allBindingsAccessor, viewModel) {
            ko.utils.registerEventHandler(element, 'keydown', function(evt) {
                if (evt.keyCode === 13) {
                    evt.preventDefault();
                    evt.target.blur();
                    valueAccessor().call(viewModel);
                }
            });
        }
    };
    
    return ko;
});