define(['jquery', 'ko'], function ($, ko) {
    "use strict";
    
    var viewModel;
    var searchUrl = '/search';
    var degreesUrl = '/degrees';
    
    function handleSearchResponse(results) {
        if (results.length == 0) {
            viewModel.searchError(true);
        } else if (results.length > 1) {
            viewModel.searchResults(results);
        } else {
            selectResult(results[0]);
        }
    }
    
    function handleSearchError() {
        viewModel.searchError(true);
    }
    
    function runSearch() {
        var searchTerm = viewModel.textSearch();
        viewModel.baconResult(undefined);
        viewModel.baconError(false);
        viewModel.searchError(false);

        var url = searchUrl + '/' + searchTerm.replace(/\s/,'+');
        $.ajax({
            dataType: "json",
            url: url,
            success: handleSearchResponse,
            error: handleSearchError
        });
    }
    
    function handleBaconError() {
        viewModel.baconError(true);
    }
    
    function selectResult(actor) {
        var url = degreesUrl + '/' + actor.imdbId;
        viewModel.searchResults([]);
        $.ajax({
            dataType: "json",
            url: url,
            success: viewModel.baconResult,
            error: handleBaconError
        });
    }

    var viewModel = {
        textSearch: ko.observable(),
        runSearch: runSearch,
        searchResults: ko.observableArray(),
        selectResult: selectResult,
        baconResult: ko.observable(),
        baconError: ko.observable(false),
        searchError: ko.observable(false)
    }


    return {
        init: function (elementId) {
            ko.applyBindings(viewModel, document.getElementById(elementId));
            $('#textSearch').focus();
        }
    }
});