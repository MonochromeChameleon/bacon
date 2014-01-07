define(['jquery', 'ko'], function ($, ko) {
    "use strict";
    
    var viewModel;
    var searchUrl = '/search';
    var degreesUrl = '/degrees';
    
    function handleSearchResponse(results) {
        if (results.length == 1) {
            selectResult(results[0]);
            return;
        }
        if (results.length == 0) {
            viewModel.searchError(true);
        } else {
            viewModel.searchResults(results);
        }

        viewModel.loading(false);
    }
    
    function handleSearchError() {
        viewModel.searchError(true);
        viewModel.loading(false);
    }
    
    function runSearch() {
        viewModel.loading(true);
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
    
    function handleBaconResult(result) {
        viewModel.loading(false);    
        viewModel.baconResult(result);
    }
    
    function handleBaconError() {
        viewModel.baconError(true);
    }
    
    function selectResult(actor) {
        viewModel.loading(true);
        var url = degreesUrl + '/' + actor.imdbId;
        viewModel.searchResults([]);
        $.ajax({
            dataType: "json",
            url: url,
            success: handleBaconResult,
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
        searchError: ko.observable(false),
        loading: ko.observable(false)
    }


    return {
        init: function (elementId) {
            ko.applyBindings(viewModel, document.getElementById(elementId));
            $('#textSearch').focus();
        }
    }
});