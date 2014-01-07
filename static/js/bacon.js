define(['jquery', 'ko'], function ($, ko) {
    "use strict";
    
    var viewModel;
    var searchUrl = '/search';
    var degreesUrl = '/degrees';
    
    function handleSearchResponse(results) {
        if (results.length > 1) {
            viewModel.searchResults(results);
        } else {
            selectResult(results[0]);
        }
    }
    
    function runSearch() {
        var searchTerm = viewModel.textSearch();
        viewModel.baconResult(undefined);

        var url = searchUrl + '/' + searchTerm.replace(/\s/,'+');
        $.getJSON(url, handleSearchResponse);
    }
    
    function handleResult(result) {
        viewModel.baconResult(result);
    }
    
    function selectResult(actor) {
        var url = degreesUrl + '/' + actor.imdbId;
        viewModel.searchResults([]);
        $.getJSON(url, handleResult);
    }

    var viewModel = {
        textSearch: ko.observable(),
        runSearch: runSearch,
        searchResults: ko.observableArray(),
        selectResult: selectResult,
        baconResult: ko.observable()
    }


    return {
        init: function (elementId) {
            ko.applyBindings(viewModel, document.getElementById(elementId));
            $('#textSearch').focus();
        }
    }
});