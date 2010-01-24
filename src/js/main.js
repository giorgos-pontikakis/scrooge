
$(document).ready(loadAll);

var autocompleteDefaults = {minChars: 1, 
			    selectFirst: false,
			    matchContains: true,
			    matchCase: true};

function loadAll () {
   makeAutocomplete("company", "#company");
   makeAutocomplete("tx-type", "#tx-type");
   makeAutocomplete("tof", "#tof");
   makeAutocomplete("city", "#city");
   makeAutocomplete("bank", "#bank");
   makeAutocomplete("debit-account", "#debit-acc");
   makeAutocomplete("credit-account", "#credit-acc");
}


function makeAutocomplete (table, id, result_id) {
   var autocompleteOptions = autocompleteDefaults;
   autocompleteOptions["extraParams"] = {table: table};
   $(id)
      .flushCache()
      .autocomplete("/scrooge/autocomplete", autocompleteOptions)
      .blur(function () {
	       $(this).search();
	    });
}

