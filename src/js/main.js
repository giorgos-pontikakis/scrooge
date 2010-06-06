
$(document).ready(loadAll);

var autocompleteDefaults = {minChars: 1, 
			    selectFirst: false,
			    matchContains: true,
			    matchCase: true};

function loadAll () {
   makeAutocomplete("company", "title", "#company");
   makeAutocomplete("tx-type", "title", "#tx-type");
   makeAutocomplete("tof", "title", "#tof");
   makeAutocomplete("city", "title", "#city");
   makeAutocomplete("bank", "title", "#bank");
   makeAutocomplete("account", "title", "#debit-acc");
   makeAutocomplete("account", "title", "#credit-acc");
}


// function makeAutocomplete (table, id, result_id) {
//    var autocompleteOptions = autocompleteDefaults;
//    autocompleteOptions["extraParams"] = {table: table};
//    $(id)
//       .flushCache()
//       .autocomplete("/scrooge/autocomplete", autocompleteOptions)
//       .blur(function () {
// 	       $(this).search();
// 	    });
// }

function makeAutocomplete (table, col, id) {
   $(id).autocomplete({source: "/scrooge/autocomplete" +
		       "?table=" + table
		       + "&column=" + col,
		       minLength: 1})

}