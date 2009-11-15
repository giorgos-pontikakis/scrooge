
$(document).ready(loadAll);

var autocompleteDefaults = {minChars: 1, 
			    selectFirst: false,
			    matchContains: true,
			    matchCase: true};

function loadAll () {
   autocompleteCompanies();
   autocompleteTxTypes();
}


function autocompleteCompanies () {
   var autocompleteOptions = autocompleteDefaults;
   autocompleteOptions["extraParams"] = {table: "company"};
   $("#company-title")
      .flushCache()
      .autocomplete("/scrooge/autocomplete", autocompleteOptions)
      .result(function (event, data, formatted) {
		 if (data) {
   		    $("#company-id").val(data[1]);
		 } else {
		    $("#company-id").val("");
		 }
   	      })
      .blur(function () {
	       $(this).search();
	    });

}

function autocompleteTxTypes () {
   var autocompleteOptions = autocompleteDefaults;
   autocompleteOptions["extraParams"] = {table: "tx-type"};
   $("#tx-type-title")
      .flushCache()
      .autocomplete("/scrooge/autocomplete", autocompleteOptions)
      .result(function (event, data, formatted) {
		 if (data) {
   		    $("#tx-type-id").val(data[1]);
		 } else {
		    $("#tx-type-id").val("");
		 }
   	      })
      .blur(function () {
	       $(this).search();
	    });
}
