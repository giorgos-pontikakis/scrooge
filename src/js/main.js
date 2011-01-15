$(document).ready(loadAll);



function loadAll () {
   makeAutocomplete("company", "title", "#company");
   makeAutocomplete("company", "occupation", "#occupation");
   makeAutocomplete("tof", "title", "#tof");
   makeAutocomplete("city", "title", "#city");
   makeAutocomplete("bank", "title", "#bank");
   makeAutocomplete("account", "title", "#debit-account");
   makeAutocomplete("account", "title", "#credit-account");
}

function makeAutocomplete (table, col, id) {
   $(id).autocomplete({source: "/scrooge/autocomplete?table=" + table + "&column=" + col,
                       minLength: 2
               })
}