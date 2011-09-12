$(document).ready(function () {
                      applyAutocomplete();
                      applyDatepicker();
                  });

function applyDatepicker () {
    $.datepicker.setDefaults({dateFormat: "dd/mm/yy"});
    $(".datepicker").datepicker();
};

function applyAutocomplete () {
    var id, table, col, source;
    var options = {minLength: 2};
    var autocompleteIDs = ["company", "tof", "city", "bank", "account", 
                           ["occupation", "company", "occupation"], "nonchequing-account"] ;
    for ( var i = 0; i < autocompleteIDs.length; i++) {
        if ((typeof autocompleteIDs[i]) === "string") {
            table = autocompleteIDs[i];
            col = "title";
            options.source = "/scrooge/autocomplete?table=" + table + "&column=" + col;
            id = table;
        } else {
            table = autocompleteIDs[i][1];
            col = autocompleteIDs[i][2] || "title";
            options.source = "/scrooge/autocomplete?table=" + table + "&column=" + col;
            id = autocompleteIDs[i][0];
        }
        $(".ac-" + id).autocomplete(options); 
    }
}

