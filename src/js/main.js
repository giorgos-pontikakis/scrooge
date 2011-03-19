$(document).ready(function () {
                      applyAutocomplete();
                      applyDatepicker();
                      applyCheckUnsaved();
                  });

function applyDatepicker () {
    $.datepicker.setDefaults({dateFormat: "dd/mm/yy"});
    $(".datepicker").datepicker();
};

function applyAutocomplete () {
    var id, table, col, source;
    var options = {minLength: 2};
    var autocompleteIDs = ["company", "tof", "city", "bank", "account", "nonchequing-account"] ;
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

function applyCheckUnsaved () {

    var message = "Έχετε κάνει αλλαγές στη φόρμα. "
                  + "Αν φύγετε από αυτή τη σελίδα οι αλλαγές αυτές θα χαθούν.";

    window.onbeforeunload = function () {

        var isDirty = false;

        $(":input:not(:submit)").each(function () {
                                          if ($(this).data("initialValue") != $(this).val()) {
                                              isDirty = true;
                                          }
                                      });

        if (isDirty === true) {
            return message;
        }
    };

    $(":input:not(:submit)").each(function() {
                                      if ($(this).attr("type") === "radio") {
                                          $(this).data("initialValue", $(this).attr("checked"));
                                      } else {
                                          $(this).data("initialValue", $(this).val());
                                      }
                                  });

    $(":submit").click(function(){
                           window.onbeforeunload = undefined;
                       });

}
