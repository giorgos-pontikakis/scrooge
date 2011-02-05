$(document).ready(function () {
                      applyAutocomplete();
                      applyCheckUnsaved();
                  });


function applyAutocomplete () {
    var table, source;
    var col = "title";
    var autocompleteIDs = ["company", "tof", "city", "bank", "debit-account", "credit-account"];
    for ( var i = 0; i < autocompleteIDs.length; i++) {
        table = autocompleteIDs[i];
        source = "/scrooge/autocomplete?table=" + table + "&column=" + col;
        $("#" + table).autocomplete({source: source, minLength: 2});
    }
}

function applyCheckUnsaved () {

    var message = "Έχετε κάνει αλλαγές στη φόρμα. Αν φύγετε από αυτή τη σελίδα οι αλλαγές αυτές θα χαθούν.";

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
