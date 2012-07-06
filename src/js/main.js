$(document).ready(function () {
   applyAutocomplete();
   applyDatepicker();
   selectableRows();
   autoSelectAccount();
   settingsUI();
});

function settingsUI () {
   // Prevent double submit: Disable submit buttons when clicked once
   $('form').submit(function(){
      $('[type="submit"]', this).attr("disabled", "disabled");
   });
   // Disable spellchecking on textareas
   $('textarea').attr('spellcheck',false);
}

function applyDatepicker () {
   $.datepicker.setDefaults({dateFormat: "dd/mm/yy"});
   $(".datepicker").datepicker();
};

function applyAutocomplete () {
   var id, table, col, root;
   var options = {minLength: 2};
   var autocompleteIDs = ["company",
                          ["occupation", "company", "occupation"],
                          ["project", "project", "description"],
                          "tof", "city", "bank", "account", "temtx",
                          "chq-account", "non-chq-account"];
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

   var autocompleteAccountIDs = [["revenues", "revenues-root-account"],
                                 ["expenses", "expenses-root-account"]];
   for ( var i = 0; i < autocompleteAccountIDs.length; i++) {
      id = autocompleteAccountIDs[i][0];
      root = autocompleteAccountIDs[i][1];
      options.source = "/scrooge/autocomplete-accounts?root=" + root;
      $(".ac-" + id).autocomplete(options);
   }

}


function selectableRows () {

   // crud-table
   $(".crud-table input").click(function (e){
      e.stopPropagation();
   });

   $(".crud-table select").click(function (e){
      e.stopPropagation();
   });

   $(".crud-table tr td.selector a").click(function(e) {
      e.stopPropagation();
      window.location = $(this).attr("href");
   });

   $(".crud-table tr").click(function () {
      $(this).children("td.selector").children("a").trigger("click");
   });

   $("form .crud-table tr").unbind("click");

   // crud-tree
   $(".crud-tree li > div a").click(function(e) {
      e.stopPropagation();
      window.location = $(this).attr("href");
   });

   $(".crud-tree li > div").click(function () {
      $(this).children(".selector").children("a").trigger("click");
   });

   $("form .crud-tree li > div").unbind("click");

   // crud-tree radio inputs
   $(".crud-tree input[type=radio]").click(function (e) {
      e.stopPropagation();
      $(".crud-tree li.selected").removeClass("selected");
      $(this).parent().parent().parent().addClass("selected");
   });

}

function autoSelectAccount () {
   $("input.ac-company").bind("autocompleteclose", clickAccount);
   $("input.ac-company").bind("blur", clickAccount);
}

function clickAccount () {
   var title = $(this).val();
   var url = "/scrooge/company/accounts?title=" + title;
   $.getJSON(url, function (data) {
      if (data.cashOnly) {
         // only accepting cash, hide accounts payable/receivable
         $(".cash-only-hidden").hide();
         var isRevenue = $(".company-dependent input[value=" + data.accountIDs[0] + "]").length === 1;
         var accountID = isRevenue ? data.accountIDs[1][0] : data.accountIDs[1][1];
         var jq = ".company-dependent input[type=radio][value=" + accountID +  "]";
         $(jq).click();
      } else {
         // accepts debits/credits; show accounts payable/receivable
         $(".cash-only-hidden").show();
         $(".cash-only-hidden input:first").click();
      }
   });
}