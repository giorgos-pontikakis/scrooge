$(document).ready(function () {
   applyAutocomplete();
   applyDatepicker();
   selectableRows();
});

function applyDatepicker () {
   $.datepicker.setDefaults({dateFormat: "dd/mm/yy"});
   $(".datepicker").datepicker();
};

function applyAutocomplete () {
   var id, table, col, source;
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
}


function selectableRows () {
   // $("tr").click(function (event) {
   //    var href = $(this).children("td.selector").children("a").attr("href");
   //    window.location.href = window.location.origin + href;
   // });

   $("tr td.selector a img").click(function(e) {
      e.stopPropagation();
   });

   $("tr").click(function () {
      $(this).children("td.selector").children("a").children("img").trigger ("click");
   });




   // $(".crud-tree li div").click(function (event) {
   //    var href = $(this).children("span.selector").children("input").click(function(event) {
   //       event.stopPropagation();
   //       alert("foo");
   //    });
   //    window.location.href = window.location.origin + href;
   // });
}