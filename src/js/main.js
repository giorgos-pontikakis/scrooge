$(document).ready(function () {
   applyAutocomplete();
   applyDatepicker();
   selectableRows();
   preventDoubleSubmit();
});

function preventDoubleSubmit () {
   $('form').submit(function(){
      $('[type="submit"]', this).attr("disabled", "disabled");
   });
}

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