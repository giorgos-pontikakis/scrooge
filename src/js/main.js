$(document).ready(function () {
  applyAutocomplete();
  applyDatepicker();
  selectableRows();
  autoSelectAccount();
  settingsUI();
  var selectedTreeInput = $(".company-dependent input[type=radio][checked]");
  maybeHideProject(selectedTreeInput);
});

function settingsUI () {
  // Prevent double submit: Disable submit buttons when clicked once
  $('form').submit(function(){
    $('[type="submit"]', this).attr("disabled", "disabled");
  });
  // Disable spellchecking and autocomplete
  $('textarea').attr('spellcheck',false);
  $('form').attr('autocomplete','off');
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
  for (var i = 0; i < autocompleteIDs.length; i++) {
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

  var acAccountIDs = [["revenues", "revenues-root-account"],
                      ["expenses", "expenses-root-account"]];
  for (var i = 0; i < acAccountIDs.length; i++) {
    id = acAccountIDs[i][0];
    root = acAccountIDs[i][1];
    options.source = "/scrooge/autocomplete/accounts?root=" + root;
    $(".ac-" + id).autocomplete(options);
  }
  var isCustomer = (window.location.pathname.match(/customer/i) !== null);
  var acTemtxPath = "/scrooge/autocomplete/temtx?customer-p="
  $(".ac-temtx").autocomplete({source: acTemtxPath + isCustomer});
  $(".ac-temtx-chq").autocomplete({source: acTemtxPath + isCustomer + "&force-chequing-p=true"});
}


function selectableRows () {

  // crud-table
  $(".crud-table input").click(function (e) {
    e.stopPropagation();
  });

  $(".crud-table select").click(function (e) {
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
    var companyInp = $(".data-form input.ac-company");
    e.stopPropagation();
    highlightCRUDTreeLeaf(this);
    maybeHideProject(this);
    companyInp.unbind("blur");
  });

}

function autoSelectAccount () {
  var companyInp = $(".data-form input.ac-company");
  companyInp.bind("autocompleteclose", clickAccount);
  companyInp.bind("blur", clickAccount);

}

function loadDropdown () {
  var url = "/scrooge/autocomplete/project?company-title=" + $('.data-form .company').val();
  $.ajax({url: url,
          datatype: "html",
          success: function (data) {
            $("#project-area").show();
            $('#project-picker').html(data);
            $('#project-picker').show();
          },
          error: function () {
            $('#project-area').hide();
          }
         });
}

function clickAccount () {
  var title = $(this).val();
  var url = "/scrooge/company/accounts?title=" + encodeURIComponent(title);
  $.getJSON(url, function (data) {
    if (data.immediateTxOnly) {
      // only accepting cash; hide accounts payable/receivable
      $(".hidden-when-immediate-tx-only").hide();
    } else {
      // accepts debits/credits; show accounts payable/receivable
      $(".hidden-when-immediate-tx-only").show();
    }
    var isRevenue = $(".company-dependent input[value=" + data.accountIDs[0] + "]").length === 1;
    var accountID = isRevenue ? data.accountIDs[1][0] : data.accountIDs[1][1];
    var jq = ".company-dependent input[type=radio][value=" + accountID +  "]";
    $(jq).attr('checked', true);
    highlightCRUDTreeLeaf(jq);
    maybeHideProject(jq);
  });
}

function highlightCRUDTreeLeaf (item) {
  $(".crud-tree li.selected").removeClass("selected");
  $(item).parent().parent().parent().addClass("selected");
}

function maybeHideProject (item) {
  var accID = parseInt($(item).attr('value'));
  var projectArea = $("#project-area");
  if (accID === accounts.project) {
    loadDropdown();
  } else {
    projectArea.hide();
  }
}
