(function() {
  $(document).ready(function() {
    var rows;
    // hide checkboxes if JS active
    $("#tasks tr").each(function() {
      return $(this).children().last().hide();
    });
    rows = $("#tasks tbody tr");
    // row highlighting
    return rows.each(function() {
      return $(this).click(function() {
        var i, r;
        r = $(this);
        i = r.find("input");
        if (i.prop("checked")) {
          i.prop("checked", false);
          return r.removeClass("selected");
        } else {
          i.prop("checked", true);
          return r.addClass("selected");
        }
      });
    });
  });

}).call(this);
