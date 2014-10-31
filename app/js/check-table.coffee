$(document).ready ->
    # hide checkboxes if JS active
    $("#tasks tr").each -> $(this).children().last().hide()
    rows = $("#tasks tbody tr")
    # row highlighting
    rows.each ->
        $(this).click ->
            r = $(this)
            i = r.find "input"
            if i.prop "checked"
                i.prop "checked", false
                r.removeClass "selected"
            else
                i.prop "checked", true
                r.addClass "selected"
