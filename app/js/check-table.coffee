$(document).ready ->
    rows = $("#tasks tbody tr")
    rows.each ->
        $(this).click ->
            r = $(this)
            i = r.find "input"
            if i.prop "checked"
                i.prop "checked", false
            else
                i.prop "checked", true

