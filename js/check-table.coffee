# © 2014 OpenLab Augsburg e.V. and contributors (see CONTRIBUTORS).
# 
# This file is part of Papstehrenwort.
# 
# Papstehrenwort is free software: you can redistribute it and/or modify
# it under the terms of the GNU Affero General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# Papstehrenwort is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Affero General Public License for more details.
# 
# You should have received a copy of the GNU Affero General Public License
# along with Papstehrenwort.  If not, see <http://www.gnu.org/licenses/>.

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
