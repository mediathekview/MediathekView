/*
 * Copyright (c) 2025-2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.tabs.tab_film

import mediathek.gui.actions.DeleteBookmarksAction
import mediathek.gui.actions.ManageBookmarkAction
import mediathek.gui.actions.PlayFilmAction
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel
import mediathek.swing.IconOnlyButton
import mediathek.swing.IconOnlyToggleButton
import mediathek.tool.ApplicationConfiguration
import javax.swing.Action
import javax.swing.JLabel
import javax.swing.JToolBar

class FilmToolBar(
    filterModel: FilterSelectionComboBoxModel,
    bookmarkAddFilmAction: GuiFilme.BookmarkAddFilmAction,
    bookmarkRemoveFilmAction: GuiFilme.BookmarkRemoveFilmAction,
    bookmarkClearListAction: DeleteBookmarksAction,
    manageBookmarkAction: ManageBookmarkAction,
    playFilmAction: PlayFilmAction,
    saveFilmAction: GuiFilme.SaveFilmAction,
    private val searchField: GuiFilme.SearchField,
    private val toggleFilterDialogVisibilityAction: GuiFilme.ToggleFilterDialogVisibilityAction
) : JToolBar() {
    private val lblSearch = JLabel("Suche:")
    private val filterSelectionComboBox = FilterSelectionComboBox(filterModel)
    val toggleFilterDialogVisibilityButton = FilterVisibilityToggleButton(toggleFilterDialogVisibilityAction)

    init {
        add(IconOnlyButton(playFilmAction))
        add(IconOnlyButton(saveFilmAction))
        addSeparator()

        add(filterSelectionComboBox)
        addSeparator()

        add(lblSearch)
        add(searchField)
        addSeparator()

        add(toggleFilterDialogVisibilityButton)

        addSeparator()
        add(IconOnlyButton(bookmarkAddFilmAction))
        add(IconOnlyButton(bookmarkRemoveFilmAction))
        addSeparator()
        add(IconOnlyButton(bookmarkClearListAction))
        addSeparator()
        add(IconOnlyButton(manageBookmarkAction))
    }

    override fun setEnabled(enabled: Boolean) {
        super.setEnabled(enabled)
        lblSearch.isEnabled = enabled
        searchField.isEnabled = enabled
        filterSelectionComboBox.isEnabled = enabled
        toggleFilterDialogVisibilityAction.isEnabled = enabled
    }

    class FilterVisibilityToggleButton(action: Action) : IconOnlyToggleButton(action) {
        init {
            isSelected = ApplicationConfiguration.getConfiguration()
                .getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false)
        }
    }
}
