/*
 * Copyright (c) 2025 derreisende77.
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

package mediathek.gui.tabs.tab_film;

import mediathek.gui.actions.ManageBookmarkAction;
import mediathek.gui.actions.PlayFilmAction;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox;
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel;
import mediathek.tool.ApplicationConfiguration;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;

public class FilmToolBar extends JToolBar {
    private final JLabel lblSearch = new JLabel("Suche:");
    private final FilterSelectionComboBox filterSelectionComboBox;
    private final GuiFilme.SearchField searchField;
    private final GuiFilme.ToggleFilterDialogVisibilityAction toggleFilterDialogVisibilityAction;
    private final FilterVisibilityToggleButton btnToggleFilterDialogVisibility;

    public FilmToolBar(@NotNull FilterSelectionComboBoxModel filterModel,
                       @NotNull GuiFilme.BookmarkAddFilmAction bookmarkAddFilmAction,
                       @NotNull GuiFilme.BookmarkRemoveFilmAction bookmarkRemoveFilmAction,
                       @NotNull GuiFilme.BookmarkClearListAction bookmarkClearListAction,
                       @NotNull ManageBookmarkAction manageBookmarkAction,
                       @NotNull PlayFilmAction playFilmAction,
                       @NotNull GuiFilme.SaveFilmAction saveFilmAction,
                       @NotNull GuiFilme.SearchField searchField,
                       @NotNull GuiFilme.ToggleFilterDialogVisibilityAction toggleFilterDialogVisibilityAction) {
        this.searchField = searchField;
        this.toggleFilterDialogVisibilityAction = toggleFilterDialogVisibilityAction;
        this.btnToggleFilterDialogVisibility = new FilterVisibilityToggleButton(toggleFilterDialogVisibilityAction);

        add(playFilmAction);
        add(saveFilmAction);
        addSeparator();

        filterSelectionComboBox = new FilterSelectionComboBox(filterModel);
        add(filterSelectionComboBox);
        addSeparator();

        add(lblSearch);
        add(searchField);
        addSeparator();

        add(btnToggleFilterDialogVisibility);

        addSeparator();
        add(bookmarkAddFilmAction);
        add(bookmarkRemoveFilmAction);
        addSeparator();
        add(bookmarkClearListAction);
        addSeparator();
        add(manageBookmarkAction);
    }

    public FilterVisibilityToggleButton getToggleFilterDialogVisibilityButton() {
        return btnToggleFilterDialogVisibility;
    }

    @Override
    public void setEnabled(boolean enabled) {
        super.setEnabled(enabled);
        lblSearch.setEnabled(enabled);
        searchField.setEnabled(enabled);
        filterSelectionComboBox.setEnabled(enabled);
        toggleFilterDialogVisibilityAction.setEnabled(enabled);
    }

    public static class FilterVisibilityToggleButton extends JToggleButton {
        public FilterVisibilityToggleButton(Action a) {
            super(a);
            setText("");
            final boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
            setSelected(visible);
        }
    }
}
