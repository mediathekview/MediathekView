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

package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.config.Konstanten;
import mediathek.gui.messages.BookmarkDeleteRepaintEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.MessageBus;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class DeleteBookmarksAction extends AbstractAction {
    private final JFrame owner;

    public DeleteBookmarksAction(MediathekGui parent) {
        super();
        owner = parent;

        putValue(Action.SHORT_DESCRIPTION, "Merkliste vollständig löschen");
        putValue(NAME, "Merkliste vollständig löschen...");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createToolBarIcon("icons/fontawesome/file-circle-xmark.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var res = JOptionPane.showConfirmDialog(owner,
                "Möchten Sie wirklich die Merkliste vollständig löschen?", Konstanten.PROGRAMMNAME, JOptionPane.YES_NO_OPTION);
        if (res == JOptionPane.YES_OPTION) {
            var bookmarkList = Daten.getInstance().getListeBookmarkList();
            bookmarkList.clear();
            bookmarkList.saveToFile();
            JOptionPane.showMessageDialog(owner, "Die Merkliste wurde gelöscht", Konstanten.PROGRAMMNAME, JOptionPane.INFORMATION_MESSAGE);
            MessageBus.getMessageBus().publishAsync(new BookmarkDeleteRepaintEvent());
        }

    }
}
