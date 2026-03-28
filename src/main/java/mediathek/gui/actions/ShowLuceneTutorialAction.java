/*
 * Copyright (c) 2026 derreisende77.
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

import mediathek.gui.dialog.lucene_tutorial.LuceneTutorialDialog;
import mediathek.tool.SVGIconUtilities;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;

public final class ShowLuceneTutorialAction extends AbstractAction {
    private final Window owner;

    public ShowLuceneTutorialAction(@NotNull Window owner) {
        this.owner = owner;
        putValue(NAME, "Lucene-Suchsyntax anzeigen...");
        putValue(SHORT_DESCRIPTION, "Lucene Query Syntax Hilfe");
        putValue(SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/circle-question.svg"));
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var dialog = new LuceneTutorialDialog(owner);
        dialog.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosed(WindowEvent e) {
                setEnabled(true);
            }
        });
        setEnabled(false);
        dialog.setVisible(true);
    }
}
