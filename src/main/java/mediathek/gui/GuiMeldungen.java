/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;

import javax.swing.*;
import java.awt.*;

@SuppressWarnings("serial")
public class GuiMeldungen extends PanelVorlage {

    public GuiMeldungen(Daten d, JFrame parentComponent) {
        super(d, parentComponent);

        PanelMeldungen panelMeldungenPlayer = new PanelMeldungen(daten, daten.getMediathekGui(), "Meldungen Hilfsprogramme");

        setLayout(new BorderLayout());
        add(panelMeldungenPlayer, BorderLayout.CENTER);
    }

    @Override
    public void isShown() {
        super.isShown();
        if (!solo) {
            daten.getMediathekGui().tabPaneIndexProperty().setValue(MediathekGui.TabPaneIndex.NONE);
        }
    }

}
