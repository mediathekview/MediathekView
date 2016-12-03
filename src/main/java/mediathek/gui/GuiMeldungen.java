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

import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;

import javax.swing.*;
import java.awt.*;
import java.beans.PropertyChangeEvent;

@SuppressWarnings("serial")
public class GuiMeldungen extends PanelVorlage {
    private JSplitPane splitPane = null;

    public GuiMeldungen(Daten d, JFrame parentComponent) {
        super(d, parentComponent);

        PanelMeldungen panelMeldungenSystem = new PanelMeldungen(daten, daten.getMediathekGui(), SysMsg.LOG_SYSTEM, "Systemmeldungen");
        PanelMeldungen panelMeldungenPlayer = new PanelMeldungen(daten, daten.getMediathekGui(), SysMsg.LOG_PLAYER, "Meldungen Hilfsprogramme");
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                panelMeldungenSystem,
                panelMeldungenPlayer);

        setLayout(new BorderLayout());
        add(splitPane, BorderLayout.CENTER);
        try {
            int divider = Integer.parseInt(MVConfig.get(MVConfig.Configs.SYSTEM_BREITE_MELDUNGEN));
            if (divider > 0) {
                splitPane.setDividerLocation(divider);
            }
        } catch (NumberFormatException ignored) {
        }
        splitPane.addPropertyChangeListener(JSplitPane.DIVIDER_LOCATION_PROPERTY, (PropertyChangeEvent pce) ->
                MVConfig.add(MVConfig.Configs.SYSTEM_BREITE_MELDUNGEN, String.valueOf(splitPane.getDividerLocation())));
    }

    @Override
    public void isShown() {
        super.isShown();
        if (!solo) {
//            daten.getMediathekGui().setTabShown(MediathekGui.TABS.TAB_NIX);
            daten.getMediathekGui().getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.NONE);

        }
    }

}
