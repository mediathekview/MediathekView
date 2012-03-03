/*
 *  MediathekView
 *  Copyright (C) 2008 W. Xaver
 *  W.Xaver[at]googlemail.com
 *  http://zdfmediathk.sourceforge.net/
 *
 *  This program is free software: you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation, either version 3 of the License, or
 *  any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.controller.io;

import mediathek.Daten;
import mediathek.Konstanten;
import mediathek.MediathekGui;
import mediathek.daten.DDaten;
import mediathek.tool.DatumZeit;

public class CheckUpdate {

    private int sekunden = 20;
    private String titelOrg = "";
    private DDaten ddaten;
    private MediathekGui gui;

    public CheckUpdate(MediathekGui gg, DDaten dd) {
        gui = gg;
        ddaten = dd;
        titelOrg = ddaten.mediathekGui.getTitle();
    }

    public void suchen() {
        new Thread(new Pruefen()).start();
    }

    private class Pruefen implements Runnable {

        @Override
        public synchronized void run() {
            try {
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR])) {
                    if (!Daten.system[Konstanten.SYSTEM_UPDATE_DATUM_NR].equals(DatumZeit.getHeute_yyyyMMdd())) {
                        ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();
                        if (pgrUpdate.checkVersion(ddaten, false)) {
                            gui.setTitle("Neue Version verfügbar");
                        } else {
                            gui.setTitle("Alles aktuell");
                        }
                        for (int i = sekunden; i > 0; --i) {
                            if (i % 3 == 0) {
                                gui.setTitle(" * " + gui.getTitle() + " * ");
                            }
                            this.wait(1000);
                        }
                        gui.setTitle(titelOrg);
                    }
                }
            } catch (InterruptedException ex) {
            }
        }
    }
}
