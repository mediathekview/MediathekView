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

import javax.swing.SwingUtilities;
import mediathek.MediathekGui;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.tool.DatumZeit;
import mediathek.tool.Konstanten;
import mediathek.tool.Log;

public class CheckUpdate {
    
    private int sekunden = 5;
    private String titelOrg = "";
    private DDaten ddaten;
    private MediathekGui gui;
    
    public CheckUpdate(MediathekGui gg, DDaten dd) {
        gui = gg;
        ddaten = dd;
        titelOrg = ddaten.mediathekGui.getTitle();
    }
    
    public void suchen() {
        gui.setTitle("");
        new Thread(new Pruefen()).start();
        
    }
    
    private class Pruefen implements Runnable {
        
        @Override
        public synchronized void run() {
            try {
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_UPDATE_SUCHEN_NR])) {
                    if (!Daten.system[Konstanten.SYSTEM_UPDATE_DATUM_NR].equals(DatumZeit.getHeute_yyyyMMdd())) {
                        final ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();

                        //wir sind nicht auf dem EDT, erzeuegen aber Swing Dialoge...ergo aufpassen!
                        SwingUtilities.invokeLater(new Runnable() {
                            @Override
                            public void run() {
                                if (pgrUpdate.checkVersion(ddaten, false /* bei aktuell anzeigen */, true /* Hinweis */, false /* hinweiseAlleAnzeigen */)) {
                                    gui.setTitle("Neue Version verfügbar");
                                } else {
                                    gui.setTitle("Alles aktuell");
                                }
                                for (int i = sekunden; i > 0; --i) {
                                    gui.setTitle(" * " + gui.getTitle() + " * ");
                                    try {
                                        this.wait(1000);
                                    } catch (InterruptedException ignored) {
                                        gui.setTitle(titelOrg);
                                    }
                                }
                            }
                        });
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(794612801, Log.FEHLER_ART_PROG, CheckUpdate.class.getName(), ex);
            }
        }
    }
}
