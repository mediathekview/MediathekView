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
package mediathek.controller;

import mediathek.daten.Daten;
import mediathek.daten.ListePsetVorlagen;
import mediathek.tool.DatumZeit;
import mediathek.tool.Funktionen;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;

public class CheckUpdate {

    private Daten daten;

    public CheckUpdate(Daten dd) {
        daten = dd;
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
                        final ProgrammUpdateSuchen pgrUpdate = new ProgrammUpdateSuchen();
                        if (pgrUpdate.checkVersion(daten, false /* bei aktuell anzeigen */, true /* Hinweis */, false /* hinweiseAlleAnzeigen */)) {
                            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, CheckUpdate.class.getSimpleName());
                        } else {
                            ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, CheckUpdate.class.getSimpleName());
                        }
                        ListePsetVorlagen.getNeuVersionStandarset(daten, Funktionen.getOsString());
                        try {
                            this.wait(10 * 1000); // 10 Sekunden den Titel anzeigen
                        } catch (Exception ignored) {
                        }
                        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, CheckUpdate.class.getSimpleName());
                    }
                }
            } catch (Exception ex) {
                Log.fehlerMeldung(794612801, Log.FEHLER_ART_PROG, CheckUpdate.class.getName(), ex);
            }
        }
    }
}
