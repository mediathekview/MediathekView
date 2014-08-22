/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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
package mediathek.tool;

import java.util.EventListener;
import javax.swing.SwingUtilities;
import javax.swing.event.EventListenerList;
import mediathek.controller.Log;

public class ListenerMediathekView implements EventListener {

    public static final int EREIGNIS_BLACKLIST_GEAENDERT = 1;
    public static final int EREIGNIS_LISTE_HISTORY_GEAENDERT = 2;
    public static final int EREIGNIS_LISTE_PSET = 3;
    public static final int EREIGNIS_FILMLISTE_GEAENDERT = 5;
    public static final int EREIGNIS_ANZAHL_DOWNLOADS = 6;
    public static final int EREIGNIS_LISTE_URL_FILMLISTEN = 7;
    public static final int EREIGNIS_LISTE_FILMLISTEN_SERVER = 8;
    public static final int EREIGNIS_LISTE_DOWNLOADS = 9;
    public static final int EREIGNIS_LISTE_ABOS = 10;
    public static final int EREIGNIS_LISTE_ERLEDIGTE_ABOS = 11;
    public static final int EREIGNIS_ART_IMPORT_FILMLISTE = 12;
    public static final int EREIGNIS_ART_DOWNLOAD_PROZENT = 13;
    public static final int EREIGNIS_START_EVENT = 14;
    public static final int EREIGNIS_START_EVENT_BUTTON = 15;
    public static final int EREIGNIS_LOG_FEHLER = 16;
    public static final int EREIGNIS_LOG_SYSTEM = 17;
    public static final int EREIGNIS_LOG_PLAYER = 18;
    public static final int EREIGNIS_PROGRAMM_OEFFNEN = 19;
    public static final int EREIGNIS_MEDIATHEKGUI_ORG_TITEL = 20;
    public static final int EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL = 21;
    public static final int EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR = 22;
    public static final int EREIGNIS_PANEL_FILTER_ANZEIGEN = 23;
    public static final int EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN = 25;
    public static final int EREIGNIS_SUCHFELD_FOCUS_SETZEN = 26;
    public static final int EREIGNIS_BLACKLIST_AUCH_FUER_ABOS = 27;
    public static final int EREIGNIS_BANDBREITE = 28;
    public static final int EREIGNIS_REIHENFOLGE_DOWNLOAD = 29;
    public static final int EREIGNIS_TIMER = 30;
    public static final int EREIGNIS_GEO = 31;
    public static final int EREIGNIS_BESCHREIBUNG = 32;
    public static final int EREIGNIS_RESET_INTERRUPT = 33;
    public static final int EREIGNIS_FILTER_ANZAHL = 34;
    //public static final int EREIGNIS_FILTER_AKT = 35;
    public static final int EREIGNIS_FONT = 36;
    public int[] mvEreignis = {-1};
    public String klasse = "";
    private static EventListenerList listeners = new EventListenerList();

    public ListenerMediathekView(int eereignis, String kklasse) {
        mvEreignis = new int[]{eereignis};
        klasse = kklasse;
    }

    public ListenerMediathekView(int[] eereignis, String kklasse) {
        mvEreignis = eereignis;
        klasse = kklasse;
    }

    public void ping() {
    }

    public static synchronized void addListener(ListenerMediathekView listener) {
        listeners.add(ListenerMediathekView.class, listener);
    }

    public static synchronized void notify(int ereignis, String klasse) {
        for (ListenerMediathekView l : listeners.getListeners(ListenerMediathekView.class)) {
            for (int er : l.mvEreignis) {
                if (er == ereignis) {
                    if (!l.klasse.equals(klasse)) {
                        // um einen Kreislauf zu verhindern
                        try {
                            l.pingen();
                        } catch (Exception ex) {
                            Log.fehlerMeldung(562314008, "ListenerMediathekView.notifyMediathekListener", ex);
                        }
                    }
                }
            }
        }
    }

    private void pingen() {
        try {
            if (SwingUtilities.isEventDispatchThread()) {
                ping();
            } else {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        ping();
                    }
                });
            }
        } catch (Exception ex) {
            Log.fehlerMeldung(698989743,  "ListenerMediathekView.pingen", ex);
        }
    }
}
