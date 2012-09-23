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

public class ListenerMediathekView implements EventListener {

    public static final int EREIGNIS_BLACKLIST_ADD = 1;
    public static final int EREIGNIS_BLACKLIST_DEL = 2;
    public static final int EREIGNIS_LISTE_PSET = 3;
    public static final int EREIGNIS_FILMLISTE_NEU = 4;
    public static final int EREIGNIS_ANZAHL_DOWNLOADS = 5;
    public static final int EREIGNIS_LISTE_UPDATESERVER = 6;
    public static final int EREIGNIS_LISTE_DOWNLOADS = 7;
    public static final int EREIGNIS_LISTE_ABOS = 8;
    public static final int EREIGNIS_LISTE_HISTORY = 9;
    public static final int EREIGNIS_LISTE_ERLEDIGTE_ABOS = 10;
    public static final int EREIGNIS_ART_IMPORT_FILMLISTE = 11;
    public static final int EREIGNIS_ART_DOWNLOAD_PROZENT = 12;
    public static final int EREIGNIS_START_EVENT = 13;
    public static final int EREIGNIS_LOG_FEHLER = 14;
    public static final int EREIGNIS_LOG_SYSTEM = 15;
    public static final int EREIGNIS_LOG_PLAYER = 16;
    public int ereignis = -1;
    public String klasse = "";

    public ListenerMediathekView() {
    }

    public ListenerMediathekView(int eereignis, String kklasse) {
        ereignis = eereignis;
        klasse = kklasse;
    }

    public void ping() {
    }

    public void pingGui() {
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
            Log.fehlerMeldung(698989743, "ListenerMediathekView.pingA", ex);
        }
    }

    void dispatchOnSwingThread(Runnable r) {
        if (SwingUtilities.isEventDispatchThread()) {
            r.run();
        } else {
            try {
                SwingUtilities.invokeLater(r);
            } catch (Exception e1) {
                e1.printStackTrace();
            }
        }
    }

    public void ping(String from) {
    }
}
