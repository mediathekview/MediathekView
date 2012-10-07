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
package mediathek;

import mediathek.daten.Daten;
import mediathek.tool.Log;

public class Main {

    public static final String STARTP_ALLES = "-alles";
    public static final String STARTP_USER_AGENT = "-agent";
    public static final String STARTP_EXPORT_DATEI = "-o";
    public static final String STARTP_IMPORT_URL = "-i";

    /*
     * Aufruf:
     * java -jar Mediathek [Pfad zur Konfigdatei, sonst homeverzeichnis] [Schalter]
     *
     * Programmschalter:
     *
     * -D Debugmode
     * -M Fenster maximiert starten
     * -A Automodus
     * -noGui ohne GUI starten und die Filmliste laden
     *
     * */
    public Main() {
    }

    /**
     * @param args the command line arguments
     */
    public static void main(String args[]) {
        final String ar[] = args;
        java.awt.EventQueue.invokeLater(new Runnable() {
            final int NORMAL = 0;
            final int AUTO = 1;
            final int NOGUI = 2;
            int state = NORMAL;

            @Override
            public void run() {

                if (ar != null) {
                    for (String s : ar) {
                        if (s.equalsIgnoreCase("-auto")) {
                            state = AUTO;
                        }
                        if (s.equalsIgnoreCase("-noGui")) {
                            state = NOGUI;
                        }
                        if (s.equalsIgnoreCase("-d")) {
                            Daten.debug = true;
                        }
                        if (s.equalsIgnoreCase("-v")) {
                            Log.versionsMeldungen(this.getClass().getName());
                            System.exit(0);
                        }
                    }
                }
                switch (state) {
                    case AUTO:
                        new MediathekAuto(ar).starten();
                        break;
                    case NOGUI:
                        new MediathekNoGui(ar).starten();
                        break;
                    default:
                        new MediathekGui(ar).setVisible(true);
                        break;
                }
            }
        });
    }
}
