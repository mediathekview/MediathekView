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

import java.text.SimpleDateFormat;
import java.util.Date;

public class Main {
    /*
     * Aufruf:
     * java -jar Mediathek [Pfad zur Konfigdatei, sonst homeverzeichnis] [Schalter]
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
     * Displays the total amount of memory, the maximal amount of memory
     * and the total amount of free memory in the Java Virtual Machine.
     */
    public static void displayStartInfos() {
        try {
            //Version
            Log.systemMeldung(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
            //Compiledate
            Date d = new Date(Main.class.getResource("Main.class").openConnection().getLastModified());
            Log.systemMeldung("compiled: " + new SimpleDateFormat("dd.MM.yyyy, HH:mm").format(d));
            //Display the total amount of memory in the Java virtual machine.
            long totalMem = Runtime.getRuntime().totalMemory();
            Log.systemMeldung("totalMemory: " + totalMem / (1024L * 1024L) + " MB");
            //Display the maximum amount of memory that the Java virtual machine will attempt to use.
            long maxMem = Runtime.getRuntime().maxMemory();
            Log.systemMeldung("maxMemory: " + maxMem / (1024L * 1024L) + " MB");
            //Display the amount of free memory in the Java Virtual Machine.
            long freeMem = Runtime.getRuntime().freeMemory();
            Log.systemMeldung("freeMemory: " + freeMem / (1024L * 1024L) + " MB");
        } catch (Exception ex) {
        }
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
                Main.displayStartInfos();
                if (ar != null) {
                    for (int i = 0; i < ar.length; ++i) {
                        if (ar[i].equals("-A")) {
                            ////state = AUTO;
                        }
                        if (ar[i].equalsIgnoreCase("-noGui")) {
                            state = NOGUI;
                        }
                        if (ar[i].equalsIgnoreCase("-v")) {
                            System.exit(0);
                        }
                    }
                }
                switch (state) {
                    case NORMAL:
                        new MediathekGui(ar).setVisible(true);
                        break;
                    case AUTO:
                        new MediathekAuto(ar).setVisible(true);
                        break;
                    case NOGUI:
                        new MediathekNoGui(ar).starten();
                        break;
                }
            }
        });
    }
}
