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
package mSearch.tool;

import javafx.collections.FXCollections;
import javafx.collections.ObservableList;

public class SysMsg {

    public static ObservableList<String> textSystem = FXCollections.observableArrayList();
    public static ObservableList<String> textProgramm = FXCollections.observableArrayList();

    public static boolean playerMeldungenAus = false;
    public static final int LOG_SYSTEM = 1;
    public static final int LOG_PLAYER = 2;

    private static final int MAX_LAENGE_1 = 50000;
    private static final int MAX_LAENGE_2 = 30000;
    private static int zeilenNrSystem = 0;
    private static int zeilenNrProgramm = 0;

    @Deprecated
    public static synchronized void sysMsg(String[] text) {
        systemmeldung(text);
    }

    @Deprecated
    public static synchronized void sysMsg(String text) {
        systemmeldung(new String[]{text});
    }

    @Deprecated
    public static synchronized void playerMsg(String text) {
        if (!playerMeldungenAus) {
            playermeldung(new String[]{text});
        }
    }

    private static void systemmeldung(String[] texte) {
        final String z = ". ";
        if (texte.length <= 1) {
            System.out.println(z + " " + texte[0]);
            notify(LOG_SYSTEM, texte[0]);
        } else {
            String zeile = "----------------------------------------                    ";
            String txt;
            System.out.println(z + zeile);
            notify(LOG_SYSTEM, zeile);
            for (int i = 0; i < texte.length; ++i) {
                txt = "| " + texte[i];
                System.out.println(z + txt);
                if (i == 0) {
                    notify(LOG_SYSTEM, texte[i]);
                } else {
                    notify(LOG_SYSTEM, "    " + texte[i]);
                }
            }
            notify(LOG_SYSTEM, " ");
            System.out.println(z + zeile);
        }
    }

    private static void playermeldung(String[] texte) {
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notify(LOG_PLAYER, texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notify(LOG_PLAYER, texte[i]);
        }
    }

    public static void clearText(int art) {
        switch (art) {
            case LOG_SYSTEM:
                zeilenNrSystem = 0;
                textSystem.clear();
                break;
            case LOG_PLAYER:
                zeilenNrProgramm = 0;
                textProgramm.clear();
                break;
            default:
                break;
        }
    }

    private static void notify(final int art, String zeile) {
        switch (art) {
            case LOG_SYSTEM:
                addText(textSystem, "[" + getNr(zeilenNrSystem++) + "]   " + zeile);
                break;
            case LOG_PLAYER:
                addText(textProgramm, "[" + getNr(zeilenNrProgramm++) + "]   " + zeile);
                break;
            default:
                break;
        }
        // Listener.notify(art, SysMsg.class.getSimpleName()); gibt sonst einen deadlock
    }

    private static String getNr(int nr) {
        final int MAX_STELLEN = 5;
        final String FUELL_ZEICHEN = "0";
        String str = String.valueOf(nr);
        while (str.length() < MAX_STELLEN) {
            str = FUELL_ZEICHEN + str;
        }
        return str;
    }

    private synchronized static void addText(ObservableList<String> text, String texte) {
        if (text.size() > MAX_LAENGE_1) {
            text.remove(0, MAX_LAENGE_2);
        }
        text.add(texte + System.getProperty("line.separator"));
    }

    public synchronized static String getText(int logArt) {
        // wegen synchronized hier
        if (logArt == SysMsg.LOG_SYSTEM) {
            return String.join("", textSystem);
        } else {
            return String.join("", textProgramm);
        }
    }

}
