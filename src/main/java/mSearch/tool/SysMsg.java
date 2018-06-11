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

    public static ObservableList<String> textProgramm = FXCollections.observableArrayList();

    public static boolean playerMeldungenAus = false;

    private static final int MAX_LAENGE_1 = 50000;
    private static final int MAX_LAENGE_2 = 30000;
    private static int zeilenNrProgramm = 0;

    @Deprecated
    public static synchronized void playerMsg(String text) {
        if (!playerMeldungenAus) {
            playermeldung(new String[]{text});
        }
    }

    private static void playermeldung(String[] texte) {
        final String z = "  >>";
        System.out.println(z + " " + texte[0]);
        notify(texte[0]);
        for (int i = 1; i < texte.length; ++i) {
            System.out.println(z + " " + texte[i]);
            notify(texte[i]);
        }
    }

    public static void clearText() {
        zeilenNrProgramm = 0;
        textProgramm.clear();
    }

    private static void notify(String zeile) {
        addText(textProgramm, "[" + getNr(zeilenNrProgramm++) + "]   " + zeile);
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

    public synchronized static String getText() {
        return String.join("", textProgramm);
    }
}
