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
package mediathek.tool;

import java.util.Iterator;
import java.util.LinkedList;

public final class MVReplaceList {

    public final static String REPLACELIST = "Ersetzungstabelle";
    public final static String VON = "von";
    public final static String NACH = "nach";
    public final static String[] COLUMN_NAMES = {VON, NACH};
    public static final int MAX_ELEM = 2;

    public static LinkedList<String[]> liste = new LinkedList<>();

    public void init() {
        // vor dem ersetzen wenn leer:
        liste.add(new String[]{" ", "_"});
        liste.add(new String[]{"\n", "_"});
        liste.add(new String[]{"\"", "_"});
        liste.add(new String[]{"*", "_"});
        liste.add(new String[]{"?", "_"});
        liste.add(new String[]{"<", "_"});
        liste.add(new String[]{">", "_"});
        liste.add(new String[]{":", "_"});
        liste.add(new String[]{"'", "_"});
        liste.add(new String[]{"|", "_"});
    }

    public String replace(String str) {
        if (liste.isEmpty()) {
            init();
        }
        Iterator<String[]> it = liste.iterator();
        while (it.hasNext()) {
            String[] sa = it.next();
            str = str.replace(sa[0], sa[1]);
        }
        return str;
    }

//    public void save() {
//        String ret = "";
//        try {
//            Iterator<String[]> it = liste.iterator();
//            while (it.hasNext()) {
//                String[] s = it.next();
//                ret += s[0] + s[1];
//            }
//            Daten.mVConfig.add(REPLACELIST, ret);
//        } catch (Exception ex) {
//            Log.fehlerMeldung(784512096, Log.FEHLER_ART_PROG, "MVReplaceList.save", ex);
//            Daten.mVConfig.add(REPLACELIST, "");
//        }
//    }
//
//    public void load() {
//        if (!Daten.mVConfig.get(REPLACELIST).isEmpty()) {
//            try {
//                String s = Daten.mVConfig.get(REPLACELIST);
//                for (int i = 0; i < s.length(); ++i) {
//                    String[] sa = new String[2];
//                    sa[0] = s.substring(i, i + 1);
//                    ++i;
//                    sa[1] = s.substring(i, i + 1);
//                    liste.add(sa);
//                }
//            } catch (Exception ex) {
//                Log.fehlerMeldung(915263607, Log.FEHLER_ART_PROG, "MVReplaceList.load", ex);
//                init();
//            }
//        } else {
//            init();
//        }
//    }
}
