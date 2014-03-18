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

import java.io.File;
import java.util.Iterator;
import java.util.LinkedList;

public final class MVReplaceList {

    public final static String REPLACELIST = "Ersetzungstabelle";
    public final static String VON = "von";
    public final static int VON_NR = 0;
    public final static String NACH = "nach";
    public final static int NACH_NR = 1;
    public final static String[] COLUMN_NAMES = {VON, NACH};
    public static final int MAX_ELEM = 2;

    public static LinkedList<String[]> liste = new LinkedList<>();

    public void init() {
        if (liste.isEmpty()) {
            // wenn leer:
            liste.add(new String[]{" ", "_"});
            liste.add(new String[]{"\"", "_"});
            liste.add(new String[]{"*", "_"});
            liste.add(new String[]{"?", "_"});
            liste.add(new String[]{"<", "_"});
            liste.add(new String[]{">", "_"});
            liste.add(new String[]{":", "_"});
            liste.add(new String[]{"'", "_"});
            liste.add(new String[]{"|", "_"});
        }
    }

    public String replace(String str, boolean pfad) {
        if (liste.isEmpty()) {
            init();
        }
        Iterator<String[]> it = liste.iterator();
        while (it.hasNext()) {
            String[] sa = it.next();
            if (pfad && sa[0].equals(File.separator)) {
                // dann kann er nicht entfernt werden :(
                continue;
            }
            str = str.replace(sa[0], sa[1]);
        }
        return str;
    }

    public boolean check() {
        for (int i = 0; i < liste.size(); ++i) {
            String[] is = liste.get(i);
            for (int k = i+1; k < liste.size(); ++k) {
                String[] ks = liste.get(k);
                if (is[1].contains(ks[0])) {
                    return true;
                }
            }
        }
        return false;
    }

    public int auf(int idx, boolean auf) {
        String[] replace = liste.remove(idx);
        int neu = idx;
        if (auf) {
            if (neu > 0) {
                --neu;
            }
        } else {
            if (neu < liste.size()) {
                ++neu;
            }
        }
        liste.add(neu, replace);
        return neu;
    }

}
