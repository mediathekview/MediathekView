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

    public LinkedList<String[]> list = new LinkedList<>();

    public void init() {
        list.clear();
        list.add(new String[]{" ", "_"});
    }

    public String replace(String strCheck, boolean pfad) {
        Iterator<String[]> it = list.iterator();
        while (it.hasNext()) {
            String[] strReplace = it.next();

            // hat der Nutzer als Suchbegriff "leer" eingegeben, dann weg damit
            if (strReplace[0].isEmpty()) {
                it.remove();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_REPLACELIST_CHANGED, MVReplaceList.class.getSimpleName());
                continue;
            }

            // bei Pfaden darf / oder \ natürlich nicht entfernt werden
            if (pfad && strReplace[0].equals(File.separator)) {
                continue;
            }

            strCheck = strCheck.replace(strReplace[0], strReplace[1]);
        }
        return strCheck;
    }

    public boolean check() {
        for (int i = 0; i < list.size(); ++i) {
            String[] is = list.get(i);
            for (int k = i + 1; k < list.size(); ++k) {
                String[] ks = list.get(k);
                if (is[1].contains(ks[0])) {
                    return true;
                }
            }
        }
        return false;
    }

    public int up(int idx, boolean up) {
        String[] replace = list.remove(idx);
        int neu = idx;
        if (up) {
            if (neu > 0) {
                --neu;
            }
        } else {
            if (neu < list.size()) {
                ++neu;
            }
        }
        list.add(neu, replace);
        return neu;
    }

}
