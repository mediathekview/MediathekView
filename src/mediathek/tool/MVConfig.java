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

import java.util.HashMap;

public class MVConfig {

    private final HashMap<String, String> hashmap = new HashMap<>();

    public MVConfig() {

    }

    public synchronized void add(String key, String value) {
        hashmap.put(key, value);
    }

    public synchronized String get(String key) {
        String s = hashmap.get(key);
        return s == null ? "" : s;
    }

    public synchronized String[][] getAll() {
        String[][] ret = new String[hashmap.size()][2];
        String[] setArray = hashmap.keySet().toArray(new String[]{});
        for (int i = 0; i < setArray.length; ++i) {
            ret[i][0] = setArray[i];
            ret[i][1] = hashmap.get(setArray[i]);
        }
        return ret;
    }
}
