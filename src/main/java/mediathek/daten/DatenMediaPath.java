/*    
 *    MediathekView
 *    Copyright (C) 2008   W. Xaver
 *    W.Xaver[at]googlemail.com
 *    http://zdfmediathk.sourceforge.net/
 *    
 *    This program is free software: you can redistribute it and/or modify
 *    it under the terms of the GNU General Public License as published by
 *    the Free Software Foundation, either version 3 of the License, or
 *    any later version.
 *
 *    This program is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU General Public License for more details.
 *
 *    You should have received a copy of the GNU General Public License
 *    along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.daten;

import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

public class DatenMediaPath implements Comparable<DatenMediaPath> {

    public final static int MEDIA_PATH_PATH = 0;
    public final static int MEDIA_PATHE_SAVE = 1;

    public final static String[] COLUMN_NAMES = {"Pfad", "Speichern"};
    public final static String[] XML_NAMES = COLUMN_NAMES;
    public static final String TAG = "MediaPath";
    public final static int MAX_ELEM = 2;
    public String[] arr;

    public DatenMediaPath(String pfad, boolean sichern) {
        initialize();

        arr[MEDIA_PATH_PATH] = pfad;
        arr[MEDIA_PATHE_SAVE] = Boolean.toString(sichern);
    }

    public DatenMediaPath() {
        initialize();
    }

    public boolean savePath() {
        return Boolean.parseBoolean(arr[MEDIA_PATHE_SAVE]);
    }

    private void initialize() {
        arr = new String[MAX_ELEM];
        Arrays.fill(arr,"");
    }

    @Override
    public int compareTo(@NotNull DatenMediaPath o) {
        return 0;
    }
}
