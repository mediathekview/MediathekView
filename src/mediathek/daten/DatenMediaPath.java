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

public class DatenMediaPath extends MVData<DatenMediaPath> {

    public final static int MEDIA_PATH_PATH = 0;
    public final static int MEDIA_PATHE_SAVE = 1;

    public final static String[] COLUMN_NAMES = {"Pfad", "Speichern"};
    public final static String[] XML_NAMES = COLUMN_NAMES;
    public static final String TAG = "MediaPath";
    public final static int MAX_ELEM = 2;
    public String[] arr;

    public DatenMediaPath(String pfad, boolean sichern) {
        makeArr();
        arr[MEDIA_PATH_PATH] = pfad;
        arr[MEDIA_PATHE_SAVE] = Boolean.toString(sichern);
    }

    public DatenMediaPath() {
        makeArr();
    }

    public boolean toSave() {
        return Boolean.parseBoolean(arr[MEDIA_PATHE_SAVE]);
    }

    //===================================
    // Private
    //===================================
    private void makeArr() {
        arr = new String[MAX_ELEM];
        for (int i = 0; i < arr.length; ++i) {
            arr[i] = "";
        }
    }

}
