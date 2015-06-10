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

import mediathek.tool.MVMediaDBFileSize;

public class DatenMediaDB {

    public final static String MEDIA_DB_NAME = "Name";
    public final static int MEDIA_DB_NAME_NR = 0;
    public final static String MEDIA_DB_PATH = "Pfad";
    public final static int MEDIA_DB_PATH_NR = 1;
    public final static String MEDIA_DB_SIZE = "Größe [MB]";
    public final static int MEDIA_DB_SIZE_NR = 2;

    public final static int MAX_ELEM = 3;
    public final static String[] COLUMN_NAMES = {MEDIA_DB_NAME, MEDIA_DB_PATH, MEDIA_DB_SIZE};

    public String[] arr;
    public MVMediaDBFileSize mVMediaDBFileSize;

    public DatenMediaDB(String name, String pfad, long size) {
        makeArr();
        arr[MEDIA_DB_NAME_NR] = name;
        arr[MEDIA_DB_PATH_NR] = pfad;
        //arr[MEDIA_DB_SIZE_NR] = getGroesse(size);
        mVMediaDBFileSize = new MVMediaDBFileSize(size);
    }

    @Override
    public String toString() {
        String ret = "";
        for (int i = 0; i < MAX_ELEM; ++i) {
            if (i == 0) {
                ret += "| ***|" + COLUMN_NAMES[i] + ": " + arr[i] + Daten.LINE_SEPARATOR;
            } else {
                ret += "|    |" + COLUMN_NAMES[i] + ": " + arr[i] + Daten.LINE_SEPARATOR;
            }
        }
        return ret;
    }

    public Object[] getRow() {
        return new Object[]{arr[MEDIA_DB_NAME_NR], arr[MEDIA_DB_PATH_NR], mVMediaDBFileSize};
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
