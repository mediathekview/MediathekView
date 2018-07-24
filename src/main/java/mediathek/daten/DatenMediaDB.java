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

import mediathek.config.Daten;
import mediathek.tool.MVMediaDBFileSize;
import org.apache.commons.lang3.StringUtils;

public class DatenMediaDB extends MVData<DatenMediaDB> {

    public final static int MEDIA_DB_NAME = 0;
    public final static int MEDIA_DB_PATH = 1;
    public final static int MEDIA_DB_SIZE = 2;
    public final static int MEDIA_DB_EXTERN = 3;

    public final static int MAX_ELEM = 4;
    public final static String[] COLUMN_NAMES = {"Name", "Pfad", "Größe [MB]", "Extern"};
    public final static String[] XML_NAMES = {"Name", "Pfad", "Groesse", "Extern"};

    public String[] arr;
    public MVMediaDBFileSize mVMediaDBFileSize;

    public DatenMediaDB(String name, String pfad, long size, boolean extern) {
        makeArr();
        arr[MEDIA_DB_NAME] = putzen(name);
        arr[MEDIA_DB_PATH] = putzen(pfad);
        mVMediaDBFileSize = new MVMediaDBFileSize(size);
        arr[MEDIA_DB_SIZE] = mVMediaDBFileSize.toString();
        arr[MEDIA_DB_EXTERN] = Boolean.toString(extern);
    }

    public Object[] getRow() {
        Object[] ob = new Object[DatenMediaDB.MAX_ELEM];
        for (int i = 0; i < DatenMediaDB.MAX_ELEM; ++i) {
            if (i == DatenMediaDB.MEDIA_DB_SIZE) {
                ob[i] = mVMediaDBFileSize;
            } else {
                ob[i] = arr[i];
            }
        }
        return ob;
    }

    public boolean isExtern() {
        return Boolean.parseBoolean(arr[MEDIA_DB_EXTERN]);
    }

    public boolean equal(DatenMediaDB m) {
        return m.arr[MEDIA_DB_NAME].equals(this.arr[MEDIA_DB_NAME])
                && m.arr[MEDIA_DB_PATH].equals(this.arr[MEDIA_DB_PATH])
                && m.arr[MEDIA_DB_SIZE].equals(this.arr[MEDIA_DB_SIZE]);
    }

    public String getEqual() {
        return arr[MEDIA_DB_NAME] + arr[MEDIA_DB_PATH] + arr[MEDIA_DB_SIZE];
    }

    private static String putzen(String s) {
        s = StringUtils.replace(s, "\n", "");
        s = StringUtils.replace(s, "|", "");
        s = StringUtils.replace(s, ListeMediaDB.TRENNER, "");

        return s;
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
