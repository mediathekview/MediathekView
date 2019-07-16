package mediathek.daten;

import mediathek.tool.MVMediaDBFileSize;
import org.apache.commons.lang3.StringUtils;
import org.jetbrains.annotations.NotNull;

import java.util.Arrays;

public class DatenMediaDB implements Comparable<DatenMediaDB> {

    public final static int MEDIA_DB_NAME = 0;
    public final static int MEDIA_DB_PATH = 1;
    public final static int MEDIA_DB_SIZE = 2;
    public final static int MEDIA_DB_EXTERN = 3;

    public final static int MAX_ELEM = 4;
    public final static String[] COLUMN_NAMES = {"Name", "Pfad", "Größe [MB]", "Extern"};

    public String[] arr = new String[MAX_ELEM];
    public MVMediaDBFileSize mVMediaDBFileSize;

    public DatenMediaDB(String name, String pfad, long size, boolean extern) {
        Arrays.fill(arr,"");

        arr[MEDIA_DB_NAME] = putzen(name);
        arr[MEDIA_DB_PATH] = putzen(pfad);
        mVMediaDBFileSize = new MVMediaDBFileSize(size);
        arr[MEDIA_DB_SIZE] = mVMediaDBFileSize.toString();
        arr[MEDIA_DB_EXTERN] = Boolean.toString(extern);
    }

    private static String putzen(String s) {
        s = StringUtils.replace(s, "\n", "");
        s = StringUtils.replace(s, "|", "");
        s = StringUtils.replace(s, ListeMediaDB.TRENNER, "");

        return s;
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

    public String getEqual() {
        return arr[MEDIA_DB_NAME] + arr[MEDIA_DB_PATH] + arr[MEDIA_DB_SIZE];
    }

    @Override
    public String toString() {
        String ret = "";
        for (int i = 0; i < MAX_ELEM; ++i) {
            if (i == 0) {
                ret += "| ***|" + COLUMN_NAMES[i] + ": " + arr[i] + System.lineSeparator();
            } else {
                ret += "|    |" + COLUMN_NAMES[i] + ": " + arr[i] + System.lineSeparator();
            }
        }
        return ret;
    }

    @Override
    public int compareTo(@NotNull DatenMediaDB o) {
        return 0;
    }
}
