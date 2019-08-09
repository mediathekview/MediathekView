package mediathek.controller.history;

import java.util.List;

public class MVUsedUrlModelHelper {
    public static final String[] TITLE_HEADER = {"Datum", "Thema", "Titel", "Url"};
    public static final int USED_URL_DATUM = 0;
    public static final int USED_URL_THEMA = 1;
    public static final int USED_URL_TITEL = 2;
    public static final int USED_URL_URL = 3;

    /**
     * Creates a "model" for the table.
     */
    public static Object[][] getObjectData(List<MVUsedUrl> listeUrlsSortDate) {
        int i = 0;
        final Object[][] object = new Object[listeUrlsSortDate.size()][];
        for (var item : listeUrlsSortDate) {
            object[i] = new String[]{item.getDatum(), item.getThema(), item.getTitel(), item.getUrl()};
            ++i;
        }
        return object;
    }


}
