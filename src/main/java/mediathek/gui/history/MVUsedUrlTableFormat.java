package mediathek.gui.history;

import ca.odell.glazedlists.gui.TableFormat;
import mediathek.controller.history.MVUsedUrl;

public class MVUsedUrlTableFormat implements TableFormat<MVUsedUrl> {
    private static final int USED_URL_DATUM = 0;
    private static final int USED_URL_THEMA = 1;
    private static final int USED_URL_TITEL = 2;
    private static final int USED_URL_URL = 3;

    @Override
    public int getColumnCount() {
        return 4;
    }

    @Override
    public String getColumnName(int column) {
        return switch (column) {
            case USED_URL_DATUM -> "Datum";
            case USED_URL_THEMA -> "Thema";
            case USED_URL_TITEL -> "Titel";
            case USED_URL_URL -> "URL";
            default -> throw new IllegalStateException();
        };
    }

    @Override
    public Object getColumnValue(MVUsedUrl baseObject, int column) {
        return switch (column) {
            case USED_URL_DATUM -> baseObject.getDatum();
            case USED_URL_THEMA -> baseObject.getThema();
            case USED_URL_TITEL -> baseObject.getTitel();
            case USED_URL_URL -> baseObject.getUrl();
            default -> throw new IllegalStateException();
        };
    }
}
