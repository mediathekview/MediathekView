package mediathek.tool;

import mediathek.gui.messages.ReplaceListChangedEvent;

import java.io.File;
import java.util.ArrayList;
import java.util.List;

public final class ReplaceList {

    public static final String REPLACELIST = "Ersetzungstabelle";
    public static final String VON = "von";
    public static final int VON_NR = 0;
    public static final String NACH = "nach";
    public static final int NACH_NR = 1;
    public static final String[] COLUMN_NAMES = {VON, NACH};
    public static final int MAX_ELEM = 2;

    public static final List<String[]> list = new ArrayList<>();

    public static void init() {
        list.clear();
        list.add(new String[]{" ", "_"});
    }

    public static void clear() {
        list.clear();
    }

    public static String replace(String strCheck, boolean pfad) {
        final var it = list.iterator();
        while (it.hasNext()) {
            String[] strReplace = it.next();

            // hat der Nutzer als Suchbegriff "leer" eingegeben, dann weg damit
            if (strReplace[0].isEmpty()) {
                it.remove();
                MessageBus.getMessageBus().publishAsync(new ReplaceListChangedEvent());
                continue;
            }

            // bei Pfaden darf / oder \ natürlich nicht entfernt werden
            if (pfad && strReplace[0].equals(File.separator))
                continue;


            strCheck = strCheck.replace(strReplace[0], strReplace[1]);
        }
        return strCheck;
    }

    public static boolean check() {
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

    public static int up(int idx, boolean up) {
        String[] replace = list.remove(idx);
        int neu = idx;
        if (up) {
            if (neu > 0) {
                --neu;
            }
        } else if (neu < list.size()) {
            ++neu;
        }
        list.add(neu, replace);
        return neu;
    }

}
