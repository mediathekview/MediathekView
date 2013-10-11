/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.tool;

import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import net.sf.jcarrierpigeon.WindowPosition;
import net.sf.jtelegraph.Telegraph;
import net.sf.jtelegraph.TelegraphQueue;
import net.sf.jtelegraph.TelegraphType;

/**
 *
 * @author emil
 */
public class MVNotification {

    public static void addNotification(String titel, String meldung) {
        add(titel, new String[]{meldung});
    }

    public static void addNotification(DatenDownload datenDownload, boolean erfolgreich) {
        String[] m = {
            "Film:   " + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
            "Sender: " + datenDownload.arr[DatenDownload.DOWNLOAD_SENDER_NR],
            "Größe:  " + datenDownload.arr[DatenDownload.DOWNLOAD_GROESSE_NR] + " MB",
            (erfolgreich ? "Download war erfolgreich" : "Download war fehlerhaft")
        };
        add("Download beendet", m, erfolgreich);
    }

    public static void addNotification(String titel, String[] meldung) {
        add(titel, meldung);
    }

    private static void add(String titel, String[] mmeldung) {
        add(titel, mmeldung, false);
    }

    private static void add(String titel, String[] mmeldung, boolean fehler) {
        if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_NOTIFICATION_NR])) {
            String meldung = "<html><head></head><body><p>";
            for (String s : mmeldung) {
                meldung += s + "<br />";
            }
            meldung += "</p></body></html>";
            Telegraph telegraph = new Telegraph(titel, meldung, fehler ? TelegraphType.MEDIATHEK_VIEW : TelegraphType.MEDIATHEK_VIEW_ERROR, WindowPosition.BOTTOMRIGHT, 6000);
            TelegraphQueue queue = new TelegraphQueue();
            queue.add(telegraph);
        }
    }
}
