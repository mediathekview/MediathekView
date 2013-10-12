/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.tool;

import java.awt.BorderLayout;
import java.awt.Color;
import java.net.URL;
import javax.swing.ImageIcon;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.SwingConstants;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.res.GetIcon;
import net.sf.jcarrierpigeon.Notification;
import net.sf.jcarrierpigeon.NotificationQueue;
import net.sf.jcarrierpigeon.WindowPosition;
import org.jdesktop.swingx.JXLabel;
import org.jdesktop.swingx.JXPanel;
import org.jdesktop.swingx.painter.GlossPainter;

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
//            Telegraph telegraph = new Telegraph(titel, meldung, fehler ? TelegraphType.MEDIATHEK_VIEW : TelegraphType.MEDIATHEK_VIEW_ERROR, WindowPosition.BOTTOMRIGHT, 6000);
//            TelegraphQueue queue = new TelegraphQueue();
//            queue.add(telegraph);

            JFrame messageFrame = new JFrame();
            messageFrame.setUndecorated(true);
            messageFrame.setLayout(new BorderLayout());
            JXPanel panel = new JXPanel();
            panel.setBackground(Color.BLACK);

            Color whiteTransparant = new Color(1.0f, 1.0f, 1.0f, 0.3f);
            GlossPainter gp = new GlossPainter(whiteTransparant, GlossPainter.GlossPosition.TOP);
            panel.setBackgroundPainter(gp);

            messageFrame.setContentPane(panel);

            JLabel iconLabel = new JLabel(GetIcon.getIcon(fehler ? "mv-notification.png" : "mv-notification-fehler.png"));
            iconLabel.setVerticalAlignment(SwingConstants.TOP);
            messageFrame.getContentPane().add(iconLabel, BorderLayout.WEST);
            JXLabel meldungsLabel = new JXLabel(meldung);
            meldungsLabel.setForeground(Color.WHITE);
            // set the background painter

            messageFrame.getContentPane().add(meldungsLabel, BorderLayout.CENTER);
            messageFrame.pack();

            Notification notification = new Notification(messageFrame, WindowPosition.BOTTOMRIGHT, 50, 50, 6000);
            NotificationQueue q = new NotificationQueue();
            q.add(notification);

        }
    }
}
