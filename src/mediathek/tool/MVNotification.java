/*
 *   MediathekView
 *   Copyright (C) 2008 W. Xaver
 *   W.Xaver[at]googlemail.com
 *   http://zdfmediathk.sourceforge.net/
 *
 *   This program is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.tool;

import java.awt.BorderLayout;
import java.awt.Color;
import javax.swing.JLabel;
import javax.swing.JWindow;
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

public class MVNotification {

    public static void addNotification(Daten daten, String titel, String meldung) {
        if (daten.mediathekGui == null) {
            return; // dann gibts keine GUI
        }
        add(titel, new String[]{meldung});
    }

    public static void addNotification(Daten daten, DatenDownload datenDownload, boolean erfolgreich) {
        if (daten.mediathekGui == null) {
            return; // dann gibts keine GUI
        }
        String[] m = {
            "Film:   " + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
            "Sender: " + datenDownload.arr[DatenDownload.DOWNLOAD_SENDER_NR],
            "Größe:  " + datenDownload.mVFilmSize.toString() + " MB",
            (erfolgreich ? "Download war erfolgreich" : "Download war fehlerhaft")
        };
        add("Download beendet", m, erfolgreich);
    }

    private static void add(String titel, String[] mmeldung) {
        add(titel, mmeldung, false);
    }

    private static void add(String titel, String[] mmeldung, boolean fehler) {
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_NOTIFICATION))) {
            String meldung = "<html><head></head><body><p>";
            for (String s : mmeldung) {
                meldung += s + "<br />";
            }
            meldung += "</p></body></html>";
//            Telegraph telegraph = new Telegraph(titel, meldung, fehler ? TelegraphType.MEDIATHEK_VIEW : TelegraphType.MEDIATHEK_VIEW_ERROR, WindowPosition.BOTTOMRIGHT, 6000);
//            TelegraphQueue queue = new TelegraphQueue();
//            queue.add(telegraph);

            JWindow messageFrame = new JWindow();
//            JFrame messageFrame = new JFrame();
//            messageFrame.setUndecorated(true);
            messageFrame.setLayout(new BorderLayout());
            JXPanel panel = new JXPanel();
            panel.setBackground(Color.BLACK);

            Color whiteTransparant = new Color(1.0f, 1.0f, 1.0f, 0.3f);
            GlossPainter gp = new GlossPainter(whiteTransparant, GlossPainter.GlossPosition.TOP);
            panel.setBackgroundPainter(gp);

            messageFrame.setContentPane(panel);

            JLabel iconLabel = new JLabel(GetIcon.getIcon(fehler ? "mv-notification.png" : "mv-notification-fehler.png", GetIcon.PFAD_RES));
            iconLabel.setVerticalAlignment(SwingConstants.TOP);
            messageFrame.getContentPane().add(iconLabel, BorderLayout.WEST);
            JXLabel meldungsLabel = new JXLabel(meldung);
            meldungsLabel.setForeground(Color.WHITE);
            // set the background painter

            messageFrame.getContentPane().add(meldungsLabel, BorderLayout.CENTER);
            messageFrame.pack();
            messageFrame.setFocusableWindowState(false);

            Notification notification = new Notification(messageFrame, WindowPosition.BOTTOMRIGHT, 20, 20, 6000);
            NotificationQueue q = new NotificationQueue();
            q.add(notification);

        }
    }
}
