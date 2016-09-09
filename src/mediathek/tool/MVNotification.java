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
import javax.swing.JPanel;
import javax.swing.JWindow;
import javax.swing.SwingConstants;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import net.sf.jcarrierpigeon.Notification;
import net.sf.jcarrierpigeon.NotificationQueue;
import net.sf.jcarrierpigeon.WindowPosition;

public class MVNotification {

    public static void addNotification(DatenDownload datenDownload, boolean erfolgreich) {
        if (Daten.mediathekGui == null) {
            return; // dann gibts keine GUI
        }
        final String[] m = {
            "Film:   " + datenDownload.arr[DatenDownload.DOWNLOAD_TITEL],
            "Sender: " + datenDownload.arr[DatenDownload.DOWNLOAD_SENDER],
            "Größe:  " + MVFilmSize.humanReadableByteCount(datenDownload.mVFilmSize.getSize(), true),
            (erfolgreich ? "Download war erfolgreich" : "Download war fehlerhaft")
        };
        add(m, erfolgreich);
    }

    private static void add(String[] mmeldung, boolean fehler) {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_NOTIFICATION))) {
            String meldung = "<html><head></head><body><p>";
            for (String s : mmeldung) {
                meldung += s + "<br />";
            }
            meldung += "</p></body></html>";

            final JWindow messageFrame = new JWindow();
            messageFrame.setLayout(new BorderLayout());
            final JPanel panel = new JPanel();
            panel.setBackground(Color.BLACK);

            messageFrame.setContentPane(panel);

            final JLabel iconLabel = new JLabel(fehler ? Icons.ICON_NOTIFICATION : Icons.ICON_NOTIFICATION_ERROR);
            iconLabel.setVerticalAlignment(SwingConstants.TOP);
            messageFrame.getContentPane().add(iconLabel, BorderLayout.WEST);
            final JLabel meldungsLabel = new JLabel(meldung);
            meldungsLabel.setForeground(Color.WHITE);

            messageFrame.getContentPane().add(meldungsLabel, BorderLayout.CENTER);
            messageFrame.pack();
            messageFrame.setFocusableWindowState(false);

            Notification notification = new Notification(messageFrame, WindowPosition.BOTTOMRIGHT, 20, 20, 6000);
            NotificationQueue q = new NotificationQueue();
            q.add(notification);

        }
    }
}
