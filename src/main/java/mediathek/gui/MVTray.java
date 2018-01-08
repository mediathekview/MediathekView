/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.gui;

import mSearch.tool.Listener;
import mSearch.tool.SysMsg;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import net.sf.jcarrierpigeon.Notification;
import net.sf.jcarrierpigeon.NotificationQueue;
import net.sf.jcarrierpigeon.WindowPosition;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public final class MVTray {

    private int trayState = 0; // 0, 1=Download, 2=Download mit Fehler
    private SystemTray tray = null;
    private TrayIcon trayIcon = null;
    private Daten daten;

    public MVTray() {
    }

    public void beenden() {
        if (tray != null && trayIcon != null) {
            tray.remove(trayIcon);
        }
    }

    public MVTray systemTray() {
        daten = Daten.getInstance();
        if (!SystemTray.isSupported()) {
            SysMsg.sysMsg("Tray wird nicht unterstützt!");
            return null;
        } else {
            tray = SystemTray.getSystemTray();
            trayIcon = new TrayIcon(Icons.ICON_TRAY);
            trayIcon.setImageAutoSize(true);
            trayIcon.setToolTip(getInfoTextDownloads());
            addListener();

            final PopupMenu popup = new PopupMenu();
            trayIcon.setPopupMenu(popup);

            MenuItem itemInfo = new MenuItem("Infos anzeigen");
            itemInfo.addActionListener(e -> addNotification(getTextInfos()));
            popup.add(itemInfo);

            MenuItem itemRemoveTray = new MenuItem("Trayicon ausblenden");
            itemRemoveTray.addActionListener(e -> {
                daten.getMediathekGui().setVisible(true); // WICHTIG!!
                MVConfig.add(MVConfig.Configs.SYSTEM_USE_TRAY, Boolean.toString(false));
                daten.getMediathekGui().setTray();
                Listener.notify(Listener.EREIGNIS_TRAYICON, MVTray.class.getSimpleName());
            });
            popup.add(itemRemoveTray);

            popup.addSeparator();
            MenuItem itemBeenden = new MenuItem("Programm beenden");
            itemBeenden.addActionListener(e -> daten.getMediathekGui().beenden(false, false));
            popup.add(itemBeenden);

            trayIcon.setPopupMenu(popup);
            try {
                tray.add(trayIcon);
                return this;
            } catch (AWTException e) {
                SysMsg.sysMsg("Tray konnte nicht geladen werden!");
            }

        }
        return null;
    }

    private void addListener() {
        trayIcon.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON1) {
                    if (e.getClickCount() == 1) {
                        daten.getMediathekGui().setVisible(!daten.getMediathekGui().isVisible());
                        if (daten.getMediathekGui().isVisible()) {
                            daten.getMediathekGui().toFront();
                            daten.getMediathekGui().requestFocus();
                        }
                    }
                }
            }
        });

        Listener.addListener(new Listener(Listener.EREIGNIS_TIMER, MVStatusBar.class.getSimpleName()) {
            int count = 0;

            @Override
            public void ping() {
                ++count;
                if (count > 3) {
                    // nur alle 3s ändern
                    trayIcon.setToolTip(getInfoTextDownloads());
                }

                // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
                int[] starts = daten.getDownloadInfos().downloadStarts;
                if (starts[6] > 0) {
                    // es gibt welche mit Fehler
                    if (trayState != 2) {
                        trayState = 2;
                        trayIcon.setImage(Icons.ICON_TRAY_ERROR);
                    }
                } else if (starts[4] > 0) {
                    // es laufen welche
                    if (trayState != 1) {
                        trayState = 1;
                        trayIcon.setImage(Icons.ICON_TRAY_DOWNLOAD);
                    }
                } else if (trayState != 0) {
                    trayState = 0;
                    trayIcon.setImage(Icons.ICON_TRAY);
                }
            }
        });
    }

    private String getTextInfos() {
        String strText = "<html><head></head><body><p>";

        strText += "Filmliste erstellt: " + daten.getListeFilme().genDate() + " Uhr  ";

        strText += "<br />";
        strText += "Anz. Filme: " + daten.getListeFilme().size();

        strText += "<br />";
        strText += getInfoTextDownloads();

        strText += "</p></body></html>";
        return strText;
    }

    private String getInfoTextDownloads() {
        String text;
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = daten.getDownloadInfos().downloadStarts;
        text = "Downloads: " + starts[0];

        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }
        if (print) {
            text += "   [ ";
            if (starts[4] == 1) {
                text += "1 läuft";
            } else {
                text += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                text += " (" + daten.getDownloadInfos().bandwidthStr + ')';
            }

            if (starts[3] == 1) {
                text += ", 1 wartet";
            } else {
                text += ", " + starts[3] + " warten";
            }
            if (starts[5] > 0) {
                if (starts[5] == 1) {
                    text += ", 1 fertig";
                } else {
                    text += ", " + starts[5] + " fertig";
                }
            }
            if (starts[6] > 0) {
                if (starts[6] == 1) {
                    text += ", 1 fehlerhaft";
                } else {
                    text += ", " + starts[6] + " fehlerhaft";
                }
            }
            text += " ]";
        }
        return text;
    }

    private void addNotification(String meldung) {
        if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_NOTIFICATION))) {

            final JWindow messageFrame = new JWindow();
            messageFrame.setLayout(new BorderLayout());
            final JPanel panel = new JPanel();
            panel.setBackground(Color.BLACK);

            messageFrame.setContentPane(panel);

            final JLabel iconLabel = new JLabel(Icons.ICON_NOTIFICATION);
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
