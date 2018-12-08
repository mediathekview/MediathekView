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

import javafx.application.Platform;
import mSearch.tool.ApplicationConfiguration;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DownloadStartInfo;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.TrayIconEvent;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.control.Notifications;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public final class MVTray {

    private final Daten daten;
    private int trayState = 0; // 0, 1=Download, 2=Download mit Fehler
    private SystemTray tray = null;
    private TrayIcon trayIcon = null;
    private int count = 0;

    public MVTray() {
        daten = Daten.getInstance();
        daten.getMessageBus().subscribe(this);
    }

    @Handler
    private void handleTimerEvent(TimerEvent msg) {
        SwingUtilities.invokeLater(() -> {
            ++count;
            if (count > 3) {
                // nur alle 3s ändern
                trayIcon.setToolTip(getInfoTextDownloads());
                count = 0;
            }

            // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
            DownloadStartInfo info = daten.getListeDownloads().getStarts();
            if (info.error > 0) {
                // es gibt welche mit Fehler
                if (trayState != 2) {
                    trayState = 2;
                    trayIcon.setImage(Icons.ICON_TRAY_ERROR);
                }
            } else if (info.running > 0) {
                // es laufen welche
                if (trayState != 1) {
                    trayState = 1;
                    trayIcon.setImage(Icons.ICON_TRAY_DOWNLOAD);
                }
            } else if (trayState != 0) {
                trayState = 0;
                trayIcon.setImage(Icons.ICON_TRAY);
            }
        });
    }

    public void beenden() {
        if (tray != null && trayIcon != null) {
            tray.remove(trayIcon);
        }
    }

    public MVTray systemTray() {
        if (!SystemTray.isSupported()) {
            logger.info("Tray wird nicht unterstützt");
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
                MediathekGui.ui().setVisible(true); // WICHTIG!!
                MVConfig.add(MVConfig.Configs.SYSTEM_USE_TRAY, Boolean.toString(false));
                MediathekGui.ui().setTray();
                daten.getMessageBus().publishAsync(new TrayIconEvent());
            });
            popup.add(itemRemoveTray);

            popup.addSeparator();
            MenuItem itemBeenden = new MenuItem("Programm beenden");
            itemBeenden.addActionListener(e -> MediathekGui.ui().beenden(false, false));
            popup.add(itemBeenden);

            trayIcon.setPopupMenu(popup);
            try {
                tray.add(trayIcon);
                return this;
            } catch (AWTException e) {
                logger.error("Tray konnte nicht geladen werden", e);
            }

        }
        return null;
    }

    private static final Logger logger = LogManager.getLogger(MVTray.class);

    private void addListener() {
        trayIcon.addMouseListener(new MouseAdapter() {
            @Override
            public void mouseClicked(MouseEvent e) {
                if (e.getButton() == MouseEvent.BUTTON1) {
                    if (e.getClickCount() == 1) {
                        final MediathekGui ui = MediathekGui.ui();
                        ui.setVisible(!ui.isVisible());
                        if (ui.isVisible()) {
                            ui.toFront();
                            ui.requestFocus();
                        }
                    }
                }
            }
        });
    }

    private String getTextInfos() {
        String strText = "";

        strText += "Filmliste erstellt: " + daten.getListeFilme().genDate() + " Uhr  ";
        strText += "\n";
        strText += "Anz. Filme: " + daten.getListeFilme().size();
        strText += "\n";
        strText += getInfoTextDownloads();

        return strText;
    }

    private String getInfoTextDownloads() {
        DownloadStartInfo info = daten.getListeDownloads().getStarts();
        String text = "Downloads: " + info.total_starts;

        if (info.hasValues()) {
            text += "   [ ";
            if (info.running == 1) {
                text += "1 läuft";
            } else {
                text += info.running + " laufen";
            }

            if (info.running > 0) {
                text += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';
            }

            if (info.initialized == 1) {
                text += ", 1 wartet";
            } else {
                text += ", " + info.initialized + " warten";
            }

            if (info.finished > 0) {
                if (info.finished == 1) {
                    text += ", 1 fertig";
                } else {
                    text += ", " + info.finished + " fertig";
                }
            }
            if (info.error > 0) {
                if (info.error == 1) {
                    text += ", 1 fehlerhaft";
                } else {
                    text += ", " + info.error + " fehlerhaft";
                }
            }
            text += " ]";
        }
        return text;
    }

    private void addNotification(String meldung) {
        if (ApplicationConfiguration.getConfiguration().getBoolean(ApplicationConfiguration.APPLICATION_SHOW_NOTIFICATIONS,true)) {
            Platform.runLater(() -> {
                Notifications msg = Notifications.create();
                msg.title("Programminfos");
                msg.text(meldung);
                msg.showInformation();
            });
        }
    }

}
