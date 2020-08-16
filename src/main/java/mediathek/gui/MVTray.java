package mediathek.gui;

import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.daten.DownloadStartInfo;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.TrayIconEvent;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.notification.thrift.MessageType;
import mediathek.tool.notification.thrift.NotificationMessage;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;

public final class MVTray {

    private final Daten daten;
    private int trayState; // 0, 1=Download, 2=Download mit Fehler
    private SystemTray tray;
    private TrayIcon trayIcon;
    private int count;

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
                MediathekGui.ui().setVisible(true);
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_USE_TRAY,false);
                MediathekGui.ui().initializeSystemTray();
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

        strText += "Filmliste erstellt: " + daten.getListeFilme().metaData().getGenerationDateTimeAsString() + " Uhr  ";
        strText += "\n";
        strText += "Anz. Filme: " + daten.getListeFilme().size();
        strText += "\n";
        strText += getInfoTextDownloads();

        return strText;
    }

    private String getInfoTextDownloads() {
        final DownloadStartInfo info = daten.getListeDownloads().getStarts();
        String text = "Downloads: " + info.total_starts;

        if (info.hasValues()) {
            text += "   [ ";
            text += (info.running == 1) ? "1 läuft" : info.running + " laufen";

            if (info.running > 0)
                text += " (" + daten.getDownloadInfos().getBandwidthStr() + ')';

            text += (info.initialized == 1) ? ", 1 wartet" : ", " + info.initialized + " warten";

            if (info.finished > 0)
                text += (info.finished == 1) ? ", 1 fertig" : ", " + info.finished + " fertig";

            if (info.error > 0)
                text += (info.error == 1) ? ", 1 fehlerhaft" : ", " + info.error + " fehlerhaft";

            text += " ]";
        }

        return text;
    }

    private void addNotification(String meldung) {
        final NotificationMessage msg = new NotificationMessage();
        msg.setTitle("Programminfos");
        msg.setMessage(meldung);
        msg.setType(MessageType.INFO);
        daten.notificationCenter().displayNotification(msg);
    }

}
