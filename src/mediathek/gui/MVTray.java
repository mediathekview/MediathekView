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

import java.awt.AWTException;
import java.awt.MenuItem;
import java.awt.PopupMenu;
import java.awt.SystemTray;
import java.awt.TrayIcon;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import mediathek.controller.Log;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.ListenerMediathekView;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;

public final class MVTray {

    private final Daten daten;
    private int trayState = 0; // 0, 1=Download, 2=Download mit Fehler

    public MVTray(Daten daten) {
        this.daten = daten;
    }

    public void systemTray() {
        if (SystemTray.isSupported()) {
            // anlegen
            final SystemTray tray = SystemTray.getSystemTray();
            MenuItem itemBeenden = new MenuItem("Programm beenden");
            final TrayIcon trayIcon = new TrayIcon(GetIcon.getIcon("mv-tray.png").getImage());
            final PopupMenu popup = new PopupMenu();

            trayIcon.setImageAutoSize(true);

            trayIcon.setPopupMenu(popup);
            trayIcon.addMouseListener(new MouseAdapter() {
                @Override
                public void mouseClicked(MouseEvent e) {
                    if (e.getClickCount() == 1) {
                        trayIcon.displayMessage(null, getTextInfo(), TrayIcon.MessageType.INFO);

//                        daten.mediathekGui.setVisible(!daten.mediathekGui.isVisible());
//                        if (daten.mediathekGui.isVisible()) {
//                            daten.mediathekGui.toFront();
//                            daten.mediathekGui.requestFocus();
//                        }
                    }
                }
            });

            popup.addSeparator();
            itemBeenden.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    daten.mediathekGui.beenden(false, false);
                }
            });
            popup.add(itemBeenden);
            popup.addSeparator();
            trayIcon.setPopupMenu(popup);

//            trayIcon.addActionListener(new ActionListener() {
//                
//                @Override
//                public void actionPerformed(ActionEvent e) {
//                    daten.mediathekGui.setVisible(!daten.mediathekGui.isVisible());
//                }
//            });
            trayIcon.setToolTip(getTextToolTip());
            Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
                @Override
                public void start(MSListenerFilmeLadenEvent event) {
                }

                @Override
                public void progress(MSListenerFilmeLadenEvent event) {
                }

                @Override
                public void fertig(MSListenerFilmeLadenEvent event) {
                    trayIcon.setToolTip(getTextToolTip());
                }
            });

            ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_TIMER, MVStatusBar.class.getSimpleName()) {
                boolean trayIconCount = true;

                @Override
                public void ping() {
                    // Anzahl, Anz-Abo, Anz-Down, nicht gestarted, laufen, fertig OK, fertig fehler
                    int[] starts = Daten.downloadInfos.downloadStarts;
                    if (starts[6] > 0) {
                        // es gibt welche mit Fehler
                        if (trayState != 2) {
                            trayState = 2;
                            trayIcon.setImage(GetIcon.getIcon("mv-tray-fehler.png").getImage());
                        }
                    } else if (starts[4] > 0) {
                        // es laufen welche
                        if (trayState != 1) {
                            trayState = 1;
                            trayIcon.setImage(GetIcon.getIcon("mv-tray-download.png").getImage());
                        }
                    } else {
                        if (trayState != 0) {
                            trayState = 0;
                            trayIcon.setImage(GetIcon.getIcon("mv-tray.png").getImage());
                        }
                    }
                }
            });
            try {
                tray.add(trayIcon);
            } catch (AWTException e) {
                Log.systemMeldung("Tray konnte nicht geladen werden!");
            }

        } else {
            Log.systemMeldung("Tray wird nicht unterstützt!");
        }
    }

    private String getTextInfo() {
        // Text rechts: alter/neuladenIn anzeigen
        String strText = "";
        strText += "Filmliste erstellt: ";
        strText += Daten.listeFilme.genDate() + " Uhr \n";
        strText+="\n";
        strText+="\n";

        final int sekunden = Daten.listeFilme.getAge();
        if (sekunden != 0) {
            strText += "Alter: ";
            final int minuten = sekunden / 60;
            String strSekunde = String.valueOf(sekunden % 60);
            String strMinute = String.valueOf(minuten % 60);
            String strStunde = String.valueOf(minuten / 60);
            if (strSekunde.length() < 2) {
                strSekunde = "0" + strSekunde;
            }
            if (strMinute.length() < 2) {
                strMinute = "0" + strMinute;
            }
            if (strStunde.length() < 2) {
                strStunde = "0" + strStunde;
            }
            strText += strStunde + ":" + strMinute + ":" + strSekunde + "\n ";
        }

        strText+="\n";
        strText+="\n";
        strText += "Anz. Filme: " + Daten.listeFilme.size();
        return strText;
    }

    private String getTextToolTip() {
        // Text rechts: alter/neuladenIn anzeigen
        String strText = "<html xmlns=\"http://www.w3.org/1999/xhtml\">\n"
                + "<head><style type=\"text/css\"> .sans { font-family: Verdana, Geneva, sans-serif; }</style></head>\n"
                + "<body><p>\n";

        strText += "<span class=\"sans\"><b>";
        strText += "Filmliste erstellt: ";
        strText += "</b></span>";
        strText += Daten.listeFilme.genDate();
        strText += " Uhr  ";

        final int sekunden = Daten.listeFilme.getAge();
        if (sekunden != 0) {
            strText += "<br />";
            strText += "<span class=\"sans\"><b>";
            strText += "Alter: ";
            strText += "</b></span>";
            final int minuten = sekunden / 60;
            String strSekunde = String.valueOf(sekunden % 60);
            String strMinute = String.valueOf(minuten % 60);
            String strStunde = String.valueOf(minuten / 60);
            if (strSekunde.length() < 2) {
                strSekunde = "0" + strSekunde;
            }
            if (strMinute.length() < 2) {
                strMinute = "0" + strMinute;
            }
            if (strStunde.length() < 2) {
                strStunde = "0" + strStunde;
            }
            strText += strStunde + ":" + strMinute + ":" + strSekunde + " ";
        }

        strText += "<br />";
        strText += "<span class=\"sans\"><b>";
        strText += "Anz. Filme: " + Daten.listeFilme.size();
        strText += "</b></span>";
        strText += "\n</p></body></html>";
        return strText;
    }

    private String getInfoTextDownloads() {
        String textLinks;
        // nicht gestarted, laufen, fertig OK, fertig fehler
        int[] starts = Daten.downloadInfos.downloadStarts;
        if (starts[0] == 1) {
            textLinks = "1 Download";
        } else {
            textLinks = starts[0] + " Downloads";
        }
        boolean print = false;
        for (int ii = 1; ii < starts.length; ++ii) {
            if (starts[ii] > 0) {
                print = true;
                break;
            }
        }
        if (print) {
            textLinks += ": ";
            if (starts[4] == 1) {
                textLinks += "1 läuft";
            } else {
                textLinks += starts[4] + " laufen";
            }

            if (starts[4] > 0) {
                textLinks += " (" + Daten.downloadInfos.bandwidthStr + ")";
            }

            if (starts[3] == 1) {
                textLinks += ", 1 wartet";
            } else {
                textLinks += ", " + starts[3] + " warten";
            }
            if (starts[5] > 0) {
                if (starts[5] == 1) {
                    textLinks += ", 1 fertig";
                } else {
                    textLinks += ", " + starts[5] + " fertig";
                }
            }
            if (starts[6] > 0) {
                if (starts[6] == 1) {
                    textLinks += ", 1 fehlerhaft";
                } else {
                    textLinks += ", " + starts[6] + " fehlerhaft";
                }
            }
        }
        return textLinks;
    }

}
