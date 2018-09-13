/*
 * MediathekView
 * Copyright (C) 2008 W. Xaver
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

import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mediathek.MediathekGui;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.tool.TABS;

import javax.swing.*;
import javax.swing.Box.Filler;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;

import static mediathek.tool.TABS.TAB_DOWNLOADS;

@SuppressWarnings("serial")
public final class DownloadToolBar extends JToolBar {

    private final Filler filler__10 = new Filler(new Dimension(10, 20), new Dimension(10, 20), new Dimension(10, 32767));
    private final Filler filler__trenner = new Filler(new Dimension(1, 5), new Dimension(1, 5), new Dimension(32767, 5));
    private final MVConfig.Configs nrToolbar;
    private final Daten daten;
    private final BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private final TABS state;
    private final ArrayList<MVButton> buttonList = new ArrayList<>();
    private final MVConfig.Configs nrIconKlein = MVConfig.Configs.SYSTEM_ICON_KLEIN;
    private MVButton jButtonDownloadAktualisieren = null;

    public DownloadToolBar(Daten ddaten) {
        daten = ddaten;
        state = TAB_DOWNLOADS;
        nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_DOWNLOAD;

        startup();
        setToolbar();
        Listener.addListener(new Listener(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, DownloadToolBar.class.getSimpleName() + state) {
            @Override
            public void ping() {
                setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
            }
        });

        ddaten.getMessageBus().subscribe(this);
    }

    private void startup() {
        // init
        setFloatable(false);

        startupDownload();

        add(filler__10);
        // Icons
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        loadVisible();
        addMouseListener(beobMausToolBar);
    }

    private void startupDownload() {
        // init
        setFilmlisteLaden();
        MVButton jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_KL);
        jButtonDownloadAktualisieren = new MVButton("Liste der Downloads aktualisieren", "Liste der Downloads aktualisieren", Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_GR, Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_KL);
        MVButton jButtonDownloadAlleStarten = new MVButton("alle Downloads starten", "alle Downloads starten", Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_GR, Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_KL);
        MVButton jButtonDownloadFilmStarten = new MVButton("Film Starten", "gespeicherten Film abspielen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_KL);
        MVButton jButtonDownloadZurueckstellen = new MVButton("Downloads zurückstellen", "Downloads zurückstellen", Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_KL);
        MVButton jButtonDownloadLoeschen = new MVButton("Downloads aus Liste entfernen", "Downloads aus Liste entfernen", Icons.ICON_TOOLBAR_DOWNLOAD_DEL_GR, Icons.ICON_TOOLBAR_DOWNLOAD_DEL_KL);
        MVButton jButtonDownloadAufraeumen = new MVButton("Liste der Downloads aufräumen", "Liste der Downloads aufräumen", Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_GR, Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_KL);
        this.add(filler__10);
        this.add(jButtonInfo);
        this.add(filler__10);
        this.add(jButtonDownloadAktualisieren);
        this.add(jButtonDownloadAlleStarten);
        this.add(jButtonDownloadFilmStarten);
        this.add(jButtonDownloadZurueckstellen);
        this.add(jButtonDownloadLoeschen);
        this.add(jButtonDownloadAufraeumen);
        jButtonInfo.addActionListener(e -> Daten.filmInfo.showInfo());
        jButtonDownloadAktualisieren.addActionListener(e -> MediathekGui.ui().tabDownloads.aktualisieren());
        jButtonDownloadAufraeumen.addActionListener(e -> MediathekGui.ui().tabDownloads.aufraeumen());
        jButtonDownloadLoeschen.addActionListener(e -> MediathekGui.ui().tabDownloads.loeschen());
        jButtonDownloadAlleStarten.addActionListener(e -> MediathekGui.ui().tabDownloads.starten(true));
        jButtonDownloadFilmStarten.addActionListener(e -> MediathekGui.ui().tabDownloads.filmAbspielen());
        jButtonDownloadZurueckstellen.addActionListener(e -> MediathekGui.ui().tabDownloads.zurueckstellen());

        this.add(filler__trenner);

        // Button Filter
        JButton jButtonFilterPanel = new JButton();
        jButtonFilterPanel.setToolTipText("Filter anzeigen/ausblenden");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(SwingConstants.BOTTOM);
        jButtonFilterPanel.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);
        this.add(jButtonFilterPanel);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_DOWNLOAD_FILTER_ANZEIGEN, DownloadToolBar.class.getName());
        });

    }

    private void setFilmlisteLaden() {
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(false);
                }
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(true);
                }
            }
        });
    }

    private void setIcon(boolean klein) {
        MVConfig.add(nrIconKlein, Boolean.toString(klein));
        beobMausToolBar.itemKlein.setSelected(klein);
        for (MVButton b : buttonList) {
            b.setIcon();
        }
        this.repaint();
    }

    private void setToolbar() {
        for (MVButton b : buttonList) {
            b.setVisible(b.anzeigen);
        }
    }

    private void loadVisible() {
        if (nrToolbar != null) {
            String[] b = MVConfig.get(nrToolbar).split(":");
            if (buttonList.size() == b.length) {
                // ansonsten gibt es neue Button: dann alle anzeigen
                for (int i = 0; i < b.length; ++i) {
                    buttonList.get(i).anzeigen = Boolean.parseBoolean(b[i]);
                    buttonList.get(i).setVisible(Boolean.parseBoolean(b[i]));
                }
            }
        }
        setToolbar();
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
    }

    private void storeVisible() {
        if (nrToolbar != null) {
            MVConfig.add(nrToolbar, "");
            for (MVButton b : buttonList) {
                if (!MVConfig.get(nrToolbar).isEmpty()) {
                    MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + ':');
                }
                MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + Boolean.toString(b.anzeigen));
            }
        }
    }

    private class MVButton extends JButton {
        private final String name;
        private final ImageIcon imageIconKlein;
        private final ImageIcon imageIconNormal;
        boolean anzeigen = true;

        public MVButton(String nname, String ttoolTip,
                        ImageIcon iimageIconNormal, ImageIcon iimageIconKlein) {
            setToolTipText(ttoolTip);
            name = nname;
            imageIconKlein = iimageIconKlein;
            imageIconNormal = iimageIconNormal;
            setOpaque(false);
            setBorder(BorderFactory.createEmptyBorder(8, 8, 8, 8));
            setHorizontalTextPosition(SwingConstants.CENTER);
            setVerticalTextPosition(SwingConstants.BOTTOM);
            buttonList.add(this);
        }

        void setIcon() {
            if (Boolean.parseBoolean(MVConfig.get(nrIconKlein))) {
                this.setIcon(imageIconKlein);
            } else {
                this.setIcon(imageIconNormal);
            }
        }
    }

    private class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JMenuItem itemReset = new JMenuItem("zurücksetzen");
        JCheckBoxMenuItem[] checkBoxMenuItems;

        public BeobMausToolBar() {
            if (nrIconKlein != null) {
                itemKlein.setSelected(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
            }
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            itemKlein.addActionListener(e -> {
                setIcon(itemKlein.isSelected());
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, DownloadToolBar.class.getSimpleName() + state);
            });
            jPopupMenu.add(itemKlein);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            // Spalten ein-ausschalten
            checkBoxMenuItems = new JCheckBoxMenuItem[buttonList.size()];
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                checkBoxMenuItems[i] = null;
                checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                if (checkBoxMenuItems[i] != null) {
                    checkBoxMenuItems[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                    checkBoxMenuItems[i].setIcon(buttonList.get(i).imageIconKlein);
                    checkBoxMenuItems[i].setSelected(buttonList.get(i).anzeigen);
                    checkBoxMenuItems[i].addActionListener(e -> {
                        setButtonList();
                        storeVisible();
                    });
                    jPopupMenu.add(checkBoxMenuItems[i]);
                }
            }
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            itemReset.addActionListener(e -> {
                resetToolbar();
                storeVisible();
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, DownloadToolBar.class.getSimpleName() + state);
            });
            jPopupMenu.add(itemReset);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private void setButtonList() {
            if (checkBoxMenuItems == null) {
                return;
            }
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                if (checkBoxMenuItems[i] == null) {
                    continue;
                }
                buttonList.get(i).anzeigen = checkBoxMenuItems[i].isSelected();
                buttonList.get(i).setVisible(checkBoxMenuItems[i].isSelected());
            }
            setToolbar();
        }

        private void resetToolbar() {
            if (checkBoxMenuItems == null) {
                return;
            }
            for (int i = 0; i < checkBoxMenuItems.length; ++i) {
                if (checkBoxMenuItems[i] == null) {
                    continue;
                }
                buttonList.get(i).anzeigen = true;
                buttonList.get(i).setVisible(true);
            }
            setToolbar();
            setIcon(false);
        }

    }
}
