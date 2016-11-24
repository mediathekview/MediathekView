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
import mediathek.tool.Filter;
import org.jdesktop.swingx.JXSearchField;

import javax.swing.Box.Filler;
import javax.swing.*;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;

@SuppressWarnings("serial")
public final class ToolBar extends JToolBar {
    Filler filler__5 = new Filler(new Dimension(5, 20), new Dimension(5, 20), new Dimension(5, 32767));
    Filler filler__10 = new Filler(new Dimension(10, 20), new Dimension(10, 20), new Dimension(10, 32767));
    Filler filler__trenner = new Filler(new Dimension(1, 5), new Dimension(1, 5), new Dimension(32767, 5));

    MVButton jButtonAboAendern = null;
    MVButton jButtonAbosAusschalten = null;
    MVButton jButtonAbosEinschalten = null;
    MVButton jButtonAbosLoeschen = null;
    MVButton jButtonDownloadAktualisieren = null;
    MVButton jButtonDownloadAlleStarten = null;
    MVButton jButtonDownloadAufraeumen = null;
    MVButton jButtonDownloadFilmStarten = null;
    MVButton jButtonDownloadLoeschen = null;
    MVButton jButtonDownloadZurueckstellen = null;
    MVButton jButtonFilmAbspielen = null;
    MVButton jButtonFilmSpeichern = null;
    MVButton jButtonFilmlisteLaden = null;
    JButton jButtonFilterPanel = null;
    MVButton jButtonInfo = null;
    public JXSearchField jTextFieldFilter;

    private MVConfig.Configs nrToolbar = null;
    private MVConfig.Configs nrIconKlein = MVConfig.Configs.SYSTEM_ICON_KLEIN;
    private final Daten daten;
    BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    boolean extern = false;
    MediathekGui.TABS state;
    LinkedList<MVButton> buttonList = new LinkedList<>();

    public ToolBar(Daten ddaten, MediathekGui.TABS state) {
        // für die Toolbar der Externen Fenster
        extern = true;
        daten = ddaten;
        this.state = state;
        switch (state) {
            case TAB_FILME:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_FILME;
                break;
            case TAB_DOWNLOADS:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_DOWNLOAD;
                break;
            case TAB_ABOS:
                nrToolbar = MVConfig.Configs.SYSTEM_TOOLBAR_ABO;
                break;
            default:
                nrToolbar = null;
                nrIconKlein = null;
        }
        startup();
        setToolbar();
        Listener.addListener(new Listener(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state) {
            @Override
            public void ping() {
                setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
            }
        });
    }

    private void startup() {
        // init
        this.setBackground(new java.awt.Color(204, 204, 204));
        this.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        this.setFloatable(false);

        switch (state) {
            case TAB_FILME:
                startupFilme();
                break;
            case TAB_DOWNLOADS:
                startupDownload();
                break;
            case TAB_ABOS:
                startupAbo();
                break;
            default:
        }

        this.add(filler__10);
        // Icons
        setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        loadVisible();
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getSimpleName()) {
            @Override
            public void ping() {
                filterAnzeigen();
            }
        });
        addMouseListener(beobMausToolBar);
    }

    private void startupFilme() {
        // init
        setFilmlisteLaden();
        jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", Icons.ICON_TOOLBAR_FILME_INFO_GR, Icons.ICON_TOOLBAR_FILME_INFO_KL);
        jButtonFilmAbspielen = new MVButton("Film abspielen", "Film abspielen", Icons.ICON_TOOLBAR_FILME_FILM_START_GR, Icons.ICON_TOOLBAR_FILME_FILM_START_KL);
        jButtonFilmSpeichern = new MVButton("Film aufzeichnen", "Film aufzeichnen", Icons.ICON_TOOLBAR_FILME_REC_GR, Icons.ICON_TOOLBAR_FILME_REC_KL);
        this.add(filler__10);
        this.add(jButtonInfo);
        this.add(filler__10);
        this.add(jButtonFilmAbspielen);
        this.add(jButtonFilmSpeichern);
        this.add(filler__10);
        this.add(filler__trenner);

        // Searchfield
        jButtonFilterPanel = new JButton();
        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        jTextFieldFilter.setBackground(new java.awt.Color(230, 230, 230));
        jTextFieldFilter.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextFieldFilter.setToolTipText("Thema/Titel suchen");
        jTextFieldFilter.setDisabledTextColor(new java.awt.Color(102, 102, 102));
        jTextFieldFilter.setMaximumSize(new java.awt.Dimension(300, 35));
        jTextFieldFilter.setName("Thema/Titel");
        jTextFieldFilter.setPreferredSize(new java.awt.Dimension(300, 25));
        jTextFieldFilter.setPrompt("Thema/Titel");
        jTextFieldFilter.setLayoutStyle(JXSearchField.LayoutStyle.MAC);
        jTextFieldFilter.setSearchMode(JXSearchField.SearchMode.INSTANT);
        jTextFieldFilter.setUseNativeSearchFieldIfPossible(true);
        jTextFieldFilter.getFindButton().setIcon(Icons.ICON_SUCHEN);
        jTextFieldFilter.addActionListener(actionEvent -> {
            Filter.checkPattern2(jTextFieldFilter);
            Daten.guiFilme.guiFilmeFiltern();
        });
        //looks like you need to explicitly set this on Linux...
        jTextFieldFilter.setInstantSearchDelay(150);
        this.add(jTextFieldFilter);

        // Button Filter
        jButtonFilterPanel.setToolTipText("Erweiterte Suche (Filter) anzeigen/ausblenden");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonFilterPanel.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);
        this.add(jButtonFilterPanel);

        jButtonFilmSpeichern.addActionListener(e -> Daten.guiFilme.guiFilmeFilmSpeichern());
        jButtonFilmAbspielen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmAbspielen());
        jButtonInfo.addActionListener(e -> Daten.filmInfo.showInfo());
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER));
            MVConfig.add(MVConfig.Configs.SYSTEM_VIS_FILTER, Boolean.toString(b));
            filterAnzeigen();
            Listener.notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getName());
        });
    }

    private void startupDownload() {
        // init
        setFilmlisteLaden();
        jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_INFO_KL);
        jButtonDownloadAktualisieren = new MVButton("Liste der Downloads aktualisieren", "Liste der Downloads aktualisieren", Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_GR, Icons.ICON_TOOLBAR_DOWNLOAD_REFRESH_KL);
        jButtonDownloadAlleStarten = new MVButton("alle Downloads starten", "alle Downloads starten", Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_GR, Icons.ICON_TOOLBAR_DOWNLOAD_ALLE_STARTEN_KL);
        jButtonDownloadFilmStarten = new MVButton("Film Starten", "gespeicherten Film abspielen", Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_GR, Icons.ICON_TOOLBAR_DOWNLOAD_FILM_START_KL);
        jButtonDownloadZurueckstellen = new MVButton("Downloads zurückstellen", "Downloads zurückstellen", Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_GR, Icons.ICON_TOOLBAR_DOWNLOAD_UNDO_KL);
        jButtonDownloadLoeschen = new MVButton("Downloads aus Liste entfernen", "Downloads aus Liste entfernen", Icons.ICON_TOOLBAR_DOWNLOAD_DEL_GR, Icons.ICON_TOOLBAR_DOWNLOAD_DEL_KL);
        jButtonDownloadAufraeumen = new MVButton("Liste der Downloads aufräumen", "Liste der Downloads aufräumen", Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_GR, Icons.ICON_TOOLBAR_DOWNLOAD_CLEAR_KL);
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
        jButtonDownloadAktualisieren.addActionListener(e -> Daten.guiDownloads.aktualisieren());
        jButtonDownloadAufraeumen.addActionListener(e -> Daten.guiDownloads.aufraeumen());
        jButtonDownloadLoeschen.addActionListener(e -> Daten.guiDownloads.loeschen());
        jButtonDownloadAlleStarten.addActionListener(e -> Daten.guiDownloads.starten(true));
        jButtonDownloadFilmStarten.addActionListener(e -> Daten.guiDownloads.filmAbspielen());
        jButtonDownloadZurueckstellen.addActionListener(e -> Daten.guiDownloads.zurueckstellen());

        this.add(filler__trenner);

        // Button Filter
        jButtonFilterPanel = new JButton();
        jButtonFilterPanel.setToolTipText("Filter anzeigen/ausblenden");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonFilterPanel.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);
        this.add(jButtonFilterPanel);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_DOWNLOAD_FILTER_ANZEIGEN, ToolBar.class.getName());
        });

    }

    private void startupAbo() {
        // init
        jButtonAbosEinschalten = new MVButton("Abos einschalten", "Abos einschalten", Icons.ICON_TOOLBAR_ABO_EIN_GR, Icons.ICON_TOOLBAR_ABO_EIN_KL);
        jButtonAbosAusschalten = new MVButton("Abos ausschalten", "Abos ausschalten", Icons.ICON_TOOLBAR_ABO_AUS_GR, Icons.ICON_TOOLBAR_ABO_AUS_KL);
        jButtonAbosLoeschen = new MVButton("Abos löschen", "Abos löschen", Icons.ICON_TOOLBAR_ABO_DEL_GR, Icons.ICON_TOOLBAR_ABO_DEL_KL);
        jButtonAboAendern = new MVButton("Abos ändern", "Abos ändern", Icons.ICON_TOOLBAR_ABO_CONFIG_GR, Icons.ICON_TOOLBAR_ABO_CONFIG_KL);
        this.add(filler__10);
        this.add(jButtonAbosEinschalten);
        this.add(jButtonAbosAusschalten);
        this.add(jButtonAbosLoeschen);
        this.add(jButtonAboAendern);
        jButtonAbosEinschalten.addActionListener(e -> Daten.guiAbo.einAus(true));
        jButtonAbosAusschalten.addActionListener(e -> Daten.guiAbo.einAus(false));
        jButtonAbosLoeschen.addActionListener(e -> Daten.guiAbo.loeschen());
        jButtonAboAendern.addActionListener(e -> Daten.guiAbo.aendern());

        this.add(filler__trenner);

        // Button Filter
        jButtonFilterPanel = new JButton();
        jButtonFilterPanel.setToolTipText("Filter anzeigen/ausblenden");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jButtonFilterPanel.setIcon(Icons.ICON_BUTTON_FILTER_ANZEIGEN);
        this.add(jButtonFilterPanel);
        jButtonFilterPanel.addActionListener(e -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, ToolBar.class.getName());
        });

    }

    private void setFilmlisteLaden() {
        jButtonFilmlisteLaden = new MVButton("Filmliste laden", "neue Filmliste laden", Icons.ICON_TOOLBAR_FILME_FILMLISTE_LADEN_GR, Icons.ICON_TOOLBAR_FILME_FILMLISTE_LADEN_KL);
        this.add(filler__5);
        this.add(jButtonFilmlisteLaden);
        daten.getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                //ddaten.infoPanel.setProgress();
                jButtonFilmlisteLaden.setEnabled(false);
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(false);
                }
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                jButtonFilmlisteLaden.setEnabled(true);
                if (jButtonDownloadAktualisieren != null) {
                    jButtonDownloadAktualisieren.setEnabled(true);
                }
            }
        });
        jButtonFilmlisteLaden.addActionListener(e -> daten.getFilmeLaden().loadFilmlistDialog(daten, false));
        jButtonFilmlisteLaden.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmlisteLaden.isEnabled()) {
                        daten.getFilmeLaden().loadFilmlistDialog(daten, true);
                    }
                }
            }

            @Override
            public void mouseReleased(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmlisteLaden.isEnabled()) {
                        daten.getFilmeLaden().loadFilmlistDialog(daten, true);
                    }
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
        filterAnzeigen();
        for (MVButton b : buttonList) {
            b.setVisible(b.anzeigen);
        }
    }

    private void filterAnzeigen() {
        if (state.equals(MediathekGui.TABS.TAB_FILME)) {
            jTextFieldFilter.setVisible(!Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_VIS_FILTER)));
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
        if (nrIconKlein != null) {
            setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        }
    }

    private void storeVisible() {
        if (nrToolbar != null) {
            MVConfig.add(nrToolbar, "");
            for (MVButton b : buttonList) {
                if (!MVConfig.get(nrToolbar).isEmpty()) {
                    MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + ":");
                }
                MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + Boolean.toString(b.anzeigen));
            }
        }
    }

    private class MVButton extends JButton {
        boolean anzeigen = true;
        private String name = "";
        private final ImageIcon imageIconKlein;
        private final ImageIcon imageIconNormal;

        public MVButton(String nname, String ttoolTip,
                        ImageIcon iimageIconNormal, ImageIcon iimageIconKlein) {
            setToolTipText(ttoolTip);
            name = nname;
            imageIconKlein = iimageIconKlein;
            imageIconNormal = iimageIconNormal;
            setOpaque(false);
            setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
            setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
            setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
            buttonList.add(this);
        }

        void setIcon() {
            if (nrIconKlein != null) {
                if (Boolean.parseBoolean(MVConfig.get(nrIconKlein))) {
                    this.setIcon(imageIconKlein);
                } else {
                    this.setIcon(imageIconNormal);
                }
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
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state);
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
                Listener.notify(Listener.EREIGNIS_TOOLBAR_BUTTON_KLEIN, ToolBar.class.getSimpleName() + state);
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
