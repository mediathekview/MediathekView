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

import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.LinkedList;
import javax.swing.Box.Filler;
import javax.swing.*;
import mSearch.filmeSuchen.ListenerFilmeLaden;
import mSearch.filmeSuchen.ListenerFilmeLadenEvent;
import mSearch.tool.Listener;
import mSearch.tool.MVConfig;
import mediathek.MediathekGui;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.Filter;
import org.jdesktop.swingx.JXSearchField;

public final class ToolBar extends JToolBar {

    Filler filler__5 = new Filler(new java.awt.Dimension(5, 20), new java.awt.Dimension(5, 20), new java.awt.Dimension(5, 32767));
    Filler filler__10 = new Filler(new java.awt.Dimension(10, 20), new java.awt.Dimension(10, 20), new java.awt.Dimension(10, 32767));
    Filler filler__trenner = new javax.swing.Box.Filler(new java.awt.Dimension(1, 5), new java.awt.Dimension(1, 5), new java.awt.Dimension(32767, 5));

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

    private String nrToolbar = "";
    private String nrIconKlein = MVConfig.SYSTEM_ICON_KLEIN;
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
                nrToolbar = MVConfig.SYSTEM_TOOLBAR_FILME;
                break;
            case TAB_DOWNLOADS:
                nrToolbar = MVConfig.SYSTEM_TOOLBAR_DOWNLOAD;
                break;
            case TAB_ABOS:
                nrToolbar = MVConfig.SYSTEM_TOOLBAR_ABO;
                break;
            default:
                nrToolbar = "";
                nrIconKlein = "";
        }
        startup();
        setToolbar(state);
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
        initListener();
    }

    private void startupFilme() {
        // init
        jButtonFilmlisteLaden = new MVButton("Filmliste laden", "neue Filmliste laden", "filmlisteLaden_32.png", "filmlisteLaden_16.png");
        jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", "info_32.png", "info_16.png");
        jButtonFilmAbspielen = new MVButton("Film abspielen", "Film abspielen", "film_start_32.png", "film_start_16.png");
        jButtonFilmSpeichern = new MVButton("Film aufzeichnen", "Film aufzeichnen", "film_rec_32.png", "film_rec_16.png");
        this.add(filler__5);
        this.add(jButtonFilmlisteLaden);
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
        jTextFieldFilter.getFindButton().setIcon(GetIcon.getProgramIcon("suchen_22.png"));
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
        jButtonFilterPanel.setIcon(GetIcon.getProgramIcon("filter_anzeigen_22.png"));
        this.add(jButtonFilterPanel);
    }

    private void startupDownload() {
        // init
        jButtonInfo = new MVButton("Filminformation anzeigen", "Filminformation anzeigen", "info_32.png", "info_16.png");
        jButtonDownloadAktualisieren = new MVButton("Liste der Downloads aktualisieren", "Liste der Downloads aktualisieren", "view-refresh_32.png", "view-refresh_16.png");
        jButtonDownloadAlleStarten = new MVButton("alle Downloads starten", "alle Downloads starten", "download_alleStarten_32.png", "download_alleStarten_16.png");
        jButtonDownloadFilmStarten = new MVButton("Film Starten", "gespeicherten Film abspielen", "film_start_32.png", "film_start_16.png");
        jButtonDownloadZurueckstellen = new MVButton("Downloads zurückstellen", "Downloads zurückstellen", "undo_32.png", "undo_16.png");
        jButtonDownloadLoeschen = new MVButton("Downloads aus Liste entfernen", "Downloads aus Liste entfernen", "download_del_32.png", "download_del_16.png");
        jButtonDownloadAufraeumen = new MVButton("Liste der Downloads aufräumen", "Liste der Downloads aufräumen", "download_clear_32.png", "download_clear_16.png");
        this.add(filler__10);
        this.add(jButtonInfo);
        this.add(filler__10);
        this.add(jButtonDownloadAktualisieren);
        this.add(jButtonDownloadAlleStarten);
        this.add(jButtonDownloadFilmStarten);
        this.add(jButtonDownloadZurueckstellen);
        this.add(jButtonDownloadLoeschen);
        this.add(jButtonDownloadAufraeumen);
    }

    private void startupAbo() {
        // init
        jButtonAbosEinschalten = new MVButton("Abos einschalten", "Abos einschalten", "ja_32.png", "ja_16.png");
        jButtonAbosAusschalten = new MVButton("Abos ausschalten", "Abos ausschalten", "nein_32.png", "nein_16.png");
        jButtonAbosLoeschen = new MVButton("Abos löschen", "Abos löschen", "del_32.png", "del_16.png");
        jButtonAboAendern = new MVButton("Abo ändern", "Abo ändern", "configure_32.png", "configure_16.png");
        this.add(filler__10);
        this.add(jButtonAbosEinschalten);
        this.add(jButtonAbosAusschalten);
        this.add(jButtonAbosLoeschen);
        this.add(jButtonAboAendern);
    }

    public final void setIcon(boolean klein) {
        MVConfig.add(nrIconKlein, Boolean.toString(klein));
        beobMausToolBar.itemKlein.setSelected(klein);

        for (MVButton b : buttonList) {
            b.setIcon();
        }
        this.repaint();
    }

    public void setToolbar() {
        if (state != null) {
            setToolbar(state);
        }
    }

    public void setToolbar(MediathekGui.TABS sstate) {
        state = sstate;
        filterAnzeigen();
        for (MVButton b : buttonList) {
            b.setEnabled(true);
            b.setVisible(b.anzeigen);
        }
    }

    public void filterAnzeigen() {
        switch (state) {
            case TAB_FILME:
                jButtonFilterPanel.setEnabled(state.equals(MediathekGui.TABS.TAB_FILME));
                jTextFieldFilter.setEnabled(state.equals(MediathekGui.TABS.TAB_FILME));
                jTextFieldFilter.setVisible(!Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
                break;
            case TAB_DOWNLOADS:
                break;
            case TAB_ABOS:
                break;
        }
    }

    public void loadVisible() {
        if (!nrToolbar.isEmpty()) {
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
        if (!nrIconKlein.isEmpty()) {
            setIcon(Boolean.parseBoolean(MVConfig.get(nrIconKlein)));
        }
    }

    private void storeVisible() {
        if (!nrToolbar.isEmpty()) {
            MVConfig.add(nrToolbar, "");
            for (MVButton b : buttonList) {
                if (!MVConfig.get(nrToolbar).isEmpty()) {
                    MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + ":");
                }
                MVConfig.add(nrToolbar, MVConfig.get(nrToolbar) + Boolean.toString(b.anzeigen));

            }
        }
    }

    private void initListener() {
        Listener.addListener(new Listener(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class.getSimpleName()) {
            @Override
            public void ping() {
                filterAnzeigen();
            }
        });
        addMouseListener(beobMausToolBar);

        switch (state) {
            case TAB_FILME:
                Daten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
                    @Override
                    public void start(ListenerFilmeLadenEvent event) {
                        //ddaten.infoPanel.setProgress();
                        jButtonFilmlisteLaden.setEnabled(false);
                    }

                    @Override
                    public void progress(ListenerFilmeLadenEvent event) {
                    }

                    @Override
                    public void fertig(ListenerFilmeLadenEvent event) {
                        jButtonFilmlisteLaden.setEnabled(true);
                    }
                });
                jButtonFilmlisteLaden.addActionListener(e -> Daten.filmeLaden.filmeLaden(daten, false));
                jButtonFilmlisteLaden.addMouseListener(new MouseAdapter() {
                    @Override
                    public void mousePressed(MouseEvent arg0) {
                        if (arg0.isPopupTrigger()) {
                            if (jButtonFilmlisteLaden.isEnabled()) {
                                Daten.filmeLaden.filmeLaden(daten, true);
                            }
                        }
                    }

                    @Override
                    public void mouseReleased(MouseEvent arg0) {
                        if (arg0.isPopupTrigger()) {
                            if (jButtonFilmlisteLaden.isEnabled()) {
                                Daten.filmeLaden.filmeLaden(daten, true);
                            }
                        }
                    }
                });
                jButtonFilmSpeichern.addActionListener(e -> Daten.guiFilme.guiFilmeFilmSpeichern());
                jButtonFilmAbspielen.addActionListener(e -> Daten.guiFilme.guiFilmeFilmAbspielen());
                jButtonInfo.addActionListener(e -> Daten.filmInfo.showInfo());
                jButtonFilterPanel.addActionListener(e -> {
                    boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.SYSTEM_VIS_FILTER));
                    MVConfig.add(MVConfig.SYSTEM_VIS_FILTER, Boolean.toString(b));
                    filterAnzeigen();
                    Listener
                            .notify(Listener.EREIGNIS_PANEL_FILTER_ANZEIGEN, ToolBar.class
                                    .getName());
                });
                break;
            case TAB_DOWNLOADS:
                jButtonInfo.addActionListener(e -> Daten.filmInfo.showInfo());
                jButtonDownloadAktualisieren.addActionListener(e -> Daten.guiDownloads.aktualisieren());
                jButtonDownloadAufraeumen.addActionListener(e -> Daten.guiDownloads.aufraeumen());
                jButtonDownloadLoeschen.addActionListener(e -> Daten.guiDownloads.loeschen());
                jButtonDownloadAlleStarten.addActionListener(e -> Daten.guiDownloads.starten(true));
                jButtonDownloadFilmStarten.addActionListener(e -> Daten.guiDownloads.filmAbspielen());
                jButtonDownloadZurueckstellen.addActionListener(e -> Daten.guiDownloads.zurueckstellen());
                break;
            case TAB_ABOS:
                jButtonAbosEinschalten.addActionListener(e -> Daten.guiAbo.einAus(true));
                jButtonAbosAusschalten.addActionListener(e -> Daten.guiAbo.einAus(false));
                jButtonAbosLoeschen.addActionListener(e -> Daten.guiAbo.loeschen());
                jButtonAboAendern.addActionListener(actionEvent -> Daten.guiAbo.aendern());
                break;

        }
    }

    private class MVButton extends JButton {

        boolean anzeigen = true;
        String name = "";
        String imageIconKlein;
        String imageIconNormal;

        public MVButton(String nname, String ttoolTip,
                String iimageIconNormal, String iimageIconKlein) {
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
            if (!nrIconKlein.isEmpty()) {
                if (Boolean.parseBoolean(MVConfig.get(nrIconKlein))) {
                    this.setIcon(GetIcon.getProgramIcon(imageIconKlein));
                } else {
                    this.setIcon(GetIcon.getProgramIcon(imageIconNormal));
                }
            }
        }
    }

    private class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JMenuItem itemReset = new JMenuItem("zurücksetzen");
        JCheckBoxMenuItem[] box;

        public BeobMausToolBar() {
            if (!nrIconKlein.isEmpty()) {
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
            box = new JCheckBoxMenuItem[buttonList.size()];
            for (int i = 0; i < box.length; ++i) {
                box[i] = null;
                box[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                if (box[i] != null) {
                    box[i] = new JCheckBoxMenuItem(buttonList.get(i).name);
                    box[i].setIcon(GetIcon.getProgramIcon(buttonList.get(i).imageIconKlein));
                    box[i].setSelected(buttonList.get(i).anzeigen);
                    box[i].addActionListener(e -> {
                        setButtonList();
                        storeVisible();
                    });
                    jPopupMenu.add(box[i]);
                }
            }
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            itemReset.addActionListener(e -> {
                resetToolbar();
                storeVisible();
            });
            jPopupMenu.add(itemReset);

            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private void setButtonList() {
            if (box == null) {
                return;
            }
            for (int i = 0; i < box.length; ++i) {
                if (box[i] == null) {
                    continue;
                }
                buttonList.get(i).anzeigen = box[i].isSelected();
                buttonList.get(i).setVisible(box[i].isSelected());
            }
            setToolbar();
        }

        private void resetToolbar() {
            if (box == null) {
                return;
            }
            for (int i = 0; i < box.length; ++i) {
                if (box[i] == null) {
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
