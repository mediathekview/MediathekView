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
package mediathek;

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.LinkedList;
import javax.swing.Box.Filler;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.Filter;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;
import org.jdesktop.swingx.JXSearchField;

public final class MVToolBar extends JToolBar {

    public static final String TOOLBAR_NIX = "";
    public static final String TOOLBAR_TAB_FILME = "Tab-Filme";
    public static final String TOOLBAR_TAB_DOWNLOADS = "Tab-Downloads";
    public static final String TOOLBAR_TAB_ABOS = "Tab-Abos";
    public static final String TOOLBAR_TAB_MELDUNGEN = "Meldungen";
    Filler filler__5;
    Filler filler__10;
    Filler filler__trenner;
    MVButton jButtonAboAendern;
    MVButton jButtonAbosAusschalten;
    MVButton jButtonAbosEinschalten;
    MVButton jButtonAbosLoeschen;
    MVButton jButtonDownloadAktualisieren;
    MVButton jButtonDownloadAlleStarten;
    MVButton jButtonDownloadAufraeumen;
    MVButton jButtonDownloadFilmStarten;
    MVButton jButtonDownloadLoeschen;
    MVButton jButtonDownloadZurueckstellen;
    MVButton jButtonFilmAbspielen;
    MVButton jButtonFilmSpeichern;
    MVButton jButtonFilmeLaden;
    JButton jButtonFilterPanel;
    MVButton jButtonInfo;
    JXSearchField jTextFieldFilter;

    private String nrToolbar = "";
    private String nrIconKlein = "";
    private final Daten daten;
    BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    LinkedList<MVButton> buttonListAlles = new LinkedList<>();
    LinkedList<MVButton> buttonListDownloads = new LinkedList<>();
    LinkedList<MVButton> buttonListAbos = new LinkedList<>();
    boolean extern = false;
    String state;
    LinkedList<MVButton> buttonListToUse;

    public MVToolBar(Daten ddaten, String sstate) {
        // für die Toolbar der Externen Fenster
        extern = true;
        daten = ddaten;
        state = sstate;
        switch (sstate) {
            case TOOLBAR_TAB_DOWNLOADS:
                nrToolbar = MVConfig.SYSTEM_TOOLBAR_DOWNLOAD_EXTERN;
                nrIconKlein = MVConfig.SYSTEM_ICON_KLEIN_DOWNLOADS_EXTERN;
                buttonListToUse = buttonListDownloads;
                break;
            case TOOLBAR_TAB_ABOS:
                nrToolbar = MVConfig.SYSTEM_TOOLBAR_ABO_EXTERN;
                nrIconKlein = MVConfig.SYSTEM_ICON_KLEIN_ABOS_EXTERN;
                buttonListToUse = buttonListAbos;
                break;
            default:
                nrToolbar = "";
                nrIconKlein = "";
                buttonListToUse = new LinkedList<>();
        }
        startup();
        setToolbar(state);
    }

    public MVToolBar(Daten ddaten) {
        // für die Toolbar im Hauptfenster
        daten = ddaten;
        nrToolbar = MVConfig.SYSTEM_TOOLBAR_ALLES;
        nrIconKlein = MVConfig.SYSTEM_ICON_KLEIN_ALLES;
        buttonListToUse = buttonListAlles;
        startup();
    }

    private void startup() {
        // init
        this.setBackground(new java.awt.Color(204, 204, 204));
        this.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        this.setFloatable(false);
        filler__5 = new Filler(new java.awt.Dimension(5, 20), new java.awt.Dimension(5, 20), new java.awt.Dimension(5, 32767));
        filler__10 = new Filler(new java.awt.Dimension(10, 20), new java.awt.Dimension(10, 20), new java.awt.Dimension(10, 32767));
        filler__trenner = new javax.swing.Box.Filler(new java.awt.Dimension(1, 5), new java.awt.Dimension(1, 5), new java.awt.Dimension(32767, 5));

        jButtonFilmeLaden = new MVButton(new String[]{TOOLBAR_TAB_FILME, TOOLBAR_TAB_DOWNLOADS}, "Filmliste laden", "neue Filmliste laden", "filmlisteLaden_32.png", "filmlisteLaden_16.png");
        buttonListAlles.add(jButtonFilmeLaden);
        buttonListDownloads.add(jButtonFilmeLaden);
        jButtonInfo = new MVButton(new String[]{TOOLBAR_TAB_FILME, TOOLBAR_TAB_DOWNLOADS}, "Infos anzeigen", "Infos anzeigen", "info_32.png", "info_16.png");
        buttonListAlles.add(jButtonInfo);
        buttonListDownloads.add(jButtonInfo);
        jButtonFilmAbspielen = new MVButton(new String[]{TOOLBAR_TAB_FILME}, "Film abspielen", "Film abspielen", "film_start_32.png", "film_start_16.png");
        buttonListAlles.add(jButtonFilmAbspielen);
        jButtonFilmSpeichern = new MVButton(new String[]{TOOLBAR_TAB_FILME}, "Film aufzeichnen", "Film aufzeichnen", "film_rec_32.png", "film_rec_16.png");
        buttonListAlles.add(jButtonFilmSpeichern);
        jButtonDownloadAktualisieren = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "Downloads aktualisieren", "Downloads aktualisieren", "view-refresh_32.png", "view-refresh_16.png");
        buttonListAlles.add(jButtonDownloadAktualisieren);
        buttonListDownloads.add(jButtonDownloadAktualisieren);
        jButtonDownloadAlleStarten = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "alle Downloads starten", "alle Downloads starten", "download_alleStarten_32.png", "download_alleStarten_16.png");
        buttonListAlles.add(jButtonDownloadAlleStarten);
        buttonListDownloads.add(jButtonDownloadAlleStarten);
        jButtonDownloadFilmStarten = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "Film Starten", "Film im Player Starten", "film_start_32.png", "film_start_16.png");
        buttonListAlles.add(jButtonDownloadFilmStarten);
        buttonListDownloads.add(jButtonDownloadFilmStarten);
        jButtonDownloadZurueckstellen = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "Download zurückstellen", "Download zurückstellen", "undo_32.png", "undo_16.png");
        buttonListAlles.add(jButtonDownloadZurueckstellen);
        buttonListDownloads.add(jButtonDownloadZurueckstellen);
        jButtonDownloadLoeschen = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "Download dauerhaft löschen", "Download dauerhaft löschen", "download_del_32.png", "download_del_16.png");
        buttonListAlles.add(jButtonDownloadLoeschen);
        buttonListDownloads.add(jButtonDownloadLoeschen);
        jButtonDownloadAufraeumen = new MVButton(new String[]{TOOLBAR_TAB_DOWNLOADS}, "Downloads aufräumen", "Liste der Downloads aufräumen", "download_clear_32.png", "download_clear_16.png");
        buttonListAlles.add(jButtonDownloadAufraeumen);
        buttonListDownloads.add(jButtonDownloadAufraeumen);
        jButtonAbosEinschalten = new MVButton(new String[]{TOOLBAR_TAB_ABOS}, "Abos einschalten", "Abos einschalten", "ja_32.png", "ja_16.png");
        buttonListAlles.add(jButtonAbosEinschalten);
        buttonListAbos.add(jButtonAbosEinschalten);
        jButtonAbosAusschalten = new MVButton(new String[]{TOOLBAR_TAB_ABOS}, "Abos deaktivieren", "Abos deaktivieren", "nein_32.png", "nein_16.png");
        buttonListAlles.add(jButtonAbosAusschalten);
        buttonListAbos.add(jButtonAbosAusschalten);
        jButtonAbosLoeschen = new MVButton(new String[]{TOOLBAR_TAB_ABOS}, "Abos löschen", "Abos löschen", "del_32.png", "del_16.png");
        buttonListAlles.add(jButtonAbosLoeschen);
        buttonListAbos.add(jButtonAbosLoeschen);
        jButtonAboAendern = new MVButton(new String[]{TOOLBAR_TAB_ABOS}, "Abo ändern", "Abo ändern", "configure_32.png", "configure_16.png");
        buttonListAlles.add(jButtonAboAendern);
        buttonListAbos.add(jButtonAboAendern);
        jButtonFilterPanel = new JButton();

        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        this.add(filler__5);
        if (!extern) {
            this.add(jButtonFilmeLaden);
            this.add(filler__10);
        }
        if (!extern || state.equals(TOOLBAR_TAB_DOWNLOADS)) {
            this.add(jButtonInfo);
            this.add(filler__10);
        }
        if (!extern) {
            this.add(jButtonFilmAbspielen);
            this.add(jButtonFilmSpeichern);
            this.add(filler__10);
        }
        if (!extern || state.equals(TOOLBAR_TAB_DOWNLOADS)) {
            this.add(jButtonDownloadAktualisieren);
            this.add(jButtonDownloadAlleStarten);
            this.add(jButtonDownloadFilmStarten);
            this.add(jButtonDownloadZurueckstellen);
            this.add(jButtonDownloadLoeschen);
            this.add(jButtonDownloadAufraeumen);
        }
        if (!extern) {
            this.add(filler__10);
        }
        if (!extern || state.equals(TOOLBAR_TAB_ABOS)) {
            this.add(jButtonAbosEinschalten);
            this.add(jButtonAbosAusschalten);
            this.add(jButtonAbosLoeschen);
            this.add(jButtonAboAendern);
        }
        if (!extern) {
            this.add(filler__trenner);
//            // Button Filter
//            jButtonFilterPanel.setToolTipText("Erweiterte Suche / Filter anzeigen");
//            jButtonFilterPanel.setBorder(null);
//            jButtonFilterPanel.setBorderPainted(false);
//            jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
//            jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
//            jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
//            jButtonFilterPanel.setOpaque(false);
//            jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
//            jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
//            jButtonFilterPanel.setIcon(GetIcon.getIcon("filter_anzeigen_22.png"));
//            this.add(jButtonFilterPanel);

            // Searchfield
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
            jTextFieldFilter.getFindButton().setIcon(GetIcon.getIcon("suchen_22.png"));
            jTextFieldFilter.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent actionEvent) {
                    Filter.checkPattern2(jTextFieldFilter);
                    daten.guiFilme.filtern();
                }
            });
            //looks like you need to explicitly set this on Linux...
            jTextFieldFilter.setInstantSearchDelay(150);
            this.add(jTextFieldFilter);

            // Button Filter
            jButtonFilterPanel.setToolTipText("Erweiterte Suche / Filter anzeigen");
            jButtonFilterPanel.setBorder(null);
            jButtonFilterPanel.setBorderPainted(false);
            jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
            jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
            jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
            jButtonFilterPanel.setOpaque(false);
            jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
            jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
            jButtonFilterPanel.setIcon(GetIcon.getIcon("filter_anzeigen_22.png"));
            this.add(jButtonFilterPanel);

            this.add(filler__10);
        }
        // Icons
        if (!nrIconKlein.isEmpty()) {
            setIcon(Boolean.parseBoolean(Daten.mVConfig.get(nrIconKlein)));
        }
        loadVisible();
        initListener();
    }

    public final void setIcon(boolean klein) {
        Daten.mVConfig.add(nrIconKlein, Boolean.toString(klein));
        beobMausToolBar.itemKlein.setSelected(klein);
        jButtonFilmeLaden.setIcon();
        jButtonFilmAbspielen.setIcon();
        jButtonInfo.setIcon();
        jButtonFilmSpeichern.setIcon();
        jButtonDownloadAktualisieren.setIcon();
        jButtonDownloadAlleStarten.setIcon();
        jButtonDownloadFilmStarten.setIcon();
        jButtonDownloadZurueckstellen.setIcon();
        jButtonDownloadLoeschen.setIcon();
        jButtonDownloadAufraeumen.setIcon();
        jButtonAbosLoeschen.setIcon();
        jButtonAbosEinschalten.setIcon();
        jButtonAbosAusschalten.setIcon();
        jButtonAboAendern.setIcon();
        this.repaint();
    }

    public void setToolbar() {
        if (state != null) {
            setToolbar(state);
        }
    }

    public void setToolbar(String sstate) {
        state = sstate;
        boolean ok;
        filterAnzeigen();
        if (sstate.equals(TOOLBAR_NIX)) {
            for (MVButton b : buttonListToUse) {
                if (extern) {
                    b.setVisible(false);
                } else {
                    b.setEnabled(false);
                }
            }
        } else {
            for (MVButton b : buttonListToUse) {
                ok = false;
                if (b.sparte.contains(sstate)) {
                    b.setEnabled(true);
                    b.setVisible(b.anzeigen);
                    ok = true;
                }
                if (!ok) {
                    if (extern) {
                        b.setVisible(false);
                    } else {
                        if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_DOWNLOAD)) && b.sparte.contains(TOOLBAR_TAB_DOWNLOADS)
                                || Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_DOWNLOAD)) && b.sparte.contains(TOOLBAR_TAB_DOWNLOADS)) {
                            b.setVisible(false);
                        } else if (!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_ABO)) && b.sparte.contains(TOOLBAR_TAB_ABOS)
                                || Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_FENSTER_ABO)) && b.sparte.contains(TOOLBAR_TAB_ABOS)) {
                            b.setVisible(false);
                        } else {
                            b.setEnabled(false);
                        }
                    }
                }
            }
        }
    }

    public void filterAnzeigen() {
        if (!extern) {
            jTextFieldFilter.setVisible(!Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER)));
        } else {
            jTextFieldFilter.setVisible(false);
        }
    }

    public void loadVisible() {
        if (!nrToolbar.isEmpty()) {
            String[] b = Daten.mVConfig.get(nrToolbar).split(":");
            if (buttonListToUse.size() == b.length) {
                // ansonsten gibt es neue Button: dann alle anzeigen
                for (int i = 0; i < b.length; ++i) {
                    buttonListToUse.get(i).anzeigen = Boolean.parseBoolean(b[i]);
                    buttonListToUse.get(i).setVisible(Boolean.parseBoolean(b[i]));
                }
            }
        }
        setToolbar();
        if (!nrIconKlein.isEmpty()) {
            setIcon(Boolean.parseBoolean(Daten.mVConfig.get(nrIconKlein)));
        }
    }

    private void storeVisible() {
        if (!nrToolbar.isEmpty()) {
            Daten.mVConfig.add(nrToolbar, "");
            for (MVButton b : buttonListToUse) {
                if (!Daten.mVConfig.get(nrToolbar).isEmpty()) {
                    Daten.mVConfig.add(nrToolbar, Daten.mVConfig.get(nrToolbar) + ":");
                }
                Daten.mVConfig.add(nrToolbar, Daten.mVConfig.get(nrToolbar) + Boolean.toString(b.anzeigen));
            }
        }
    }

    private void initListener() {
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MVToolBar.class.getSimpleName()) {
            @Override
            public void ping() {
                filterAnzeigen();
            }
        });
        addMouseListener(beobMausToolBar);
        Daten.filmeLaden.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public void start(MSearchListenerFilmeLadenEvent event) {
                //ddaten.infoPanel.setProgress();
                jButtonFilmeLaden.setEnabled(false);
            }

            @Override
            public void progress(MSearchListenerFilmeLadenEvent event) {
            }

            @Override
            public void fertig(MSearchListenerFilmeLadenEvent event) {
                jButtonFilmeLaden.setEnabled(true);
            }
        });
        jButtonFilmeLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.filmeLaden.filmeLaden(daten, false);
            }
        });
        jButtonFilmeLaden.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmeLaden.isEnabled()) {
                        Daten.filmeLaden.filmeLaden(daten, true);
                    }
                }
            }

            @Override
            public void mouseReleased(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmeLaden.isEnabled()) {
                        Daten.filmeLaden.filmeLaden(daten, true);
                    }
                }
            }
        });
        // Tab Filme
        jButtonFilmSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.filmSpeichern();
            }
        });
        jButtonFilmAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.filmAbspielen();
            }
        });
        jButtonInfo.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.filmInfoHud.show();
            }
        });
        // Tab Downloads
        jButtonDownloadAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aktualisieren();
            }
        });
        jButtonDownloadAufraeumen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aufraeumen();
            }
        });
        jButtonDownloadLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.loeschen();
            }
        });
        jButtonDownloadAlleStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.starten(true);
            }
        });
        jButtonDownloadFilmStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.filmAbspielen();
            }
        });
        jButtonDownloadZurueckstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.zurueckstellen();
            }
        });
        // Tab Abo
        jButtonAbosEinschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(true);
            }
        });
        jButtonAbosAusschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(false);
            }
        });
        jButtonAbosLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.loeschen();
            }
        });
        jButtonAboAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                daten.guiAbo.aendern();
            }
        });
        jButtonFilterPanel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                boolean b = !Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_VIS_FILTER));
                Daten.mVConfig.add(MVConfig.SYSTEM_VIS_FILTER, Boolean.toString(b));
                filterAnzeigen();
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MVToolBar.class.getName());
            }
        });
    }

    private class MVButton extends JButton {

        boolean anzeigen = true;
        String name = "";
        String imageIconKlein;
        String imageIconNormal;
        ArrayList<String> sparte = new ArrayList<>();

        public MVButton(String[] ssparte, String nname, String ttoolTip,
                String iimageIconNormal, String iimageIconKlein) {
            setToolTipText(ttoolTip);
            name = nname;
            imageIconKlein = iimageIconKlein;
            imageIconNormal = iimageIconNormal;
            for (String s : ssparte) {
                sparte.add(s);
            }
            setOpaque(false);
            setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
            setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
            setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        }

        void setIcon() {
            if (!nrIconKlein.isEmpty()) {
                if (Boolean.parseBoolean(Daten.mVConfig.get(nrIconKlein))) {
                    this.setIcon(GetIcon.getIcon(imageIconKlein));
                } else {
                    this.setIcon(GetIcon.getIcon(imageIconNormal));
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
                itemKlein.setSelected(Boolean.parseBoolean(Daten.mVConfig.get(nrIconKlein)));
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
            itemKlein.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    setIcon(itemKlein.isSelected());
                }
            });
            jPopupMenu.add(itemKlein);
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##

            // Spalten ein-ausschalten
            box = new JCheckBoxMenuItem[buttonListToUse.size()];
            for (int i = 0; i < box.length; ++i) {
                box[i] = null;
                if (extern) {
                    for (String s : buttonListToUse.get(i).sparte) {
                        if (s.equals(state)) {
                            box[i] = new JCheckBoxMenuItem(buttonListToUse.get(i).name);
                            break;
                        }
                    }
                } else {
                    box[i] = new JCheckBoxMenuItem(buttonListToUse.get(i).name);
                }
                if (box[i] != null) {
                    box[i] = new JCheckBoxMenuItem(buttonListToUse.get(i).name);
                    box[i].setIcon(GetIcon.getIcon(buttonListToUse.get(i).imageIconKlein));
                    box[i].setSelected(buttonListToUse.get(i).anzeigen);
                    box[i].addActionListener(new ActionListener() {
                        @Override
                        public void actionPerformed(ActionEvent e) {
                            setButtonList();
                            storeVisible();
                        }
                    });
                    jPopupMenu.add(box[i]);
                }
            }
            //##Trenner##
            jPopupMenu.addSeparator();
            //##Trenner##
            itemReset.addActionListener(new ActionListener() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    resetToolbar();
                    storeVisible();
                }
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
                buttonListToUse.get(i).anzeigen = box[i].isSelected();
                buttonListToUse.get(i).setVisible(box[i].isSelected());
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
                buttonListToUse.get(i).anzeigen = true;
                buttonListToUse.get(i).setVisible(true);
            }
            setToolbar();
            setIcon(false);
        }

    }
}
