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
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import mediathek.MediathekGui.UIButtonState;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.res.GetIcon;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;
import org.jdesktop.swingx.JXSearchField;

public class MVToolBar extends JToolBar {

    private Daten daten;
    BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    MVToolBar mVToolBar;
    javax.swing.Box.Filler filler3;
    javax.swing.Box.Filler filler5;
    javax.swing.Box.Filler filler6;
    javax.swing.Box.Filler filler8;
    javax.swing.JButton jButtonAboAendern;
    javax.swing.JButton jButtonAbosAusschalten;
    javax.swing.JButton jButtonAbosEinschalten;
    javax.swing.JButton jButtonAbosLoeschen;
    javax.swing.JButton jButtonDownloadAktualisieren;
    javax.swing.JButton jButtonDownloadAlleStarten;
    javax.swing.JButton jButtonDownloadAufraeumen;
    javax.swing.JButton jButtonDownloadFilmStarten;
    javax.swing.JButton jButtonDownloadLoeschen;
    javax.swing.JButton jButtonDownloadZurueckstellen;
    javax.swing.JButton jButtonFilmAbspielen;
    javax.swing.JButton jButtonFilmSpeichern;
    javax.swing.JButton jButtonFilmeLaden;
    javax.swing.JButton jButtonFilterPanel;
    javax.swing.JButton jButtonInfo;
    org.jdesktop.swingx.JXSearchField jTextFieldFilter;

    public MVToolBar(Daten ddaten) {
        daten = ddaten;
        mVToolBar = this;
        filler3 = new javax.swing.Box.Filler(new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 32767));
        jButtonFilmeLaden = new javax.swing.JButton();
        javax.swing.Box.Filler filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonInfo = new javax.swing.JButton();
        filler6 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonFilmAbspielen = new javax.swing.JButton();
        jButtonFilmSpeichern = new javax.swing.JButton();
        javax.swing.Box.Filler filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonDownloadAktualisieren = new javax.swing.JButton();
        jButtonDownloadAlleStarten = new javax.swing.JButton();
        jButtonDownloadFilmStarten = new javax.swing.JButton();
        jButtonDownloadZurueckstellen = new javax.swing.JButton();
        jButtonDownloadLoeschen = new javax.swing.JButton();
        jButtonDownloadAufraeumen = new javax.swing.JButton();
        javax.swing.Box.Filler filler4 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonAbosEinschalten = new javax.swing.JButton();
        jButtonAbosAusschalten = new javax.swing.JButton();
        jButtonAbosLoeschen = new javax.swing.JButton();
        jButtonAboAendern = new javax.swing.JButton();
        filler5 = new javax.swing.Box.Filler(new java.awt.Dimension(1, 5), new java.awt.Dimension(1, 5), new java.awt.Dimension(32767, 5));
        jButtonFilterPanel = new javax.swing.JButton();
        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        filler8 = new javax.swing.Box.Filler(new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5));

        this.setBackground(new java.awt.Color(204, 204, 204));
        this.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        this.setFloatable(false);
        this.add(filler3);

        jButtonFilmeLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/filmlisteLaden_32.png"))); // NOI18N
        jButtonFilmeLaden.setToolTipText("neue Filmliste laden");
        jButtonFilmeLaden.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmeLaden.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmeLaden.setMinimumSize(new java.awt.Dimension(50, 60));
        jButtonFilmeLaden.setOpaque(false);
        jButtonFilmeLaden.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonFilmeLaden);
        this.add(filler1);

        jButtonInfo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/info_32.png"))); // NOI18N
        jButtonInfo.setToolTipText("Infos anzeigen");
        jButtonInfo.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonInfo.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonInfo.setOpaque(false);
        jButtonInfo.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonInfo);
        this.add(filler6);

        jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/film_start_32.png"))); // NOI18N
        jButtonFilmAbspielen.setToolTipText("Film abspielen");
        jButtonFilmAbspielen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmAbspielen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmAbspielen.setOpaque(false);
        jButtonFilmAbspielen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonFilmAbspielen);

        jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/film_rec_32.png"))); // NOI18N
        jButtonFilmSpeichern.setToolTipText("Film aufzeichnen");
        jButtonFilmSpeichern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmSpeichern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmSpeichern.setOpaque(false);
        jButtonFilmSpeichern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonFilmSpeichern);
        this.add(filler2);

        jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png"))); // NOI18N
        jButtonDownloadAktualisieren.setToolTipText("Downloads aktualisieren");
        jButtonDownloadAktualisieren.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAktualisieren.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAktualisieren.setOpaque(false);
        jButtonDownloadAktualisieren.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadAktualisieren);

        jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_alleStarten_32.png"))); // NOI18N
        jButtonDownloadAlleStarten.setToolTipText("alle Downloads starten");
        jButtonDownloadAlleStarten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAlleStarten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAlleStarten.setOpaque(false);
        jButtonDownloadAlleStarten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadAlleStarten);

        jButtonDownloadFilmStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/film_start_32.png"))); // NOI18N
        jButtonDownloadFilmStarten.setToolTipText("Film im Player Starten");
        jButtonDownloadFilmStarten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadFilmStarten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadFilmStarten.setOpaque(false);
        jButtonDownloadFilmStarten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadFilmStarten);

        jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_32.png"))); // NOI18N
        jButtonDownloadZurueckstellen.setToolTipText("Download zurückstellen");
        jButtonDownloadZurueckstellen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadZurueckstellen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadZurueckstellen.setOpaque(false);
        jButtonDownloadZurueckstellen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadZurueckstellen);

        jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_del_32.png"))); // NOI18N
        jButtonDownloadLoeschen.setToolTipText("Download dauerhaft löschen");
        jButtonDownloadLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadLoeschen.setOpaque(false);
        jButtonDownloadLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadLoeschen);

        jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_clear_32.png"))); // NOI18N
        jButtonDownloadAufraeumen.setToolTipText("Liste der Downloads aufräumen");
        jButtonDownloadAufraeumen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAufraeumen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAufraeumen.setOpaque(false);
        jButtonDownloadAufraeumen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonDownloadAufraeumen);
        this.add(filler4);

        jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_32.png"))); // NOI18N
        jButtonAbosEinschalten.setToolTipText("Abos einschalten");
        jButtonAbosEinschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosEinschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosEinschalten.setOpaque(false);
        jButtonAbosEinschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonAbosEinschalten);

        jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_32.png"))); // NOI18N
        jButtonAbosAusschalten.setToolTipText("Abos deaktivieren");
        jButtonAbosAusschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosAusschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosAusschalten.setOpaque(false);
        jButtonAbosAusschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonAbosAusschalten);

        jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonAbosLoeschen.setToolTipText("Abos löschen");
        jButtonAbosLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosLoeschen.setOpaque(false);
        jButtonAbosLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonAbosLoeschen);

        jButtonAboAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_32.png"))); // NOI18N
        jButtonAboAendern.setToolTipText("Abo ändern");
        jButtonAboAendern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAboAendern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAboAendern.setOpaque(false);
        jButtonAboAendern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonAboAendern);
        this.add(filler5);

        jButtonFilterPanel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/filter_anzeigen_22.png"))); // NOI18N
        jButtonFilterPanel.setToolTipText("Erweiterte Suche");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setBorderPainted(false);
        jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        this.add(jButtonFilterPanel);

        jTextFieldFilter.setBackground(new java.awt.Color(230, 230, 230));
        jTextFieldFilter.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextFieldFilter.setToolTipText("Thema/Titel suchen");
        jTextFieldFilter.setDisabledTextColor(new java.awt.Color(102, 102, 102));
        jTextFieldFilter.setMaximumSize(new java.awt.Dimension(300, 35));
        jTextFieldFilter.setName("Thema/Titel"); // NOI18N
        jTextFieldFilter.setPreferredSize(new java.awt.Dimension(300, 25));
        jTextFieldFilter.setPrompt("Thema/Titel");
        this.add(jTextFieldFilter);
        this.add(filler8);

    }

    public void setIcon(boolean klein) {
        Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR] = Boolean.toString(klein);
        beobMausToolBar.itemKlein.setSelected(klein);
        if (klein) {
            jButtonFilmeLaden.setIcon(GetIcon.getIcon("filmlisteLaden_16.png"));
            jButtonFilmAbspielen.setIcon(GetIcon.getIcon("film_start_16.png"));
            jButtonInfo.setIcon(GetIcon.getIcon("info_16.png"));
            jButtonFilmSpeichern.setIcon(GetIcon.getIcon("film_rec_16.png"));
            jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_16.png"));
            jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("download_alleStarten_16.png"));
            jButtonDownloadFilmStarten.setIcon(GetIcon.getIcon("film_start_16.png"));
            jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_16.png"));
            jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("download_del_16.png"));
            jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("download_clear_16.png"));
            jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
            jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_16.png"));
            jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_16.png"));
            jButtonAboAendern.setIcon(GetIcon.getIcon("configure_16.png"));
        } else {
            jButtonFilmeLaden.setIcon(GetIcon.getIcon("filmlisteLaden_32.png"));
            jButtonFilmAbspielen.setIcon(GetIcon.getIcon("film_start_32.png"));
            jButtonInfo.setIcon(GetIcon.getIcon("info_32.png"));
            jButtonFilmSpeichern.setIcon(GetIcon.getIcon("film_rec_32.png"));
            jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_32.png"));
            jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("download_alleStarten_32.png"));
            jButtonDownloadFilmStarten.setIcon(GetIcon.getIcon("film_start_32.png"));
            jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_32.png"));
            jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("download_del_32.png"));
            jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("download_clear_32.png"));
            jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
            jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_32.png"));
            jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_32.png"));
            jButtonAboAendern.setIcon(GetIcon.getIcon("configure_32.png"));
        }
        this.repaint();
    }

    public void init() {
        jButtonFilmeLaden.setIcon(GetIcon.getIcon("filmlisteLaden_32.png"));
        jButtonFilmAbspielen.setIcon(GetIcon.getIcon("film_start_32.png"));
        jButtonInfo.setIcon(GetIcon.getIcon("info_32.png"));
        jButtonFilmSpeichern.setIcon(GetIcon.getIcon("film_rec_32.png"));
        jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_32.png"));
        jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("download_alleStarten_32.png"));
        jButtonDownloadFilmStarten.setIcon(GetIcon.getIcon("film_start_32.png"));
        jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_32.png"));
        jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("download_del_32.png"));
        jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("download_clear_32.png"));
        jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_32.png"));
        jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_32.png"));
        jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
        jButtonAboAendern.setIcon(GetIcon.getIcon("configure_32.png"));
        jButtonFilterPanel.setIcon(GetIcon.getIcon("filter_anzeigen_22.png"));
        initSearchField();
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
        setIcon(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
        jButtonFilmeLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmeLaden(false);
            }
        });
        jButtonFilmeLaden.addMouseListener(new MouseAdapter() {
            @Override
            public void mousePressed(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmeLaden.isEnabled()) {
                        filmeLaden(true);
                    }
                }
            }

            @Override
            public void mouseReleased(MouseEvent arg0) {
                if (arg0.isPopupTrigger()) {
                    if (jButtonFilmeLaden.isEnabled()) {
                        filmeLaden(true);
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
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.TRUE.toString();
                filterAnzeigen(daten.guiFilme.isVisible() && !Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getName());
            }
        });
    }

    class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
//        JMenuItem itemAusblenden = new JMenuItem("Toolbar ausblenden");

        public BeobMausToolBar() {
            itemKlein.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
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
//            itemAusblenden.addActionListener(new ActionListener() {
//                @Override
//                public void actionPerformed(ActionEvent e) {
//                    //daten.mediathekGui.setToolbarVisible(false);
//                    mVToolBar.setVisible(false);
//                }
//            });
//            jPopupMenu.add(itemAusblenden);
            itemKlein.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    setIcon(itemKlein.isSelected());
                }
            });
            jPopupMenu.add(itemKlein);
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    private void initSearchField() {
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
    }

    public void setToolbar(UIButtonState nr) {
        // public enum UIButtonState {        AUS, FILME, DOWNLOAD, ABO    }
        switch (nr) {
            case AUS:
                buttonAus();
                break;
            case FILME:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonFilmAbspielen.setEnabled(true);
                jButtonInfo.setEnabled(true);
                jButtonFilmSpeichern.setEnabled(true);
                filterAnzeigen(!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                break;
            case DOWNLOAD:
                buttonAus();
                jButtonInfo.setEnabled(true);
                jButtonFilmeLaden.setEnabled(true);
                jButtonDownloadAktualisieren.setEnabled(true);
                jButtonDownloadAlleStarten.setEnabled(true);
                jButtonDownloadFilmStarten.setEnabled(true);
                jButtonDownloadZurueckstellen.setEnabled(true);
                jButtonDownloadLoeschen.setEnabled(true);
                jButtonDownloadAufraeumen.setEnabled(true);
                filterAnzeigen(false);
                break;
            case ABO:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonAbosLoeschen.setEnabled(true);
                jButtonAbosEinschalten.setEnabled(true);
                jButtonAbosAusschalten.setEnabled(true);
                jButtonAboAendern.setEnabled(true);
                filterAnzeigen(false);
                break;
        }
    }

    public void filterAnzeigen(boolean anz) {
        jTextFieldFilter.setVisible(anz);
        jButtonFilterPanel.setVisible(anz);
    }

    private void buttonAus() {
        jButtonFilmeLaden.setEnabled(false);
        jButtonFilmAbspielen.setEnabled(false);
        jButtonInfo.setEnabled(false);
        jButtonFilmSpeichern.setEnabled(false);
        jButtonDownloadAktualisieren.setEnabled(false);
        jButtonDownloadAlleStarten.setEnabled(false);
        jButtonDownloadFilmStarten.setEnabled(false);
        jButtonDownloadZurueckstellen.setEnabled(false);
        jButtonDownloadLoeschen.setEnabled(false);
        jButtonDownloadAufraeumen.setEnabled(false);
        jButtonAbosLoeschen.setEnabled(false);
        jButtonAboAendern.setEnabled(false);
        jButtonAbosEinschalten.setEnabled(false);
        jButtonAbosAusschalten.setEnabled(false);
    }

    private void filmeLaden(boolean manuell) {
        if (manuell || GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(daten.mediathekGui, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(daten, daten.mediathekGui, dialog));
            dialog.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            jButtonFilmeLaden.setEnabled(false);
            Daten.filmeLaden.importFilmliste("");
        }
    }

}
