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
import java.util.LinkedList;
import javax.swing.Box.Filler;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.JToolBar;
import mediathek.MediathekGui.UIButtonState;
import mediathek.daten.Daten;
import mediathek.res.GetIcon;
import mediathek.tool.Filter;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;
import org.jdesktop.swingx.JXSearchField;

public class MVToolBar extends JToolBar {

    private Daten daten;
    BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    Filler filler3;
    Filler filler5;
    Filler filler6;
    Filler filler8;
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

    LinkedList<MVButton> buttonListe = new LinkedList<>();

    public MVToolBar() {
    }

    public MVToolBar(Daten ddaten) {
        daten = ddaten;

        // init
        this.setBackground(new java.awt.Color(204, 204, 204));
        this.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        this.setFloatable(false);
        filler3 = new javax.swing.Box.Filler(new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 32767));
        jButtonFilmeLaden = new MVButton("Filmliste laden", "neue Filmliste laden", "filmlisteLaden_32.png", "filmlisteLaden_16.png");
        buttonListe.add(jButtonFilmeLaden);
        javax.swing.Box.Filler filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonInfo = new MVButton("Infos anzeigen", "Infos anzeigen", "info_32.png", "info_16.png");
        buttonListe.add(jButtonInfo);
        filler6 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonFilmAbspielen = new MVButton("Film abspielen", "Film abspielen", "film_start_32.png", "film_start_16.png");
        buttonListe.add(jButtonFilmAbspielen);
        jButtonFilmSpeichern = new MVButton("Film aufzeichnen", "Film aufzeichnen", "film_rec_32.png", "film_rec_16.png");
        buttonListe.add(jButtonFilmSpeichern);
        javax.swing.Box.Filler filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonDownloadAktualisieren = new MVButton("Downloads aktualisieren", "Downloads aktualisieren", "view-refresh_32.png", "view-refresh_16.png");
        buttonListe.add(jButtonDownloadAktualisieren);
        jButtonDownloadAlleStarten = new MVButton("alle Downloads starten", "alle Downloads starten", "download_alleStarten_32.png", "download_alleStarten_16.png");
        buttonListe.add(jButtonDownloadAlleStarten);
        jButtonDownloadFilmStarten = new MVButton("Film Starten", "Film im Player Starten", "film_start_32.png", "film_start_16.png");
        buttonListe.add(jButtonDownloadFilmStarten);
        jButtonDownloadZurueckstellen = new MVButton("Download zurückstellen", "Download zurückstellen", "undo_32.png", "undo_16.png");
        buttonListe.add(jButtonDownloadZurueckstellen);
        jButtonDownloadLoeschen = new MVButton("Download dauerhaft löschen", "Download dauerhaft löschen", "download_del_32.png", "download_del_16.png");
        buttonListe.add(jButtonDownloadLoeschen);
        jButtonDownloadAufraeumen = new MVButton("Downloads aufräumen", "Liste der Downloads aufräumen", "download_clear_32.png", "download_clear_16.png");
        buttonListe.add(jButtonDownloadAufraeumen);
        javax.swing.Box.Filler filler4 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonAbosEinschalten = new MVButton("Abos einschalten", "Abos einschalten", "ja_32.png", "ja_16.png");
        buttonListe.add(jButtonAbosEinschalten);
        jButtonAbosAusschalten = new MVButton("Abos deaktivieren", "Abos deaktivieren", "nein_32.png", "nein_16.png");
        buttonListe.add(jButtonAbosAusschalten);
        jButtonAbosLoeschen = new MVButton("Abos löschen", "Abos löschen", "del_32.png", "del_16.png");
        buttonListe.add(jButtonAbosLoeschen);
        jButtonAboAendern = new MVButton("Abo ändern", "Abo ändern", "configure_32.png", "configure_16.png");
        buttonListe.add(jButtonAboAendern);
        filler5 = new javax.swing.Box.Filler(new java.awt.Dimension(1, 5), new java.awt.Dimension(1, 5), new java.awt.Dimension(32767, 5));
        jButtonFilterPanel = new JButton();

        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        filler8 = new javax.swing.Box.Filler(new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5));
        this.add(filler3);
        jButtonFilmeLaden.setMinimumSize(new java.awt.Dimension(50, 60));
        this.add(jButtonFilmeLaden);
        this.add(filler1);
        this.add(jButtonInfo);
        this.add(filler6);
        this.add(jButtonFilmAbspielen);
        this.add(jButtonFilmSpeichern);
        this.add(filler2);
        this.add(jButtonDownloadAktualisieren);
        this.add(jButtonDownloadAlleStarten);
        this.add(jButtonDownloadFilmStarten);
        this.add(jButtonDownloadZurueckstellen);
        this.add(jButtonDownloadLoeschen);
        this.add(jButtonDownloadAufraeumen);
        this.add(filler4);
        this.add(jButtonAbosEinschalten);
        this.add(jButtonAbosAusschalten);
        this.add(jButtonAbosLoeschen);
        this.add(jButtonAboAendern);
        this.add(filler5);

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
        jTextFieldFilter.setName("Thema/Titel");
        jTextFieldFilter.setPreferredSize(new java.awt.Dimension(300, 25));
        jTextFieldFilter.setPrompt("Thema/Titel");
        this.add(jTextFieldFilter);
        this.add(filler8);
        // Searchfield
        jButtonFilterPanel.setIcon(GetIcon.getIcon("filter_anzeigen_22.png"));
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
        // Icons
        setIcon(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
        initListener();
    }

    public final void setIcon(boolean klein) {
        Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR] = Boolean.toString(klein);
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

    private void initListener() {
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
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.TRUE.toString();
                filterAnzeigen(false);
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getName());
            }
        });
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
        }

        void setIcon() {
            if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR])) {
                this.setIcon(GetIcon.getIcon(imageIconKlein));
            } else {
                this.setIcon(GetIcon.getIcon(imageIconNormal));
            }
        }
    }

    private class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JCheckBoxMenuItem[] box;

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
            box = new JCheckBoxMenuItem[buttonListe.size()];
            for (int i = 0; i < box.length; ++i) {
                box[i] = new JCheckBoxMenuItem(buttonListe.get(i).name);
                box[i].setIcon(GetIcon.getIcon(buttonListe.get(i).imageIconKlein));
                box[i].setSelected(buttonListe.get(i).anzeigen);
                box[i].addActionListener(new ActionListener() {
                    @Override
                    public void actionPerformed(ActionEvent e) {
                        setSpalten();
                    }
                });
                jPopupMenu.add(box[i]);
            }
            //anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        private void setSpalten() {
            for (int i = 0; i < box.length; ++i) {
                if (box[i] != null) {
                    buttonListe.get(i).anzeigen = box[i].isSelected();
                    buttonListe.get(i).setVisible(box[i].isSelected());
                }
            }
        }

    }
}
