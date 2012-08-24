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

import mediathek.tool.Log;
import mediathek.daten.Konstanten;
import mediathek.daten.Daten;
import mediathek.gui.dialog.DialogStarteinstellungen;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.tool.ListenerMediathekView;
import mediathek.controller.io.CheckUpdate;
import mediathek.controller.io.IoXmlLesen;
import mediathek.controller.io.ProgrammLog;
import mediathek.daten.DDaten;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDebug;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.InfoPanel;
import mediathek.tool.OSXAdapter;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialog.PanelHilfe;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;

public final class MediathekGui extends javax.swing.JFrame {

    public static final int ButtonAus = 0;
    public static final int ButtonFilme = 1;
    public static final int ButtonDonwload = 2;
    public static final int ButtonAbo = 3;
    private DDaten ddaten;
    // Check that we are on Mac OS X.  This is crucial to loading and using the OSXAdapter class.
    public static boolean MAC_OS_X = (System.getProperty("os.name").toLowerCase().startsWith("mac os x"));
    private BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private DialogEinstellungen dialogEinstellungen;
    private JSpinner jSpinnerAnzahl = new JSpinner(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));
    JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    JPanel jPanelAnzahl = new JPanel();
    JSplitPane splitPane = null;

    public MediathekGui(String[] ar) {
        String pfad = "";
        boolean max = false;
        initComponents();
        if (ar != null) {
            if (ar.length > 0) {
                if (!ar[0].startsWith("-")) {
                    if (!ar[0].endsWith(File.separator)) {
                        ar[0] += File.separator;
                    }
                    pfad = ar[0];
                }
            }
            for (int i = 0; i < ar.length; ++i) {
                if (ar[i].equals("-M")) {
                    max = true;
                }
            }
        }
        ddaten = new DDaten(pfad, true);
        Log.startMeldungen(this.getClass().getName());
        jPanelInfo.setLayout(new BorderLayout());
        jPanelInfo.add(ddaten.infoPanel, BorderLayout.CENTER);
        if (IoXmlLesen.einstellungenExistieren()) {
            ddaten.allesLaden();
        } else {
            // erster Start mit der Version 3.0
            new DialogStarteinstellungen(null, true, ddaten).setVisible(true);
        }
        this.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
        GuiFunktionen.setLook(this);
        init();
        setSize(max);
        dialogEinstellungen = new DialogEinstellungen(null, false, ddaten);
        // Set up our application to respond to the Mac OS X application menu
        registerForMacOSXEvents();
        new CheckUpdate(this, ddaten).suchen();
        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUTO) {
            if (Daten.listeFilme.filmlisteIstAelter()) {
                DDaten.filmeLaden.importFilmliste("");
            }
        }
    }

    //===================================
    // public
    //===================================
    public void setToolbar(int nr) {
        switch (nr) {
            case ButtonAus:
                buttonAus();
                break;
            case ButtonFilme:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonFilmAbspielen.setEnabled(true);
                jButtonFilmSpeichern.setEnabled(true);
                jMenuItemFilmAbspielen.setEnabled(true);
                jMenuItemFilmAufzeichnen.setEnabled(true);
                break;
            case ButtonDonwload:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonDownloadAktualisieren.setEnabled(true);
                jButtonDownloadAlleStarten.setEnabled(true);
                jButtonDownloadZurueckstellen.setEnabled(true);
                jButtonDownloadLoeschen.setEnabled(true);
                jButtonDownloadAufraeumen.setEnabled(true);
                jMenuItemDownloadStarten.setEnabled(true);
                jMenuItemDownloadStoppen.setEnabled(true);
                jMenuItemDownloadAlleStoppen.setEnabled(true);
                jMenuItemDownloadsAktualisieren.setEnabled(true);
                jMenuItemDownloadsAufraeumen.setEnabled(true);
                jMenuItemDownloadsLoeschen.setEnabled(true);
                jMenuItemDownloadsAlleStarten.setEnabled(true);
                jMenuItemDownloadAendern.setEnabled(true);
                jMenuItemDownloadsZurueckstellen.setEnabled(true);
                jMenuItemDownloadVorziehen.setEnabled(true);
                jSpinnerAnzahl.setEnabled(true);
                jLabelAnzahl.setEnabled(true);
                break;
            case ButtonAbo:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonAbosLoeschen.setEnabled(true);
                jButtonAbosEinschalten.setEnabled(true);
                jButtonAbosAusschalten.setEnabled(true);
                jMenuItemAbosEinschalten.setEnabled(true);
                jMenuItemAbosAusschalten.setEnabled(true);
                jMenuItemAbosLoeschen.setEnabled(true);
                jMenuItemAbosAendern.setEnabled(true);
                break;
        }
    }
    //===================================
    // private
    //===================================

    private void buttonAus() {
        jButtonFilmeLaden.setEnabled(false);
        jButtonFilmAbspielen.setEnabled(false);
        jButtonFilmSpeichern.setEnabled(false);
        jButtonDownloadAktualisieren.setEnabled(false);
        jButtonDownloadAlleStarten.setEnabled(false);
        jButtonDownloadZurueckstellen.setEnabled(false);
        jButtonDownloadLoeschen.setEnabled(false);
        jButtonDownloadAufraeumen.setEnabled(false);
        jButtonAbosLoeschen.setEnabled(false);
        jButtonAbosEinschalten.setEnabled(false);
        jButtonAbosAusschalten.setEnabled(false);
        // Menü
        jMenuItemFilmAbspielen.setEnabled(false);
        jMenuItemFilmAufzeichnen.setEnabled(false);
        jMenuItemDownloadsAktualisieren.setEnabled(false);
        jMenuItemDownloadsAufraeumen.setEnabled(false);
        jMenuItemDownloadsLoeschen.setEnabled(false);
        jMenuItemDownloadsAlleStarten.setEnabled(false);
        jMenuItemDownloadStarten.setEnabled(false);
        jMenuItemDownloadStoppen.setEnabled(false);
        jMenuItemDownloadAlleStoppen.setEnabled(false);
        jMenuItemDownloadAendern.setEnabled(false);
        jMenuItemDownloadsZurueckstellen.setEnabled(false);
        jMenuItemDownloadVorziehen.setEnabled(false);
        jSpinnerAnzahl.setEnabled(false);
        jLabelAnzahl.setEnabled(false);
        jMenuItemAbosEinschalten.setEnabled(false);
        jMenuItemAbosAusschalten.setEnabled(false);
        jMenuItemAbosLoeschen.setEnabled(false);
        jMenuItemAbosAendern.setEnabled(false);
    }

    private void setSize(boolean max) {
        this.pack();
        if (max || Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_FENSTER_MAX_NR])) {
            this.setExtendedState(Frame.MAXIMIZED_BOTH);
        } else {
            int x;
            int y;
            try {
                x = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_X_NR]);
                y = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_Y_NR]);
            } catch (Exception ex) {
                x = 0;
                y = 0;
            }
            if (x > 0 && y > 0) {
                this.setSize(new Dimension(x, y));
            }
        }
    }

    private void init() {
        initTabs();
        initMenue();
        initToolBar();
        DDaten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                //ddaten.infoPanel.setProgress();
                jButtonFilmeLaden.setEnabled(false);
                jMenuItemFilmlisteLaden.setEnabled(false);
            }

            @Override
            public void progress(ListenerFilmeLadenEvent event) {
                ddaten.infoPanel.setProgressBar(event);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                ddaten.infoPanel.clearProgress();
                jButtonFilmeLaden.setEnabled(true);
                jMenuItemFilmlisteLaden.setEnabled(true);
            }
        });
        addWindowListener(new java.awt.event.WindowAdapter() {
            @Override
            public void windowClosing(java.awt.event.WindowEvent evt) {
                beenden();
            }
        });
        jToolBar.addMouseListener(beobMausToolBar);
    }

    private void initTabs() {
        ddaten.mediathekGui = this;
        ddaten.guiFilme = new GuiFilme(ddaten);
        ddaten.guiDownloads = new GuiDownloads(ddaten);
        ddaten.guiAbo = new GuiAbo(ddaten);
        ddaten.guiDebug = new GuiDebug(ddaten);
        jTabbedPane.addTab("Filme", ddaten.guiFilme);
        jTabbedPane.addTab("Downloads", ddaten.guiDownloads);
        jTabbedPane.addTab("Abos", ddaten.guiAbo);
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                new PanelMeldungen(ddaten, Log.textSystem, Log.LOG_SYSTEM, "Systemmeldungen"),
                new PanelMeldungen(ddaten, Log.textProgramm, Log.LOG_PLAYER, "Meldungen Videoplayer"));
        splitPane.addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                ddaten.mediathekGui.setToolbar(MediathekGui.ButtonAus);
                ddaten.infoPanel.setIdx(InfoPanel.IDX_GUI_FILME);
            }
        });

        if (Daten.debug) {
            jTabbedPane.addTab("Debug", ddaten.guiDebug);
        }
    }

    private void setPanelMeldungen() {
        if (jCheckBoxMenuItemMeldungen.isSelected()) {
            jTabbedPane.add("Meldungen", splitPane);
        }
    }

    private void initToolBar() {
        setIcon(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
        jButtonFilmeLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmeLaden();
            }
        });
        // Tab Filme
        jButtonFilmSpeichern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiFilme.filmSpeichern();
            }
        });
        jButtonFilmAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiFilme.filmAbspielen();
            }
        });
        // Tab Downloads
        jButtonDownloadAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.aktualisieren();
            }
        });
        jButtonDownloadAufraeumen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.aufraeumen();
            }
        });
        jButtonDownloadLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.loeschen();
            }
        });
        jButtonDownloadAlleStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.starten(true);
            }
        });
        jButtonDownloadZurueckstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.zurueckstellen();
            }
        });
        // Tab Abo
        jButtonAbosEinschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.einAus(true);
            }
        });
        jButtonAbosAusschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.einAus(false);
            }
        });
        jButtonAbosLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.loeschen();
            }
        });
    }

    private void initSpinner() {
        if (Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR].equals("")) {
            jSpinnerAnzahl.setValue(1);
            Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        } else {
            jSpinnerAnzahl.setValue(Integer.parseInt(Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR]));
        }
    }

    private void initMenue() {
        initSpinner();
        // Anzahl gleichzeitiger Downlaods
        jPanelAnzahl.setLayout(new BorderLayout());
        jPanelAnzahl.add(jLabelAnzahl, BorderLayout.WEST);
        jPanelAnzahl.add(jSpinnerAnzahl, BorderLayout.EAST);
        jLabelAnzahl.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/up_down_16.png")));
        jSpinnerAnzahl.addChangeListener(new ChangeListener() {
            @Override
            public void stateChanged(ChangeEvent arg0) {
                Daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] =
                        String.valueOf(((Number) jSpinnerAnzahl.getModel().getValue()).intValue());
                Daten.notifyMediathekListener(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName());
                DDaten.setGeaendert();
            }
        });
        Daten.addAdListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        jMenuDownload.add(jPanelAnzahl);
        // Datei
        jMenuItemEinstellungen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                dialogEinstellungen.setVisible(true);
            }
        });
        jMenuItemBeenden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                beenden();
            }
        });
        // Filme
        jMenuItemFilmlisteLaden.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmeLaden();
            }
        });
        jMenuItemFilmAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiFilme.filmAbspielen();
            }
        });
        jMenuItemFilmAufzeichnen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiFilme.filmSpeichern();
            }
        });
        // Downloads
        jMenuItemDownloadsAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.aktualisieren();
            }
        });
        jMenuItemDownloadsAufraeumen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.aufraeumen();
            }
        });
        jMenuItemDownloadsLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.loeschen();
            }
        });
        jMenuItemDownloadsAlleStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.starten(true /* alle */);
            }
        });
        jMenuItemDownloadStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.starten(false /* alle */);
            }
        });
        jMenuItemDownloadsZurueckstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.zurueckstellen();
            }
        });
        jMenuItemDownloadVorziehen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.vorziehen();
            }
        });
        jMenuItemDownloadAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.aendern();
            }
        });
        jMenuItemDownloadAlleStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.stoppen(true /* alle */);
            }
        });
        jMenuItemDownloadStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiDownloads.stoppen(false /* alle */);
            }
        });

        // Abo
        jMenuItemAbosEinschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.einAus(true);
            }
        });
        jMenuItemAbosAusschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.einAus(false);
            }
        });
        jMenuItemAbosLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.loeschen();
            }
        });
        jMenuItemAbosAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.guiAbo.aendern();
            }
        });

        // Ansicht
        jCheckBoxMenuItemToolBar.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                jToolBar.setVisible(jCheckBoxMenuItemToolBar.isSelected());
            }
        });
        jCheckBoxIconKlein.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
        jCheckBoxIconKlein.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.setGeaendert();
                setIcon(jCheckBoxIconKlein.isSelected());
            }
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR]));
        ddaten.guiFilme.videoPlayerAnzeigen(jCheckBoxMenuItemVideoplayer.isSelected());
        jCheckBoxMenuItemVideoplayer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.setGeaendert();
                Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected());
                ddaten.guiFilme.videoPlayerAnzeigen(jCheckBoxMenuItemVideoplayer.isSelected());
            }
        });
        jCheckBoxMenuItemMeldungen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_MELDUNGEN_ANZEIGEN_NR]));
        setPanelMeldungen();
        jCheckBoxMenuItemMeldungen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.setGeaendert();
                Daten.system[Konstanten.SYSTEM_PANEL_MELDUNGEN_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemMeldungen.isSelected());
                if (jCheckBoxMenuItemMeldungen.isSelected()) {
                    jTabbedPane.add("Meldungen", splitPane);
                } else {
                    jTabbedPane.remove(splitPane);
                }
            }
        });
        // Hilfe
        jMenuItemAnleitung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {

                DialogOk dialogOk = new DialogOk(null, true, new PanelHilfe(ddaten), "Hilfe zum Programm");
                dialogOk.setVisible(true);
            }
        });
        jMenuItemProgrammlog.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ProgrammLog.LogDateiSchreiben(ddaten);
            }
        });
        // Debug
        if (!DDaten.debug) {
            jMenuDebug.setVisible(false);
        } else {
            jMenuItemLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    DDaten.listeFilme.clear();
                    Daten.notifyMediathekListener(ListenerMediathekView.EREIGNIS_FILMLISTE_NEU, MediathekGui.class.getSimpleName());
                }
            });
        }
    }

    private void beenden() {
        ddaten.listeDownloads.listePutzen();
        ddaten.allesAbbrechen();
        //int t = this.getExtendedState();
        if (this.getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            Daten.system[Konstanten.SYSTEM_FENSTER_MAX_NR] = Boolean.TRUE.toString();
        } else {
            Daten.system[Konstanten.SYSTEM_FENSTER_MAX_NR] = Boolean.FALSE.toString();
        }
        Daten.system[Konstanten.SYSTEM_GROESSE_X_NR] = String.valueOf(this.getSize().width);
        Daten.system[Konstanten.SYSTEM_GROESSE_Y_NR] = String.valueOf(this.getSize().height);
        ddaten.allesSpeichern();
        Log.printFehlerNummer();
        this.dispose();
        System.exit(0);
    }

    private void setIcon(boolean klein) {
        Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR] = Boolean.toString(klein);
        jCheckBoxIconKlein.setSelected(klein);
        beobMausToolBar.itemKlein.setSelected(klein);
        if (klein) {
            jButtonFilmeLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_16.png")));
            jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png")));
            jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_16.png")));
            jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png")));
            jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_16.png")));
            jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_16.png")));
            jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png")));
            jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
            jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_16.png")));
        } else {
            jButtonFilmeLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_32.png")));
            jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_32.png")));
            jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_32.png")));
            jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png")));
            jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_32.png")));
            jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_32.png")));
            jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png")));
            jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_32.png")));
            jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png")));
            jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_32.png")));
            jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_32.png")));
        }
        this.repaint();
    }

    private void filmeLaden() {
        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS) {
            DialogLeer dialog = new DialogLeer(this, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(ddaten, dialog));
            dialog.setVisible(true);
        } else {
            jButtonFilmeLaden.setEnabled(false);
            jMenuItemFilmlisteLaden.setEnabled(false);
            Daten.setGeaendert();
            Daten.filmeLaden.importFilmliste("");
        }
    }

    /**
     * This method is called from within the constructor to initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is always
     * regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jPanel1 = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jToolBar = new javax.swing.JToolBar();
        jButtonFilmeLaden = new javax.swing.JButton();
        filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonFilmAbspielen = new javax.swing.JButton();
        jButtonFilmSpeichern = new javax.swing.JButton();
        filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonDownloadAktualisieren = new javax.swing.JButton();
        jButtonDownloadAlleStarten = new javax.swing.JButton();
        jButtonDownloadZurueckstellen = new javax.swing.JButton();
        jButtonDownloadLoeschen = new javax.swing.JButton();
        jButtonDownloadAufraeumen = new javax.swing.JButton();
        filler4 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonAbosEinschalten = new javax.swing.JButton();
        jButtonAbosAusschalten = new javax.swing.JButton();
        jButtonAbosLoeschen = new javax.swing.JButton();
        jTabbedPane = new javax.swing.JTabbedPane();
        jMenuBar1 = new javax.swing.JMenuBar();
        jMenu1 = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
        jMenuItemEinstellungen = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        jMenuItemBeenden = new javax.swing.JMenuItem();
        jMenu4 = new javax.swing.JMenu();
        jMenuItemFilmAbspielen = new javax.swing.JMenuItem();
        jMenuItemFilmAufzeichnen = new javax.swing.JMenuItem();
        jMenuDownload = new javax.swing.JMenu();
        jMenuItemDownloadsAlleStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadAlleStoppen = new javax.swing.JMenuItem();
        jSeparator1 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadStoppen = new javax.swing.JMenuItem();
        jMenuItemDownloadVorziehen = new javax.swing.JMenuItem();
        jMenuItemDownloadsZurueckstellen = new javax.swing.JMenuItem();
        jMenuItemDownloadsLoeschen = new javax.swing.JMenuItem();
        jMenuItemDownloadAendern = new javax.swing.JMenuItem();
        jSeparator3 = new javax.swing.JPopupMenu.Separator();
        jMenuItemDownloadsAktualisieren = new javax.swing.JMenuItem();
        jMenuItemDownloadsAufraeumen = new javax.swing.JMenuItem();
        jMenu9 = new javax.swing.JMenu();
        jMenuItemAbosEinschalten = new javax.swing.JMenuItem();
        jMenuItemAbosAusschalten = new javax.swing.JMenuItem();
        jMenuItemAbosLoeschen = new javax.swing.JMenuItem();
        jMenuItemAbosAendern = new javax.swing.JMenuItem();
        jMenu7 = new javax.swing.JMenu();
        jCheckBoxMenuItemToolBar = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxIconKlein = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemVideoplayer = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemMeldungen = new javax.swing.JCheckBoxMenuItem();
        jMenu3 = new javax.swing.JMenu();
        jMenuItemAnleitung = new javax.swing.JMenuItem();
        jMenuItemProgrammlog = new javax.swing.JMenuItem();
        jMenuDebug = new javax.swing.JMenu();
        jMenuItemLoeschen = new javax.swing.JMenuItem();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jPanel1.setLayout(new java.awt.BorderLayout());

        javax.swing.GroupLayout jPanelInfoLayout = new javax.swing.GroupLayout(jPanelInfo);
        jPanelInfo.setLayout(jPanelInfoLayout);
        jPanelInfoLayout.setHorizontalGroup(
            jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 1083, Short.MAX_VALUE)
        );
        jPanelInfoLayout.setVerticalGroup(
            jPanelInfoLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 21, Short.MAX_VALUE)
        );

        jPanel1.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);

        jToolBar.setRollover(true);

        jButtonFilmeLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_32.png"))); // NOI18N
        jButtonFilmeLaden.setToolTipText("neue Filmliste laden");
        jButtonFilmeLaden.setFocusable(false);
        jButtonFilmeLaden.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmeLaden.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmeLaden);
        jToolBar.add(filler1);

        jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_32.png"))); // NOI18N
        jButtonFilmAbspielen.setToolTipText("Film abspielen");
        jButtonFilmAbspielen.setFocusable(false);
        jButtonFilmAbspielen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmAbspielen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmAbspielen);

        jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_32.png"))); // NOI18N
        jButtonFilmSpeichern.setToolTipText("Film speichern");
        jButtonFilmSpeichern.setFocusable(false);
        jButtonFilmSpeichern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmSpeichern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmSpeichern);
        jToolBar.add(filler2);

        jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png"))); // NOI18N
        jButtonDownloadAktualisieren.setToolTipText("Downloads aktualisieren");
        jButtonDownloadAktualisieren.setFocusable(false);
        jButtonDownloadAktualisieren.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAktualisieren.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAktualisieren);

        jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_32.png"))); // NOI18N
        jButtonDownloadAlleStarten.setToolTipText("alle Downloads starten");
        jButtonDownloadAlleStarten.setFocusable(false);
        jButtonDownloadAlleStarten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAlleStarten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAlleStarten);

        jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_32.png"))); // NOI18N
        jButtonDownloadZurueckstellen.setToolTipText("Download zurückstellen");
        jButtonDownloadZurueckstellen.setFocusable(false);
        jButtonDownloadZurueckstellen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadZurueckstellen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadZurueckstellen);

        jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonDownloadLoeschen.setToolTipText("Download löschen");
        jButtonDownloadLoeschen.setFocusable(false);
        jButtonDownloadLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadLoeschen);

        jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_32.png"))); // NOI18N
        jButtonDownloadAufraeumen.setToolTipText("Liste der Downloads aufräumen");
        jButtonDownloadAufraeumen.setFocusable(false);
        jButtonDownloadAufraeumen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAufraeumen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAufraeumen);
        jToolBar.add(filler4);

        jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_32.png"))); // NOI18N
        jButtonAbosEinschalten.setToolTipText("Abos einschalten");
        jButtonAbosEinschalten.setFocusable(false);
        jButtonAbosEinschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosEinschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosEinschalten);

        jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_32.png"))); // NOI18N
        jButtonAbosAusschalten.setToolTipText("Abos deaktivieren");
        jButtonAbosAusschalten.setFocusable(false);
        jButtonAbosAusschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosAusschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosAusschalten);

        jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonAbosLoeschen.setToolTipText("Abos löschen");
        jButtonAbosLoeschen.setFocusable(false);
        jButtonAbosLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosLoeschen);

        jPanel1.add(jToolBar, java.awt.BorderLayout.PAGE_START);
        jPanel1.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        jMenu1.setText("Datei");

        jMenuItemFilmlisteLaden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
        jMenuItemFilmlisteLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_16.png"))); // NOI18N
        jMenuItemFilmlisteLaden.setText("neue Filmliste laden");
        jMenu1.add(jMenuItemFilmlisteLaden);

        jMenuItemEinstellungen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, 0));
        jMenuItemEinstellungen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemEinstellungen.setText("Einstellungen");
        jMenuItemEinstellungen.setToolTipText("allgemeine Programmeinstellungen");
        jMenu1.add(jMenuItemEinstellungen);
        jMenu1.add(jSeparator2);

        jMenuItemBeenden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/exit_16.png"))); // NOI18N
        jMenuItemBeenden.setText("Beenden");
        jMenu1.add(jMenuItemBeenden);

        jMenuBar1.add(jMenu1);

        jMenu4.setText("Filme");

        jMenuItemFilmAbspielen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F6, 0));
        jMenuItemFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png"))); // NOI18N
        jMenuItemFilmAbspielen.setText("Film abspielen");
        jMenu4.add(jMenuItemFilmAbspielen);

        jMenuItemFilmAufzeichnen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F7, 0));
        jMenuItemFilmAufzeichnen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_16.png"))); // NOI18N
        jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
        jMenu4.add(jMenuItemFilmAufzeichnen);

        jMenuBar1.add(jMenu4);

        jMenuDownload.setText("Downloads");

        jMenuItemDownloadsAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_16.png"))); // NOI18N
        jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
        jMenuDownload.add(jMenuItemDownloadsAlleStarten);

        jMenuItemDownloadAlleStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png"))); // NOI18N
        jMenuItemDownloadAlleStoppen.setText("alle stoppen");
        jMenuItemDownloadAlleStoppen.setToolTipText("alle Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadAlleStoppen);
        jMenuDownload.add(jSeparator1);

        jMenuItemDownloadStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png"))); // NOI18N
        jMenuItemDownloadStarten.setText("Download starten");
        jMenuDownload.add(jMenuItemDownloadStarten);

        jMenuItemDownloadStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png"))); // NOI18N
        jMenuItemDownloadStoppen.setText("Download stoppen");
        jMenuDownload.add(jMenuItemDownloadStoppen);

        jMenuItemDownloadVorziehen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/up_blue_16.png"))); // NOI18N
        jMenuItemDownloadVorziehen.setText("Download vorziehen");
        jMenuDownload.add(jMenuItemDownloadVorziehen);

        jMenuItemDownloadsZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_16.png"))); // NOI18N
        jMenuItemDownloadsZurueckstellen.setText("Download zurückstellen");
        jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

        jMenuItemDownloadsLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jMenuItemDownloadsLoeschen.setText("Download löschen");
        jMenuDownload.add(jMenuItemDownloadsLoeschen);

        jMenuItemDownloadAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemDownloadAendern.setText("Download ändern");
        jMenuDownload.add(jMenuItemDownloadAendern);
        jMenuDownload.add(jSeparator3);

        jMenuItemDownloadsAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png"))); // NOI18N
        jMenuItemDownloadsAktualisieren.setText("Downloads aktualisieren");
        jMenuDownload.add(jMenuItemDownloadsAktualisieren);

        jMenuItemDownloadsAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png"))); // NOI18N
        jMenuItemDownloadsAufraeumen.setText("Downloads aufräumen");
        jMenuDownload.add(jMenuItemDownloadsAufraeumen);

        jMenuBar1.add(jMenuDownload);

        jMenu9.setText("Abos");

        jMenuItemAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png"))); // NOI18N
        jMenuItemAbosEinschalten.setText("einschalten");
        jMenu9.add(jMenuItemAbosEinschalten);

        jMenuItemAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_16.png"))); // NOI18N
        jMenuItemAbosAusschalten.setText("ausschalten");
        jMenu9.add(jMenuItemAbosAusschalten);

        jMenuItemAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jMenuItemAbosLoeschen.setText("löschen");
        jMenu9.add(jMenuItemAbosLoeschen);

        jMenuItemAbosAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemAbosAendern.setText("ändern");
        jMenu9.add(jMenuItemAbosAendern);

        jMenuBar1.add(jMenu9);

        jMenu7.setText("Ansicht");

        jCheckBoxMenuItemToolBar.setSelected(true);
        jCheckBoxMenuItemToolBar.setText("Toolbar");
        jMenu7.add(jCheckBoxMenuItemToolBar);

        jCheckBoxIconKlein.setSelected(true);
        jCheckBoxIconKlein.setText("kleine Icons");
        jMenu7.add(jCheckBoxIconKlein);

        jCheckBoxMenuItemVideoplayer.setText("weitere Videoplayer");
        jMenu7.add(jCheckBoxMenuItemVideoplayer);

        jCheckBoxMenuItemMeldungen.setText("Meldungen anzeigen");
        jMenu7.add(jCheckBoxMenuItemMeldungen);

        jMenuBar1.add(jMenu7);

        jMenu3.setText("Hilfe");

        jMenuItemAnleitung.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/agt_support.png"))); // NOI18N
        jMenuItemAnleitung.setText("Hilfe und Fragen zum Programm");
        jMenu3.add(jMenuItemAnleitung);

        jMenuItemProgrammlog.setText("Programmlog in Datei schreiben");
        jMenu3.add(jMenuItemProgrammlog);

        jMenuBar1.add(jMenu3);

        jMenuDebug.setText("Debug");

        jMenuItemLoeschen.setText("Filmliste löschen");
        jMenuDebug.add(jMenuItemLoeschen);

        jMenuBar1.add(jMenuDebug);

        setJMenuBar(jMenuBar1);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 737, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.Box.Filler filler1;
    private javax.swing.Box.Filler filler2;
    private javax.swing.Box.Filler filler4;
    private javax.swing.JButton jButtonAbosAusschalten;
    private javax.swing.JButton jButtonAbosEinschalten;
    private javax.swing.JButton jButtonAbosLoeschen;
    private javax.swing.JButton jButtonDownloadAktualisieren;
    private javax.swing.JButton jButtonDownloadAlleStarten;
    private javax.swing.JButton jButtonDownloadAufraeumen;
    private javax.swing.JButton jButtonDownloadLoeschen;
    private javax.swing.JButton jButtonDownloadZurueckstellen;
    private javax.swing.JButton jButtonFilmAbspielen;
    private javax.swing.JButton jButtonFilmSpeichern;
    private javax.swing.JButton jButtonFilmeLaden;
    private javax.swing.JCheckBoxMenuItem jCheckBoxIconKlein;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemMeldungen;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemToolBar;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemVideoplayer;
    private javax.swing.JMenu jMenu1;
    private javax.swing.JMenu jMenu3;
    private javax.swing.JMenu jMenu4;
    private javax.swing.JMenu jMenu7;
    private javax.swing.JMenu jMenu9;
    private javax.swing.JMenuBar jMenuBar1;
    private javax.swing.JMenu jMenuDebug;
    private javax.swing.JMenu jMenuDownload;
    private javax.swing.JMenuItem jMenuItemAbosAendern;
    private javax.swing.JMenuItem jMenuItemAbosAusschalten;
    private javax.swing.JMenuItem jMenuItemAbosEinschalten;
    private javax.swing.JMenuItem jMenuItemAbosLoeschen;
    private javax.swing.JMenuItem jMenuItemAnleitung;
    private javax.swing.JMenuItem jMenuItemBeenden;
    private javax.swing.JMenuItem jMenuItemDownloadAendern;
    private javax.swing.JMenuItem jMenuItemDownloadAlleStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadStarten;
    private javax.swing.JMenuItem jMenuItemDownloadStoppen;
    private javax.swing.JMenuItem jMenuItemDownloadVorziehen;
    private javax.swing.JMenuItem jMenuItemDownloadsAktualisieren;
    private javax.swing.JMenuItem jMenuItemDownloadsAlleStarten;
    private javax.swing.JMenuItem jMenuItemDownloadsAufraeumen;
    private javax.swing.JMenuItem jMenuItemDownloadsLoeschen;
    private javax.swing.JMenuItem jMenuItemDownloadsZurueckstellen;
    private javax.swing.JMenuItem jMenuItemEinstellungen;
    private javax.swing.JMenuItem jMenuItemFilmAbspielen;
    private javax.swing.JMenuItem jMenuItemFilmAufzeichnen;
    private javax.swing.JMenuItem jMenuItemFilmlisteLaden;
    private javax.swing.JMenuItem jMenuItemLoeschen;
    private javax.swing.JMenuItem jMenuItemProgrammlog;
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JTabbedPane jTabbedPane;
    private javax.swing.JToolBar jToolBar;
    // End of variables declaration//GEN-END:variables

    // Generic registration with the Mac OS X application menu
    // Checks the platform, then attempts to register with the Apple EAWT
    // See OSXAdapter.java to see how this is done without directly referencing any Apple APIs
    public void registerForMacOSXEvents() {
        if (MAC_OS_X) {
            try {
                // Generate and register the OSXAdapter, passing it a hash of all the methods we wish to
                // use as delegates for various com.apple.eawt.ApplicationListener methods
                OSXAdapter.setQuitHandler(this, getClass().getDeclaredMethod("quitForMac", (Class[]) null));
            } catch (Exception e) {
                System.err.println("Error while loading the OSXAdapter:");
                //e.printStackTrace();
            }
        }
    }

    public void quitForMac() {
        beenden();
    }

    class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JMenuItem itemAusblenden = new JMenuItem("Toolbar ausblenden");

        public BeobMausToolBar() {
            itemKlein.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR]));
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            JPopupMenu jPopupMenu = new JPopupMenu();
            itemAusblenden.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    jToolBar.setVisible(false);
                    jCheckBoxMenuItemToolBar.setSelected(false);
                }
            });
            jPopupMenu.add(itemAusblenden);
            itemKlein.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    Daten.setGeaendert();
                    setIcon(itemKlein.isSelected());
                }
            });
            jPopupMenu.add(itemKlein);
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
