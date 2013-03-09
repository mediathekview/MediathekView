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

import com.jidesoft.utils.SystemInfo;
import java.awt.BorderLayout;
import java.awt.Dimension;
import java.awt.Frame;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import javax.imageio.ImageIO;
import javax.swing.ImageIcon;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.SpinnerNumberModel;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.controller.io.CheckUpdate;
import mediathek.controller.io.IoXmlLesen;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDebug;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.gui.dialog.DialogLeer;
import mediathek.gui.dialog.DialogOk;
import mediathek.gui.dialog.DialogStarteinstellungen;
import mediathek.gui.dialog.MVAboutDialog;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.gui.dialog.PanelHilfe;
import mediathek.gui.dialogEinstellungen.DialogEinstellungen;
import mediathek.gui.dialogEinstellungen.PanelFilmlisteLaden;
import mediathek.gui.dialogEinstellungen.PanelInfoStarts;
import mediathek.gui.dialogEinstellungen.PanelMeldungen;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import org.jdesktop.swingx.JXSearchField;
import org.simplericity.macify.eawt.Application;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.simplericity.macify.eawt.DefaultApplication;

public final class MediathekGui extends javax.swing.JFrame implements ApplicationListener {

    private DDaten ddaten;
    private BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private DialogEinstellungen dialogEinstellungen;
    private JSpinner jSpinnerAnzahl = new JSpinner(new SpinnerNumberModel(1, 1, 9, 1));
    JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    JPanel jPanelAnzahl = new JPanel();
    JSplitPane splitPane = null;
    private MVStatusBar statusBar;

    public enum UIButtonState {

        AUS, FILME, DOWNLOAD, ABO
    }

    /**
     * Legt die statusbar an.
     */
    private void createStatusBar() {
        statusBar = new MVStatusBar();
        jPanelInfo.add(statusBar.getComponent(), BorderLayout.PAGE_START);
    }

    public MVStatusBar getStatusBar() {
        return statusBar;
    }

    public String getFilterToolBar() {
        return jTextFieldFilter.getText();
    }

    public MediathekGui(String[] ar) {
        //we must check if we were started with enough memory, do it as early as possible
        checkMemoryRequirements();

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
            for (String anAr : ar) {
                if (anAr.equals("-M")) {
                    max = true;
                }
            }
        }

        ddaten = new DDaten(pfad, true);
        Log.startMeldungen(this.getClass().getName());

        createStatusBar();

        //create the Film Information HUD
        ddaten.filmInfoHud = new MVFilmInformation(this, jTabbedPane);
        if (IoXmlLesen.einstellungenExistieren()) {
            // gibt schon Programmeinstellungen, dann damit starten
            ddaten.allesLaden();
        } else {
            // erster Start
            new DialogStarteinstellungen(null, true, ddaten).setVisible(true);
        }

        setOrgTitel();
        GuiFunktionen.setLook(this);
        init();
        setSize(max);

        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(this, ddaten);

        // Prüfen obs ein Programmupdate gibt
        new CheckUpdate(ddaten).suchen();

        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUTO) {
            if (Daten.listeFilme.filmlisteZuAlt()) {
                Log.systemMeldung("Neue Filmliste laden");
                DDaten.filmeLaden.importFilmliste("");
            }
        }

        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PROGRAMM_MEDIATHEKGUI_ORG_TITEL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setOrgTitel();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PROGRAMM_MEDIATHEKGUI_PROGRAMM_AKTUELL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelAllesAktuell();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PROGRAMM_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelUpdate();
            }
        });
    }

    /**
     * This function will check if we have enough memory.
     * Experience showed that default memory allocation for java RT is not enough
     */
    protected void checkMemoryRequirements() {
        if (SystemInfo.isMacOSX()) {
            //all values in bytes
            final long TO_MBYTES = (1024 * 1024);
            long totalMemory = Runtime.getRuntime().maxMemory() / TO_MBYTES;
            //if we have less than 1GB, show warning
            //JDK7 reports less than 1000MBytes free memory...
            if (totalMemory < 900) {
                final String strMessage = "<html>Sie haben MediathekView wahrscheinlich nicht mit dem Startscript gestartet.<br>"
                        + "Dadurch kann das Laden der Filmliste wegen zuwenig Arbeitsspeicher fehlschlagen.<br><br>"
                        + "<b>Bitte nutzen Sie die Startscripte!</b></html>";
                JOptionPane.showMessageDialog(this, strMessage, "Arbeitsspeicher", JOptionPane.WARNING_MESSAGE);
            }
        }
    }

    //===================================
    // public
    //===================================
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
                jButtonFilmSpeichern.setEnabled(true);
                jMenuItemFilmAbspielen.setEnabled(true);
                jMenuItemFilmAufzeichnen.setEnabled(true);
                jCheckBoxMenuItemFilterAnzeigen.setEnabled(true);
                filterAnzeigen(!Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                break;
            case DOWNLOAD:
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
                jCheckBoxMenuItemShutDown.setEnabled(true);
                jSpinnerAnzahl.setEnabled(true);
                jLabelAnzahl.setEnabled(true);
                filterAnzeigen(false);
                break;
            case ABO:
                buttonAus();
                jButtonFilmeLaden.setEnabled(true);
                jButtonAbosLoeschen.setEnabled(true);
                jButtonAbosEinschalten.setEnabled(true);
                jButtonAbosAusschalten.setEnabled(true);
                jButtonAboAendern.setEnabled(true);
                jMenuItemAbosEinschalten.setEnabled(true);
                jMenuItemAbosAusschalten.setEnabled(true);
                jMenuItemAbosLoeschen.setEnabled(true);
                jMenuItemAbosAendern.setEnabled(true);
                filterAnzeigen(false);
                break;
        }
    }

    public void filterAnzeigen(boolean anz) {
        jTextFieldFilter.setVisible(anz);
        jButtonFilterPanel.setVisible(anz);
    }

    //===================================
    // private
    //===================================
    private void setOrgTitel() {
        this.setTitle(Konstanten.PROGRAMMNAME + " " + Konstanten.VERSION);
    }

    private void setTitelUpdate() {
        setTitle("Ein Programmupdate ist verfügbar");
    }

    private void setTitelAllesAktuell() {
        setTitle("Programmversion ist aktuell");
    }

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
        jButtonAboAendern.setEnabled(false);
        jButtonAbosEinschalten.setEnabled(false);
        jButtonAbosAusschalten.setEnabled(false);
        // Menü
        jMenuItemFilmAbspielen.setEnabled(false);
        jMenuItemFilmAufzeichnen.setEnabled(false);
        jCheckBoxMenuItemFilterAnzeigen.setEnabled(false);
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
        jCheckBoxMenuItemShutDown.setEnabled(false);
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
            int breite, hoehe, posX, posY;
            try {
                breite = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_X_NR]);
                hoehe = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_Y_NR]);
                posX = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_X_NR]);
                posY = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_Y_NR]);
            } catch (NumberFormatException ex) {
                breite = 0;
                hoehe = 0;
                posX = 0;
                posY = 0;
            }
            if (breite > 0 && hoehe > 0) {
                this.setSize(new Dimension(breite, hoehe));
                this.setLocation(posX, posY);
            }
        }
    }

    /**
     * Initialisiert das SearchField
     */
    private void initSearchField() {
        jTextFieldFilter.setLayoutStyle(JXSearchField.LayoutStyle.MAC);
        jTextFieldFilter.setSearchMode(JXSearchField.SearchMode.REGULAR);
        jTextFieldFilter.setUseNativeSearchFieldIfPossible(true);
        jTextFieldFilter.getFindButton().setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/viewmag_22.png")));
        jTextFieldFilter.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                Filter.checkPattern2(jTextFieldFilter);
                ddaten.guiFilme.filtern();
            }
        });
        jTextFieldFilter.getDocument().addDocumentListener(new DocumentListener() {
            @Override
            public void insertUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void removeUpdate(DocumentEvent e) {
                tus();
            }

            @Override
            public void changedUpdate(DocumentEvent e) {
                tus();
            }

            private void tus() {
                Filter.checkPattern2(jTextFieldFilter);
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ECHTZEITSUCHE_NR])) {
                    ddaten.guiFilme.filtern();
                }
            }
        });

    }

    private void init() {
        initTabs();
        initMenue();
        initToolBar();
        initSearchField();
        DDaten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start_(ListenerFilmeLadenEvent event) {
                //ddaten.infoPanel.setProgress();
                jButtonFilmeLaden.setEnabled(false);
                jMenuItemFilmlisteLaden.setEnabled(false);
            }

            @Override
            public void progress_(ListenerFilmeLadenEvent event) {
                getStatusBar().updateProgressBar(event);
            }

            @Override
            public void fertig_(ListenerFilmeLadenEvent event) {
                getStatusBar().hideProgressIndicators();
                jButtonFilmeLaden.setEnabled(true);
                jMenuItemFilmlisteLaden.setEnabled(true);
                ddaten.allesSpeichern(); // damit nichts verlorengeht
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent evt) {
                beenden();
            }
        });
        jToolBar.addMouseListener(beobMausToolBar);
    }

    private void initTabs() {
        ddaten.mediathekGui = this;
        ddaten.guiFilme = new GuiFilme(ddaten, ddaten.mediathekGui);
        ddaten.guiDownloads = new GuiDownloads(ddaten, ddaten.mediathekGui);
        ddaten.guiAbo = new GuiAbo(ddaten, ddaten.mediathekGui);
        jTabbedPane.addTab("Filme", ddaten.guiFilme);
        jTabbedPane.addTab("Downloads", ddaten.guiDownloads);
        jTabbedPane.addTab("Abos", ddaten.guiAbo);
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                new PanelMeldungen(ddaten, ddaten.mediathekGui, Log.textSystem, ListenerMediathekView.EREIGNIS_LOG_SYSTEM, "Systemmeldungen"),
                new PanelMeldungen(ddaten, ddaten.mediathekGui, Log.textProgramm, ListenerMediathekView.EREIGNIS_LOG_PLAYER, "Meldungen Videoplayer"));
        splitPane.setName("Meldungen");
        splitPane.addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                setToolbar(MediathekGui.UIButtonState.AUS);
                statusBar.setIndexForCenterDisplay(MVStatusBar.StatusbarIndex.FILME);
            }
        });
        if (Daten.debug) {
            jTabbedPane.addTab("Debug", new GuiDebug(ddaten, ddaten.mediathekGui));
            jTabbedPane.addTab("Starts", new PanelInfoStarts(ddaten, ddaten.mediathekGui));
        }
    }

    private void setPanelMeldungen() {
        if (jCheckBoxMenuItemMeldungen.isSelected()) {
            jTabbedPane.add(splitPane, 3);
        } else {
            jTabbedPane.remove(splitPane);
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
        jButtonAboAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent actionEvent) {
                ddaten.guiAbo.aendern();
            }
        });
        jButtonFilterPanel.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = Boolean.TRUE.toString();
                filterAnzeigen(ddaten.guiFilme.isVisible() && !Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getName());
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

    private void setupUserInterfaceForOsx() {
        //OS X specific menu initializations
        Application application = new DefaultApplication();
        application.addApplicationListener(this);
        application.addAboutMenuItem();
        application.addPreferencesMenuItem();
        application.setEnabledAboutMenu(true);
        application.setEnabledPreferencesMenu(true);

        //setup the MediathekView Dock Icon
        try {
            URL url = this.getClass().getResource("res/MediathekView.png");
            BufferedImage appImage = ImageIO.read(url);
            application.setApplicationIconImage(appImage);
        } catch (IOException ex) {
            System.err.println("Application image could not be loaded");
        }

        //Remove all menu items which don´t need to be displayed due to OS X´s native menu support
        if (application.isMac()) {
            //Datei->Beenden
            jMenuDatei.remove(jSeparator2);
            jMenuDatei.remove(jMenuItemBeenden);
            //Datei->Einstellungen
            jMenuDatei.remove(jMenuItemEinstellungen);
            //Hilfe->Über
            jMenuHilfe.remove(jSeparator4);
            jMenuHilfe.remove(jMenuItemAbout);
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
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName());
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ANZAHL_DOWNLOADS, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                initSpinner();
            }
        });
        jMenuDownload.add(jPanelAnzahl, 12);
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
        jCheckBoxMenuItemFilterAnzeigen.setSelected(Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
        jCheckBoxMenuItemFilterAnzeigen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemFilterAnzeigen.isSelected());
                filterAnzeigen(ddaten.guiFilme.isVisible() && !Boolean.parseBoolean(DDaten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getName());
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
        ///
        jCheckBoxMenuItemShutDown.setVisible(DDaten.debug);
        jSeparatorShutDown.setVisible((DDaten.debug));
        jCheckBoxMenuItemShutDown.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                ddaten.nachDownloadShutDown = jCheckBoxMenuItemShutDown.isSelected();
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
                setIcon(jCheckBoxIconKlein.isSelected());
            }
        });
        jCheckBoxMenuItemVideoplayer.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR]));
        jCheckBoxMenuItemVideoplayer.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_VIDEOPLAYER_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemVideoplayer.isSelected());
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_LISTE_PSET, MediathekGui.class.getSimpleName());
            }
        });
        jCheckBoxMenuItemMeldungen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_MELDUNGEN_ANZEIGEN_NR]));
        if (Daten.debug) {
            jCheckBoxMenuItemMeldungen.setSelected(true);
        }
        setPanelMeldungen();
        jCheckBoxMenuItemMeldungen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_MELDUNGEN_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemMeldungen.isSelected());
                setPanelMeldungen();
            }
        });
        // Hilfe
        jMenuItemAnleitung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                DialogOk dialogOk = new DialogOk(ddaten.mediathekGui, true, new PanelHilfe(ddaten, ddaten.mediathekGui), "Hilfe zum Programm");
                dialogOk.setVisible(true);
            }
        });
        // Über
        jMenuItemAbout.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                showAboutDialog();
            }
        });
        setupUserInterfaceForOsx();
    }

    /**
     * Display the About Box
     */
    private void showAboutDialog() {
        MVAboutDialog aboutDialog = new MVAboutDialog(this, SystemInfo.isMacOSX());
        aboutDialog.setVisible(true);
        aboutDialog.dispose();
    }

    @Override
    public void handleQuit(ApplicationEvent event) {
        beenden();
    }

    @Override
    public void handleReOpenApplication(ApplicationEvent event) {
        //unused
    }

    @Override
    public void handlePrintFile(ApplicationEvent event) {
        //unused
    }

    @Override
    public void handlePreferences(ApplicationEvent event) {
        dialogEinstellungen.setVisible(true);
    }

    @Override
    public void handleOpenFile(ApplicationEvent event) {
        //unused
    }

    @Override
    public void handleOpenApplication(ApplicationEvent event) {
        //unused
    }

    @Override
    public void handleAbout(ApplicationEvent event) {
        showAboutDialog();
        event.setHandled(true);
    }

    private void beenden() {
        // Tabelleneinstellungen merken
        ddaten.guiFilme.tabelleSpeichern();
        ddaten.guiDownloads.tabelleSpeichern();
        ddaten.guiAbo.tabelleSpeichern();
        ddaten.listeDownloads.listePutzen();
        if (ddaten.starterClass != null) {
            // alle laufenden Downloads/Programme stoppen
            ddaten.starterClass.delAllStart();
        }
        if (this.getExtendedState() == JFrame.MAXIMIZED_BOTH) {
            Daten.system[Konstanten.SYSTEM_FENSTER_MAX_NR] = Boolean.TRUE.toString();
        } else {
            Daten.system[Konstanten.SYSTEM_FENSTER_MAX_NR] = Boolean.FALSE.toString();
        }
        // Hauptfenster
        Daten.system[Konstanten.SYSTEM_GROESSE_X_NR] = String.valueOf(this.getSize().width);
        Daten.system[Konstanten.SYSTEM_GROESSE_Y_NR] = String.valueOf(this.getSize().height);
        Daten.system[Konstanten.SYSTEM_POS_X_NR] = String.valueOf(this.getLocation().x);
        Daten.system[Konstanten.SYSTEM_POS_Y_NR] = String.valueOf(this.getLocation().y);
        // Dialog Einstellungen
        Daten.system[Konstanten.SYSTEM_GROESSE_EINSTELLUNEN_X_NR] = String.valueOf(dialogEinstellungen.getSize().width);
        Daten.system[Konstanten.SYSTEM_GROESSE_EINSTELLUNEN_Y_NR] = String.valueOf(dialogEinstellungen.getSize().height);
        Daten.system[Konstanten.SYSTEM_POS_EINSTELLUNEN_X_NR] = String.valueOf(dialogEinstellungen.getLocation().x);
        Daten.system[Konstanten.SYSTEM_POS_EINSTELLUNEN_Y_NR] = String.valueOf(dialogEinstellungen.getLocation().y);
        ddaten.allesSpeichern();
        Log.printEndeMeldung();
        this.dispose();
        System.exit(0);
    }

    private void setIcon(boolean klein) {
        Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR] = Boolean.toString(klein);
        jCheckBoxIconKlein.setSelected(klein);
        beobMausToolBar.itemKlein.setSelected(klein);
        if (klein) {
            jButtonFilmeLaden.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/download_16.png")));
            jButtonFilmAbspielen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png")));
            jButtonFilmSpeichern.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/player_rec_16.png")));
            jButtonDownloadAktualisieren.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png")));
            jButtonDownloadAlleStarten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/next_16.png")));
            jButtonDownloadZurueckstellen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/undo_16.png")));
            jButtonDownloadLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jButtonDownloadAufraeumen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png")));
            jButtonAbosLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jButtonAbosEinschalten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/ja_16.png")));
            jButtonAbosAusschalten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/nein_16.png")));
            jButtonAboAendern.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/configure_16.png")));
        } else {
            jButtonFilmeLaden.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/download_32.png")));
            jButtonFilmAbspielen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/player_play_32.png")));
            jButtonFilmSpeichern.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/player_rec_32.png")));
            jButtonDownloadAktualisieren.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png")));
            jButtonDownloadAlleStarten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/next_32.png")));
            jButtonDownloadZurueckstellen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/undo_32.png")));
            jButtonDownloadLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/del_32.png")));
            jButtonDownloadAufraeumen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/edit-clear_32.png")));
            jButtonAbosLoeschen.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/del_32.png")));
            jButtonAbosEinschalten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/ja_32.png")));
            jButtonAbosAusschalten.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/nein_32.png")));
            jButtonAboAendern.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/configure_32.png")));
        }
        this.repaint();
    }

    private void filmeLaden() {
        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(this, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(ddaten, ddaten.mediathekGui, dialog));
            dialog.setVisible(true);
        } else {
            // Filme werden automatisch geladen
            jButtonFilmeLaden.setEnabled(false);
            jMenuItemFilmlisteLaden.setEnabled(false);
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

        jPanel2 = new javax.swing.JPanel();
        jPanel1 = new javax.swing.JPanel();
        jPanelInfo = new javax.swing.JPanel();
        jToolBar = new javax.swing.JToolBar();
        filler3 = new javax.swing.Box.Filler(new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 0), new java.awt.Dimension(5, 32767));
        jButtonFilmeLaden = new javax.swing.JButton();
        javax.swing.Box.Filler filler1 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonFilmAbspielen = new javax.swing.JButton();
        jButtonFilmSpeichern = new javax.swing.JButton();
        javax.swing.Box.Filler filler2 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonDownloadAktualisieren = new javax.swing.JButton();
        jButtonDownloadAlleStarten = new javax.swing.JButton();
        jButtonDownloadZurueckstellen = new javax.swing.JButton();
        jButtonDownloadLoeschen = new javax.swing.JButton();
        jButtonDownloadAufraeumen = new javax.swing.JButton();
        javax.swing.Box.Filler filler4 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
        jButtonAbosEinschalten = new javax.swing.JButton();
        jButtonAbosAusschalten = new javax.swing.JButton();
        jButtonAbosLoeschen = new javax.swing.JButton();
        jButtonAboAendern = new javax.swing.JButton();
        filler7 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 0), new java.awt.Dimension(0, 0), new java.awt.Dimension(32767, 0));
        filler5 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 0), new java.awt.Dimension(0, 0), new java.awt.Dimension(32767, 0));
        filler6 = new javax.swing.Box.Filler(new java.awt.Dimension(0, 0), new java.awt.Dimension(0, 0), new java.awt.Dimension(32767, 0));
        jButtonFilterPanel = new javax.swing.JButton();
        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        filler8 = new javax.swing.Box.Filler(new java.awt.Dimension(2, 0), new java.awt.Dimension(2, 0), new java.awt.Dimension(2, 32767));
        jTabbedPane = new javax.swing.JTabbedPane();
        javax.swing.JMenuBar jMenuBar = new javax.swing.JMenuBar();
        jMenuDatei = new javax.swing.JMenu();
        jMenuItemFilmlisteLaden = new javax.swing.JMenuItem();
        jMenuItemEinstellungen = new javax.swing.JMenuItem();
        jSeparator2 = new javax.swing.JPopupMenu.Separator();
        jMenuItemBeenden = new javax.swing.JMenuItem();
        jMenuFilme = new javax.swing.JMenu();
        jMenuItemFilmAbspielen = new javax.swing.JMenuItem();
        jMenuItemFilmAufzeichnen = new javax.swing.JMenuItem();
        jSeparator5 = new javax.swing.JPopupMenu.Separator();
        jCheckBoxMenuItemFilterAnzeigen = new javax.swing.JCheckBoxMenuItem();
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
        jSeparatorShutDown = new javax.swing.JPopupMenu.Separator();
        jCheckBoxMenuItemShutDown = new javax.swing.JCheckBoxMenuItem();
        jMenuAbos = new javax.swing.JMenu();
        jMenuItemAbosEinschalten = new javax.swing.JMenuItem();
        jMenuItemAbosAusschalten = new javax.swing.JMenuItem();
        jMenuItemAbosLoeschen = new javax.swing.JMenuItem();
        jMenuItemAbosAendern = new javax.swing.JMenuItem();
        jMenuAnsicht = new javax.swing.JMenu();
        jCheckBoxMenuItemToolBar = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxIconKlein = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemVideoplayer = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemMeldungen = new javax.swing.JCheckBoxMenuItem();
        jMenuHilfe = new javax.swing.JMenu();
        jMenuItemAnleitung = new javax.swing.JMenuItem();
        jSeparator4 = new javax.swing.JPopupMenu.Separator();
        jMenuItemAbout = new javax.swing.JMenuItem();

        javax.swing.GroupLayout jPanel2Layout = new javax.swing.GroupLayout(jPanel2);
        jPanel2.setLayout(jPanel2Layout);
        jPanel2Layout.setHorizontalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );
        jPanel2Layout.setVerticalGroup(
            jPanel2Layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGap(0, 100, Short.MAX_VALUE)
        );

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jPanel1.setLayout(new java.awt.BorderLayout());

        jPanelInfo.setLayout(new java.awt.BorderLayout());
        jPanel1.add(jPanelInfo, java.awt.BorderLayout.PAGE_END);

        jToolBar.setBackground(new java.awt.Color(204, 204, 204));
        jToolBar.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        jToolBar.setFloatable(false);
        jToolBar.add(filler3);

        jButtonFilmeLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_32.png"))); // NOI18N
        jButtonFilmeLaden.setToolTipText("neue Filmliste laden");
        jButtonFilmeLaden.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmeLaden.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmeLaden.setMinimumSize(new java.awt.Dimension(50, 60));
        jButtonFilmeLaden.setOpaque(false);
        jButtonFilmeLaden.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmeLaden);
        jToolBar.add(filler1);

        jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_32.png"))); // NOI18N
        jButtonFilmAbspielen.setToolTipText("Film abspielen");
        jButtonFilmAbspielen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmAbspielen.setFocusable(false);
        jButtonFilmAbspielen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmAbspielen.setOpaque(false);
        jButtonFilmAbspielen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmAbspielen);

        jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_32.png"))); // NOI18N
        jButtonFilmSpeichern.setToolTipText("Film aufzeichnen");
        jButtonFilmSpeichern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmSpeichern.setFocusable(false);
        jButtonFilmSpeichern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmSpeichern.setOpaque(false);
        jButtonFilmSpeichern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmSpeichern);
        jToolBar.add(filler2);

        jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png"))); // NOI18N
        jButtonDownloadAktualisieren.setToolTipText("Downloads aktualisieren");
        jButtonDownloadAktualisieren.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAktualisieren.setFocusable(false);
        jButtonDownloadAktualisieren.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAktualisieren.setOpaque(false);
        jButtonDownloadAktualisieren.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAktualisieren);

        jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_32.png"))); // NOI18N
        jButtonDownloadAlleStarten.setToolTipText("alle Downloads starten");
        jButtonDownloadAlleStarten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAlleStarten.setFocusable(false);
        jButtonDownloadAlleStarten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAlleStarten.setOpaque(false);
        jButtonDownloadAlleStarten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAlleStarten);

        jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_32.png"))); // NOI18N
        jButtonDownloadZurueckstellen.setToolTipText("Download zurückstellen");
        jButtonDownloadZurueckstellen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadZurueckstellen.setFocusable(false);
        jButtonDownloadZurueckstellen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadZurueckstellen.setOpaque(false);
        jButtonDownloadZurueckstellen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadZurueckstellen);

        jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonDownloadLoeschen.setToolTipText("Download dauerhaft löschen");
        jButtonDownloadLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadLoeschen.setFocusable(false);
        jButtonDownloadLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadLoeschen.setOpaque(false);
        jButtonDownloadLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadLoeschen);

        jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_32.png"))); // NOI18N
        jButtonDownloadAufraeumen.setToolTipText("Liste der Downloads aufräumen");
        jButtonDownloadAufraeumen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAufraeumen.setFocusable(false);
        jButtonDownloadAufraeumen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAufraeumen.setOpaque(false);
        jButtonDownloadAufraeumen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAufraeumen);
        jToolBar.add(filler4);

        jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_32.png"))); // NOI18N
        jButtonAbosEinschalten.setToolTipText("Abos einschalten");
        jButtonAbosEinschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosEinschalten.setFocusable(false);
        jButtonAbosEinschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosEinschalten.setOpaque(false);
        jButtonAbosEinschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosEinschalten);

        jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_32.png"))); // NOI18N
        jButtonAbosAusschalten.setToolTipText("Abos deaktivieren");
        jButtonAbosAusschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosAusschalten.setFocusable(false);
        jButtonAbosAusschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosAusschalten.setOpaque(false);
        jButtonAbosAusschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosAusschalten);

        jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonAbosLoeschen.setToolTipText("Abos löschen");
        jButtonAbosLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosLoeschen.setFocusable(false);
        jButtonAbosLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosLoeschen.setOpaque(false);
        jButtonAbosLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosLoeschen);

        jButtonAboAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_32.png"))); // NOI18N
        jButtonAboAendern.setToolTipText("Abo ändern");
        jButtonAboAendern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAboAendern.setFocusable(false);
        jButtonAboAendern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAboAendern.setOpaque(false);
        jButtonAboAendern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAboAendern);
        jToolBar.add(filler7);
        jToolBar.add(filler5);
        jToolBar.add(filler6);

        jButtonFilterPanel.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-full_22.png"))); // NOI18N
        jButtonFilterPanel.setToolTipText("Erweiterte Suche");
        jButtonFilterPanel.setBorder(null);
        jButtonFilterPanel.setFocusable(false);
        jButtonFilterPanel.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilterPanel.setMaximumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setMinimumSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setOpaque(false);
        jButtonFilterPanel.setPreferredSize(new java.awt.Dimension(40, 40));
        jButtonFilterPanel.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilterPanel);

        jTextFieldFilter.setBackground(new java.awt.Color(230, 230, 230));
        jTextFieldFilter.setBorder(javax.swing.BorderFactory.createEmptyBorder(1, 1, 1, 1));
        jTextFieldFilter.setToolTipText("Thema/Titel suchen");
        jTextFieldFilter.setDisabledTextColor(new java.awt.Color(102, 102, 102));
        jTextFieldFilter.setMaximumSize(new java.awt.Dimension(300, 35));
        jTextFieldFilter.setName("Thema/Titel"); // NOI18N
        jTextFieldFilter.setPreferredSize(new java.awt.Dimension(300, 25));
        jTextFieldFilter.setPrompt("Thema/Titel");
        jToolBar.add(jTextFieldFilter);
        jToolBar.add(filler8);

        jPanel1.add(jToolBar, java.awt.BorderLayout.PAGE_START);

        jTabbedPane.setBorder(javax.swing.BorderFactory.createEmptyBorder(5, 1, 1, 1));
        jPanel1.add(jTabbedPane, java.awt.BorderLayout.CENTER);

        jMenuDatei.setText("Datei");

        jMenuItemFilmlisteLaden.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F5, 0));
        jMenuItemFilmlisteLaden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/download_16.png"))); // NOI18N
        jMenuItemFilmlisteLaden.setText("neue Filmliste laden");
        jMenuDatei.add(jMenuItemFilmlisteLaden);

        jMenuItemEinstellungen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F4, 0));
        jMenuItemEinstellungen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemEinstellungen.setText("Einstellungen");
        jMenuItemEinstellungen.setToolTipText("allgemeine Programmeinstellungen");
        jMenuDatei.add(jMenuItemEinstellungen);
        jMenuDatei.add(jSeparator2);

        jMenuItemBeenden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/exit_16.png"))); // NOI18N
        jMenuItemBeenden.setText("Beenden");
        jMenuDatei.add(jMenuItemBeenden);

        jMenuBar.add(jMenuDatei);

        jMenuFilme.setText("Filme");

        jMenuItemFilmAbspielen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F6, 0));
        jMenuItemFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png"))); // NOI18N
        jMenuItemFilmAbspielen.setText("Film abspielen");
        jMenuFilme.add(jMenuItemFilmAbspielen);

        jMenuItemFilmAufzeichnen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F7, 0));
        jMenuItemFilmAufzeichnen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_16.png"))); // NOI18N
        jMenuItemFilmAufzeichnen.setText("Film aufzeichnen");
        jMenuFilme.add(jMenuItemFilmAufzeichnen);
        jMenuFilme.add(jSeparator5);

        jCheckBoxMenuItemFilterAnzeigen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F8, 0));
        jCheckBoxMenuItemFilterAnzeigen.setText("Filter anzeigen");
        jMenuFilme.add(jCheckBoxMenuItemFilterAnzeigen);

        jMenuBar.add(jMenuFilme);

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

        jMenuItemDownloadsZurueckstellen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_BACK_SPACE, 0));
        jMenuItemDownloadsZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_16.png"))); // NOI18N
        jMenuItemDownloadsZurueckstellen.setText("Download zurückstellen");
        jMenuDownload.add(jMenuItemDownloadsZurueckstellen);

        jMenuItemDownloadsLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemDownloadsLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jMenuItemDownloadsLoeschen.setText("Download dauerhaft löschen");
        jMenuDownload.add(jMenuItemDownloadsLoeschen);

        jMenuItemDownloadAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemDownloadAendern.setText("Download ändern");
        jMenuDownload.add(jMenuItemDownloadAendern);
        jMenuDownload.add(jSeparator3);

        jMenuItemDownloadsAktualisieren.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_SPACE, 0));
        jMenuItemDownloadsAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png"))); // NOI18N
        jMenuItemDownloadsAktualisieren.setText("Downloads aktualisieren");
        jMenuDownload.add(jMenuItemDownloadsAktualisieren);

        jMenuItemDownloadsAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png"))); // NOI18N
        jMenuItemDownloadsAufraeumen.setText("Downloads aufräumen");
        jMenuDownload.add(jMenuItemDownloadsAufraeumen);
        jMenuDownload.add(jSeparatorShutDown);

        jCheckBoxMenuItemShutDown.setText("anschließend PC herunterfahren");
        jMenuDownload.add(jCheckBoxMenuItemShutDown);

        jMenuBar.add(jMenuDownload);

        jMenuAbos.setText("Abos");

        jMenuItemAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_16.png"))); // NOI18N
        jMenuItemAbosEinschalten.setText("einschalten");
        jMenuAbos.add(jMenuItemAbosEinschalten);

        jMenuItemAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_16.png"))); // NOI18N
        jMenuItemAbosAusschalten.setText("ausschalten");
        jMenuAbos.add(jMenuItemAbosAusschalten);

        jMenuItemAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jMenuItemAbosLoeschen.setText("löschen");
        jMenuAbos.add(jMenuItemAbosLoeschen);

        jMenuItemAbosAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png"))); // NOI18N
        jMenuItemAbosAendern.setText("ändern");
        jMenuAbos.add(jMenuItemAbosAendern);

        jMenuBar.add(jMenuAbos);

        jMenuAnsicht.setText("Ansicht");

        jCheckBoxMenuItemToolBar.setSelected(true);
        jCheckBoxMenuItemToolBar.setText("Toolbar");
        jMenuAnsicht.add(jCheckBoxMenuItemToolBar);

        jCheckBoxIconKlein.setSelected(true);
        jCheckBoxIconKlein.setText("kleine Icons");
        jMenuAnsicht.add(jCheckBoxIconKlein);

        jCheckBoxMenuItemVideoplayer.setText("Buttons anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemVideoplayer);

        jCheckBoxMenuItemMeldungen.setText("Meldungen anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemMeldungen);

        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setText("Hilfe");

        jMenuItemAnleitung.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/agt_support.png"))); // NOI18N
        jMenuItemAnleitung.setText("Hilfe und Fragen zum Programm");
        jMenuHilfe.add(jMenuItemAnleitung);
        jMenuHilfe.add(jSeparator4);

        jMenuItemAbout.setText("Über MediathekView");
        jMenuHilfe.add(jMenuItemAbout);

        jMenuBar.add(jMenuHilfe);

        setJMenuBar(jMenuBar);

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 1083, Short.MAX_VALUE)
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanel1, javax.swing.GroupLayout.DEFAULT_SIZE, 737, Short.MAX_VALUE)
        );

        pack();
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.Box.Filler filler3;
    private javax.swing.Box.Filler filler5;
    private javax.swing.Box.Filler filler6;
    private javax.swing.Box.Filler filler7;
    private javax.swing.Box.Filler filler8;
    private javax.swing.JButton jButtonAboAendern;
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
    private javax.swing.JButton jButtonFilterPanel;
    private javax.swing.JCheckBoxMenuItem jCheckBoxIconKlein;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemFilterAnzeigen;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemMeldungen;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemShutDown;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemToolBar;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemVideoplayer;
    private javax.swing.JMenu jMenuAbos;
    private javax.swing.JMenu jMenuAnsicht;
    private javax.swing.JMenu jMenuDatei;
    private javax.swing.JMenu jMenuDownload;
    private javax.swing.JMenu jMenuFilme;
    private javax.swing.JMenu jMenuHilfe;
    private javax.swing.JMenuItem jMenuItemAbosAendern;
    private javax.swing.JMenuItem jMenuItemAbosAusschalten;
    private javax.swing.JMenuItem jMenuItemAbosEinschalten;
    private javax.swing.JMenuItem jMenuItemAbosLoeschen;
    private javax.swing.JMenuItem jMenuItemAbout;
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
    private javax.swing.JPanel jPanel1;
    private javax.swing.JPanel jPanel2;
    private javax.swing.JPanel jPanelInfo;
    private javax.swing.JPopupMenu.Separator jSeparator1;
    private javax.swing.JPopupMenu.Separator jSeparator2;
    private javax.swing.JPopupMenu.Separator jSeparator3;
    private javax.swing.JPopupMenu.Separator jSeparator4;
    private javax.swing.JPopupMenu.Separator jSeparator5;
    private javax.swing.JPopupMenu.Separator jSeparatorShutDown;
    private javax.swing.JTabbedPane jTabbedPane;
    private org.jdesktop.swingx.JXSearchField jTextFieldFilter;
    private javax.swing.JToolBar jToolBar;
    // End of variables declaration//GEN-END:variables

    class BeobMausToolBar extends MouseAdapter {

        JCheckBoxMenuItem itemKlein = new JCheckBoxMenuItem("kleine Icons");
        JMenuItem itemAusblenden = new JMenuItem("Toolbar ausblenden");

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
                    setIcon(itemKlein.isSelected());
                }
            });
            jPopupMenu.add(itemKlein);
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
