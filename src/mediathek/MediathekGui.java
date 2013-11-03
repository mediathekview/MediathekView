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
import java.awt.AlphaComposite;
import java.awt.BorderLayout;
import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.Frame;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.SplashScreen;
import java.awt.Toolkit;
import java.awt.Window;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.util.ListIterator;
import javax.imageio.ImageIO;
import javax.swing.AbstractAction;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JLabel;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPanel;
import javax.swing.JPopupMenu;
import javax.swing.JSpinner;
import javax.swing.JSplitPane;
import javax.swing.KeyStroke;
import javax.swing.SpinnerNumberModel;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.WindowConstants;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.CheckUpdate;
import mediathek.controller.IoXmlLesen;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
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
import mediathek.res.GetIcon;
import mediathek.tool.Duration;
import mediathek.tool.Filter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.MVMessageDialog;
import msearch.filmeSuchen.MSearchListenerFilmeLaden;
import msearch.filmeSuchen.MSearchListenerFilmeLadenEvent;
import org.jdesktop.swingx.JXSearchField;
import org.simplericity.macify.eawt.Application;
import org.simplericity.macify.eawt.ApplicationEvent;
import org.simplericity.macify.eawt.ApplicationListener;
import org.simplericity.macify.eawt.DefaultApplication;

public final class MediathekGui extends javax.swing.JFrame implements ApplicationListener {

    private Daten daten;
    private BeobMausToolBar beobMausToolBar = new BeobMausToolBar();
    private DialogEinstellungen dialogEinstellungen;
    private JSpinner jSpinnerAnzahl = new JSpinner(new SpinnerNumberModel(1, 1, 9, 1));
    JLabel jLabelAnzahl = new JLabel("Anzahl gleichzeitige Downloads");
    JPanel jPanelAnzahl = new JPanel();
    JSplitPane splitPane = null;
    private MVStatusBar statusBar;
    private Duration duration = new Duration(MediathekGui.class.getSimpleName());

    public enum UIButtonState {

        AUS, FILME, DOWNLOAD, ABO
    }

    /**
     * Legt die statusbar an.
     */
    private void createStatusBar() {
        if (SystemInfo.isMacOSX()) {
            statusBar = new MVStatusBar_Mac();
        } else {
            statusBar = new MVStatusBar_Win_Linux(daten);
        }
        jPanelInfo.add(statusBar.getComponent(), BorderLayout.CENTER);
    }

    public MVStatusBar getStatusBar() {
        return statusBar;
    }

    public String getFilterTextFromSearchField() {
        return jTextFieldFilter.getText();
    }
    /**
     * The JVM {@link java.awt.SplashScreen} storage
     */
    private SplashScreen splash = null;
    /**
     * Store the splash screen {@link Graphics2D} context here for reuse
     */
    private Graphics2D splashScreenContext = null;
    /**
     * helper variable to calculate splash screen progress
     */
    private int splashScreenProgress = 0; //currently 10

    /** Update the {@link java.awt.SplashScreen} with the given text
     *
     * @param text The text which is to be displayed.
     */
    public void updateSplashScreenText(final String text) {
        //bail out when we don´ have a splash screen...
        if (splashScreenContext == null) {
            return;
        }

        final int y = 430;
        final int x = 120;
        final int width = 300;
        final int maxSteps = 11; // KEEP THIS CURRENT!

        splashScreenProgress++;
        //System.out.println("HOW_MANY_STEPS: " + splashScreenProgress);

        splashScreenContext.setRenderingHint(
                RenderingHints.KEY_TEXT_ANTIALIASING,
                RenderingHints.VALUE_TEXT_ANTIALIAS_ON);
        //clear the drawing area...
        splashScreenContext.setComposite(AlphaComposite.Clear);
        splashScreenContext.fillRect(x, (y - 10), width, 40);
        splashScreenContext.setPaintMode();
        //paint the text string...
        splashScreenContext.setFont(new Font("SansSerif", Font.BOLD, 12));
        splashScreenContext.setColor(Color.WHITE);
        splashScreenContext.drawString(text, x, y + 2);
        // paint the full progress indicator...
        splashScreenContext.setColor(Color.BLUE);
        splashScreenContext.fillRect(x, y - 15, width, 5);
        //paint how much is done...
        splashScreenContext.setColor(Color.GREEN);
        splashScreenContext.fillRect(x, y - 15, splashScreenProgress * (width / maxSteps), 5);
        splash.update();
    }

    /**
     * Initialize the Splash Screen variables.
     */
    private void initializeSplashScreen() {
        try {
            splash = SplashScreen.getSplashScreen();
            if (splash != null) {
                splashScreenContext = splash.createGraphics();
            }
        } catch (Exception ex) {
            ex.printStackTrace();
        }
    }

    public MediathekGui(String[] ar) {
        initializeSplashScreen();

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
            for (int i = 0; i < ar.length; ++i) {
                if (ar[i].equalsIgnoreCase(Main.STARTP_MAXIMIERT)) {
                    max = true;
                }
                if (ar[i].equalsIgnoreCase(Main.STARTP_LOGFILE)) {
                    if (ar.length > i) {
                        Log.setLogFile(new File(ar[i + 1]));
                    }
                }
            }
        }
        this.setDefaultCloseOperation(WindowConstants.DO_NOTHING_ON_CLOSE); // soll abgefangen werden
        this.setIconImage(Toolkit.getDefaultToolkit().getImage(MediathekGui.class.getResource("/mediathek/res/MediathekView_k.gif")));

        updateSplashScreenText("Anwendungsdaten laden...");
        duration.ping("Start");

        daten = new Daten(pfad, this);
        duration.ping("Daten");

        Log.startMeldungen(this.getClass().getName());
        createStatusBar();

        //create the Film Information HUD
        daten.filmInfoHud = new MVFilmInformation(this, jTabbedPane, daten);
        duration.ping("HUD");

        if (IoXmlLesen.einstellungenExistieren()) {
            // gibt schon Programmeinstellungen, dann damit starten
            daten.allesLaden();
            updateSplashScreenText("GUI Initialisieren...");
        } else {
            // erster Start
            new DialogStarteinstellungen(null, true, daten).setVisible(true);
        }
        duration.ping("Alles laden");

        setOrgTitel();
        setLookAndFeel();
        duration.ping("LookAndFeel");
        //GuiFunktionen.setLook(this);
        init();
        duration.ping("init");
        setSize(max);
        duration.ping("setSize");

        // Dialog mit den Programmeinstellungen einrichten
        dialogEinstellungen = new DialogEinstellungen(this, daten);

        // Prüfen obs ein Programmupdate gibt
        new CheckUpdate(daten).suchen();
        duration.ping("CheckUpdate");

        if (GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUTO) {
            if (Daten.listeFilme.filmlisteZuAlt()) {
                Log.systemMeldung("Neue Filmliste laden");
                Daten.filmeLaden.importFilmliste("");
            }
        }
        duration.ping("FilmlisteZuAlt");

        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_ORG_TITEL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setOrgTitel();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_PROGRAMM_AKTUELL, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelAllesAktuell();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_MEDIATHEKGUI_UPDATE_VERFUEGBAR, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                setTitelUpdate();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName()) {
            @Override
            public void ping() {
                jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR]));
            }
        });

        this.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, KeyEvent.CTRL_DOWN_MASK), "f");
        this.getRootPane().getActionMap().put("f", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setFocusSuchfeld();
            }
        });
        // für den Mac
        this.getRootPane().getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_F, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "mac-f");
        this.getRootPane().getActionMap().put("mac-f", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                setFocusSuchfeld();
            }
        });
        setFocusSuchfeld();
        duration.ping("Gui steht!");
    }

    private void setFocusSuchfeld() {
        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_SUCHFELD_FOCUS_SETZEN, MediathekGui.class.getName());
        if (!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR])) {
            jTextFieldFilter.requestFocus();
            jTextFieldFilter.setCaretPosition(0);
        }
    }

    /**
     * This will set the Look&Feel based on Application Preferences.
     * In case of error it will always reset to system LAF.
     */
    private void setLookAndFeel() {
        try {
            String laf = Daten.system[Konstanten.SYSTEM_LOOK_NR];
            //if we have the old values, reset to System LAF
            if (laf.equals("") || laf.length() == 1) {
                UIManager.setLookAndFeel(UIManager.getSystemLookAndFeelClassName());
            } else {
                //otherwise set the requested UI
                laf = Daten.system[Konstanten.SYSTEM_LOOK_NR];
                UIManager.setLookAndFeel(laf);
            }
            SwingUtilities.updateComponentTreeUI(this);
            for (Frame f : Frame.getFrames()) {
                SwingUtilities.updateComponentTreeUI(f);
                for (Window w : f.getOwnedWindows()) {
                    SwingUtilities.updateComponentTreeUI(w);
                }
            }
        } catch (Exception ignored) {
            //update the LAF parameter, just in case we tried to load a non-existing LAF before
            Daten.system[Konstanten.SYSTEM_LOOK_NR] = UIManager.getSystemLookAndFeelClassName();
        }
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
                MVMessageDialog.showMessageDialog(this, strMessage, "Arbeitsspeicher", JOptionPane.WARNING_MESSAGE);
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
                jButtonInfo.setEnabled(true);
                jButtonFilmSpeichern.setEnabled(true);
                jMenuItemFilmAbspielen.setEnabled(true);
                jMenuItemFilmAufzeichnen.setEnabled(true);
                jCheckBoxMenuItemFilterAnzeigen.setEnabled(true);
                filterAnzeigen(!Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                break;
            case DOWNLOAD:
                buttonAus();
                jButtonInfo.setEnabled(true);
                jButtonFilmeLaden.setEnabled(true);
                jButtonDownloadAktualisieren.setEnabled(true);
                jButtonDownloadAlleStarten.setEnabled(true);
                jButtonDownloadZurueckstellen.setEnabled(true);
                jButtonDownloadLoeschen.setEnabled(true);
                jButtonDownloadAufraeumen.setEnabled(true);
                jMenuItemDownloadStarten.setEnabled(true);
                jMenuItemDownloadStoppen.setEnabled(true);
                jMenuItemDownloadAlleStoppen.setEnabled(true);
                jMenuItemDownloadWartendeStoppen.setEnabled(true);
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
        jCheckBoxMenuItemFilterAnzeigen.setSelected(!anz);
    }

    public void videoplayerAnzeigen(boolean anz) {
        jCheckBoxMenuItemVideoplayer.setSelected(!anz);
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
        jButtonInfo.setEnabled(false);
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
        jMenuItemDownloadWartendeStoppen.setEnabled(false);
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
            int breite, hoehe, posX, posY, divider;
            try {
                breite = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_X_NR]);
                hoehe = Integer.parseInt(Daten.system[Konstanten.SYSTEM_GROESSE_Y_NR]);
                posX = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_X_NR]);
                posY = Integer.parseInt(Daten.system[Konstanten.SYSTEM_POS_Y_NR]);
                divider = Integer.parseInt(Daten.system[Konstanten.SYSTEM_BREITE_MELDUNGEN_NR]);
            } catch (NumberFormatException ex) {
                breite = 0;
                hoehe = 0;
                posX = 0;
                posY = 0;
                divider = 0;
            }
            if (breite > 0 && hoehe > 0) {
                this.setSize(new Dimension(breite, hoehe));
                this.setLocation(posX, posY);
            }
            if (divider > 0) {
                splitPane.setDividerLocation(divider);
            }
        }
    }

    /**
     * Initialisiert das SearchField
     */
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

    private void init() {
        jButtonFilmeLaden.setIcon(GetIcon.getIcon("download_32.png"));
        jButtonFilmAbspielen.setIcon(GetIcon.getIcon("player_play_32.png"));
        jButtonInfo.setIcon(GetIcon.getIcon("info_32.png"));
        jButtonFilmSpeichern.setIcon(GetIcon.getIcon("player_rec_32.png"));
        jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_32.png"));
        jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("alle_starten_32.png"));
        jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_32.png"));
        jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
        jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("clear_32.png"));
        jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_32.png"));
        jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_32.png"));
        jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
        jButtonAboAendern.setIcon(GetIcon.getIcon("configure_32.png"));
        jButtonFilterPanel.setIcon(GetIcon.getIcon("filter_anzeigen_22.png"));
        jMenuItemFilmlisteLaden.setIcon(GetIcon.getIcon("download_16.png"));
        jMenuItemEinstellungen.setIcon(GetIcon.getIcon("configure_16.png"));
        jMenuItemBeenden.setIcon(GetIcon.getIcon("beenden_16.png"));
        jMenuItemFilmAbspielen.setIcon(GetIcon.getIcon("player_play_16.png"));
        jMenuItemFilmAufzeichnen.setIcon(GetIcon.getIcon("player_rec_16.png"));
        jMenuItemDownloadsAlleStarten.setIcon(GetIcon.getIcon("alle_starten_16.png"));
        jMenuItemDownloadAlleStoppen.setIcon(GetIcon.getIcon("player_stop_16.png"));
        jMenuItemDownloadWartendeStoppen.setIcon(GetIcon.getIcon("player_stop_16.png"));
        jMenuItemDownloadStarten.setIcon(GetIcon.getIcon("player_play_16.png"));
        jMenuItemDownloadStoppen.setIcon(GetIcon.getIcon("player_stop_16.png"));
        jMenuItemDownloadVorziehen.setIcon(GetIcon.getIcon("move_up_16.png"));
        jMenuItemDownloadsZurueckstellen.setIcon(GetIcon.getIcon("undo_16.png"));
        jMenuItemDownloadsLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
        jMenuItemDownloadAendern.setIcon(GetIcon.getIcon("configure_16.png"));
        jMenuItemDownloadsAktualisieren.setIcon(GetIcon.getIcon("view-refresh_16.png"));
        jMenuItemDownloadsAufraeumen.setIcon(GetIcon.getIcon("clear_16.png"));
        jMenuItemAbosEinschalten.setIcon(GetIcon.getIcon("ja_16.png"));
        jMenuItemAbosAusschalten.setIcon(GetIcon.getIcon("nein_16.png"));
        jMenuItemAbosLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
        jMenuItemAbosAendern.setIcon(GetIcon.getIcon("configure_16.png"));
        jMenuItemAnleitung.setIcon(GetIcon.getIcon("help_16.png"));
        initTabs();
        initMenue();
        initToolBar();
        initSearchField();
        Daten.filmeLaden.addAdListener(new MSearchListenerFilmeLaden() {
            @Override
            public void start(MSearchListenerFilmeLadenEvent event) {
                //ddaten.infoPanel.setProgress();
                jButtonFilmeLaden.setEnabled(false);
                jMenuItemFilmlisteLaden.setEnabled(false);
            }

            @Override
            public void progress(MSearchListenerFilmeLadenEvent event) {
                getStatusBar().updateProgressBar(event);
            }

            @Override
            public void fertig(MSearchListenerFilmeLadenEvent event) {
                getStatusBar().hideProgressIndicators();
                jButtonFilmeLaden.setEnabled(true);
                jMenuItemFilmlisteLaden.setEnabled(true);
                daten.allesSpeichern(); // damit nichts verlorengeht
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
        daten.guiFilme = new GuiFilme(daten, daten.mediathekGui);
        daten.guiDownloads = new GuiDownloads(daten, daten.mediathekGui);
        daten.guiAbo = new GuiAbo(daten, daten.mediathekGui);
//        MVJFrame frameFilme = new MVJFrame(daten.guiFilme);
//        frameFilme.setVisible(true);
//        MVJFrame frameDownloads = new MVJFrame(daten.guiDownloads);
//        frameDownloads.setVisible(true);
//        MVJFrame frameAbos = new MVJFrame(daten.guiAbo);
//        frameAbos.setVisible(true);

        jTabbedPane.addTab("Filme", daten.guiFilme);
        jTabbedPane.addTab("Downloads", daten.guiDownloads);
        jTabbedPane.addTab("Abos", daten.guiAbo);
        splitPane = new JSplitPane(JSplitPane.HORIZONTAL_SPLIT,
                new PanelMeldungen(daten, daten.mediathekGui, Log.textSystem, ListenerMediathekView.EREIGNIS_LOG_SYSTEM, "Systemmeldungen"),
                new PanelMeldungen(daten, daten.mediathekGui, Log.textProgramm, ListenerMediathekView.EREIGNIS_LOG_PLAYER, "Meldungen Hilfsprogramme"));
        splitPane.setName("Meldungen");
        splitPane.addComponentListener(new java.awt.event.ComponentAdapter() {
            @Override
            public void componentShown(java.awt.event.ComponentEvent evt) {
                setToolbar(MediathekGui.UIButtonState.AUS);
                statusBar.setIndexForCenterDisplay(MVStatusBar_Mac.StatusbarIndex.FILME);
            }
        });
        if (Daten.debug) {
            jTabbedPane.addTab("Debug", new GuiDebug(daten, daten.mediathekGui));
            jTabbedPane.addTab("Starts", new PanelInfoStarts(daten, daten.mediathekGui));
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
        jLabelAnzahl.setIcon(GetIcon.getIcon("up_down_16.png"));
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
                filmeLaden(false);
            }
        });
        jMenuItemFilmAbspielen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.filmAbspielen();
            }
        });
        jMenuItemFilmAufzeichnen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiFilme.filmSpeichern();
            }
        });
        jCheckBoxMenuItemFilterAnzeigen.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
        jCheckBoxMenuItemFilterAnzeigen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemFilterAnzeigen.isSelected());
                filterAnzeigen(daten.guiFilme.isVisible() && !Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_FILTER_ANZEIGEN_NR]));
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_FILTER_ANZEIGEN, MediathekGui.class.getName());
            }
        });

        // Downloads
        jMenuItemDownloadsAktualisieren.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aktualisieren();
            }
        });
        jMenuItemDownloadsAufraeumen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aufraeumen();
            }
        });
        jMenuItemDownloadsLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.loeschen();
            }
        });
        jMenuItemDownloadsAlleStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.starten(true /* alle */);
            }
        });
        jMenuItemDownloadStarten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.starten(false /* alle */);
            }
        });
        jMenuItemDownloadsZurueckstellen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.zurueckstellen();
            }
        });
        jMenuItemDownloadVorziehen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.vorziehen();
            }
        });
        jMenuItemDownloadAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.aendern();
            }
        });
        jMenuItemDownloadAlleStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.stoppen(true /* alle */);
            }
        });
        jMenuItemDownloadWartendeStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.wartendeStoppen();
            }
        });
        jMenuItemDownloadStoppen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiDownloads.stoppen(false /* alle */);
            }
        });
        ///
        jCheckBoxMenuItemShutDown.setVisible(Daten.debug);
        jSeparatorShutDown.setVisible((Daten.debug));
        jCheckBoxMenuItemShutDown.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.nachDownloadShutDown = jCheckBoxMenuItemShutDown.isSelected();
            }
        });

        // Abo
        jMenuItemAbosEinschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(true);
            }
        });
        jMenuItemAbosAusschalten.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.einAus(false);
            }
        });
        jMenuItemAbosLoeschen.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.loeschen();
            }
        });
        jMenuItemAbosAendern.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                daten.guiAbo.aendern();
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
        jCheckBoxMenuItemBeschreibung.setSelected(Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR]));
        jCheckBoxMenuItemBeschreibung.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                Daten.system[Konstanten.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN_NR] = String.valueOf(jCheckBoxMenuItemBeschreibung.isSelected());
                ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, MediathekGui.class.getSimpleName());
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
                DialogOk dialogOk = new DialogOk(daten.mediathekGui, true, new PanelHilfe(daten, daten.mediathekGui), "Hilfe zum Programm");
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

    public void beenden() {
        if (Daten.listeDownloads.nochNichtFertigeDownloads()) {
            // erst mal prüfen ob noch Downloads laufen
            int ret = JOptionPane.showConfirmDialog(this, "Laufende Downloads abbrechen?", "Abbrechen?", JOptionPane.YES_NO_OPTION);
            if (ret != JOptionPane.OK_OPTION) {
                return;
            }
        }
        // Tabelleneinstellungen merken
        daten.guiFilme.tabelleSpeichern();
        daten.guiDownloads.tabelleSpeichern();
        daten.guiAbo.tabelleSpeichern();
        if (Daten.listeDownloads != null) {
            // alle laufenden Downloads/Programme stoppen
            ListIterator<DatenDownload> it = Daten.listeDownloads.listIterator();
            while (it.hasNext()) {
                Start s = it.next().start;
                if (s != null) {
                    s.stoppen = true;
                }
            }
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
        Daten.system[Konstanten.SYSTEM_BREITE_MELDUNGEN_NR] = String.valueOf(splitPane.getDividerLocation());
        daten.allesSpeichern();
        Log.printEndeMeldung();
        this.dispose();
        System.exit(0);
    }

    private void setIcon(boolean klein) {
        Daten.system[Konstanten.SYSTEM_ICON_KLEIN_NR] = Boolean.toString(klein);
        jCheckBoxIconKlein.setSelected(klein);
        beobMausToolBar.itemKlein.setSelected(klein);
        if (klein) {
            jButtonFilmeLaden.setIcon(GetIcon.getIcon("download_16.png"));
            jButtonFilmAbspielen.setIcon(GetIcon.getIcon("player_play_16.png"));
            jButtonInfo.setIcon(GetIcon.getIcon("info_16.png"));
            jButtonFilmSpeichern.setIcon(GetIcon.getIcon("player_rec_16.png"));
            jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_16.png"));
            jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("alle_starten_16.png"));
            jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_16.png"));
            jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
            jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("clear_16.png"));
            jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_16.png"));
            jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_16.png"));
            jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_16.png"));
            jButtonAboAendern.setIcon(GetIcon.getIcon("configure_16.png"));
        } else {
            jButtonFilmeLaden.setIcon(GetIcon.getIcon("download_32.png"));
            jButtonFilmAbspielen.setIcon(GetIcon.getIcon("player_play_32.png"));
            jButtonInfo.setIcon(GetIcon.getIcon("info_32.png"));
            jButtonFilmSpeichern.setIcon(GetIcon.getIcon("player_rec_32.png"));
            jButtonDownloadAktualisieren.setIcon(GetIcon.getIcon("view-refresh_32.png"));
            jButtonDownloadAlleStarten.setIcon(GetIcon.getIcon("alle_starten_32.png"));
            jButtonDownloadZurueckstellen.setIcon(GetIcon.getIcon("undo_32.png"));
            jButtonDownloadLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
            jButtonDownloadAufraeumen.setIcon(GetIcon.getIcon("clear_32.png"));
            jButtonAbosLoeschen.setIcon(GetIcon.getIcon("del_32.png"));
            jButtonAbosEinschalten.setIcon(GetIcon.getIcon("ja_32.png"));
            jButtonAbosAusschalten.setIcon(GetIcon.getIcon("nein_32.png"));
            jButtonAboAendern.setIcon(GetIcon.getIcon("configure_32.png"));
        }
        this.repaint();
    }

    private void filmeLaden(boolean manuell) {
        if (manuell || GuiFunktionen.getImportArtFilme() == GuiKonstanten.UPDATE_FILME_AUS) {
            // Dialog zum Laden der Filme anzeigen
            DialogLeer dialog = new DialogLeer(this, true);
            dialog.init("Einstellungen zum Laden der Filme", new PanelFilmlisteLaden(daten, daten.mediathekGui, dialog));
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
        jButtonInfo = new javax.swing.JButton();
        filler6 = new javax.swing.Box.Filler(new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 0), new java.awt.Dimension(10, 32767));
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
        filler5 = new javax.swing.Box.Filler(new java.awt.Dimension(1, 5), new java.awt.Dimension(1, 5), new java.awt.Dimension(32767, 5));
        jButtonFilterPanel = new javax.swing.JButton();
        jTextFieldFilter = new org.jdesktop.swingx.JXSearchField();
        filler8 = new javax.swing.Box.Filler(new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5), new java.awt.Dimension(4, 5));
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
        jMenuDownload = new javax.swing.JMenu();
        jMenuItemDownloadsAlleStarten = new javax.swing.JMenuItem();
        jMenuItemDownloadWartendeStoppen = new javax.swing.JMenuItem();
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
        jCheckBoxMenuItemFilterAnzeigen = new javax.swing.JCheckBoxMenuItem();
        jCheckBoxMenuItemBeschreibung = new javax.swing.JCheckBoxMenuItem();
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

        setDefaultCloseOperation(javax.swing.WindowConstants.DO_NOTHING_ON_CLOSE);

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

        jButtonInfo.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/info_32.png"))); // NOI18N
        jButtonInfo.setToolTipText("Infos anzeigen");
        jButtonInfo.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonInfo.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonInfo.setOpaque(false);
        jButtonInfo.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonInfo);
        jToolBar.add(filler6);

        jButtonFilmAbspielen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_32.png"))); // NOI18N
        jButtonFilmAbspielen.setToolTipText("Film abspielen");
        jButtonFilmAbspielen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmAbspielen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmAbspielen.setOpaque(false);
        jButtonFilmAbspielen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmAbspielen);

        jButtonFilmSpeichern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_rec_32.png"))); // NOI18N
        jButtonFilmSpeichern.setToolTipText("Film aufzeichnen");
        jButtonFilmSpeichern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonFilmSpeichern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonFilmSpeichern.setOpaque(false);
        jButtonFilmSpeichern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonFilmSpeichern);
        jToolBar.add(filler2);

        jButtonDownloadAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_32.png"))); // NOI18N
        jButtonDownloadAktualisieren.setToolTipText("Downloads aktualisieren");
        jButtonDownloadAktualisieren.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAktualisieren.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAktualisieren.setOpaque(false);
        jButtonDownloadAktualisieren.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAktualisieren);

        jButtonDownloadAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/alle_starten_32.png"))); // NOI18N
        jButtonDownloadAlleStarten.setToolTipText("alle Downloads starten");
        jButtonDownloadAlleStarten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAlleStarten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAlleStarten.setOpaque(false);
        jButtonDownloadAlleStarten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAlleStarten);

        jButtonDownloadZurueckstellen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_32.png"))); // NOI18N
        jButtonDownloadZurueckstellen.setToolTipText("Download zurückstellen");
        jButtonDownloadZurueckstellen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadZurueckstellen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadZurueckstellen.setOpaque(false);
        jButtonDownloadZurueckstellen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadZurueckstellen);

        jButtonDownloadLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonDownloadLoeschen.setToolTipText("Download dauerhaft löschen");
        jButtonDownloadLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadLoeschen.setOpaque(false);
        jButtonDownloadLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadLoeschen);

        jButtonDownloadAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/clear_32.png"))); // NOI18N
        jButtonDownloadAufraeumen.setToolTipText("Liste der Downloads aufräumen");
        jButtonDownloadAufraeumen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonDownloadAufraeumen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonDownloadAufraeumen.setOpaque(false);
        jButtonDownloadAufraeumen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonDownloadAufraeumen);
        jToolBar.add(filler4);

        jButtonAbosEinschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/ja_32.png"))); // NOI18N
        jButtonAbosEinschalten.setToolTipText("Abos einschalten");
        jButtonAbosEinschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosEinschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosEinschalten.setOpaque(false);
        jButtonAbosEinschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosEinschalten);

        jButtonAbosAusschalten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/nein_32.png"))); // NOI18N
        jButtonAbosAusschalten.setToolTipText("Abos deaktivieren");
        jButtonAbosAusschalten.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosAusschalten.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosAusschalten.setOpaque(false);
        jButtonAbosAusschalten.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosAusschalten);

        jButtonAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_32.png"))); // NOI18N
        jButtonAbosLoeschen.setToolTipText("Abos löschen");
        jButtonAbosLoeschen.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAbosLoeschen.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAbosLoeschen.setOpaque(false);
        jButtonAbosLoeschen.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAbosLoeschen);

        jButtonAboAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_32.png"))); // NOI18N
        jButtonAboAendern.setToolTipText("Abo ändern");
        jButtonAboAendern.setBorder(javax.swing.BorderFactory.createEmptyBorder(8, 8, 8, 8));
        jButtonAboAendern.setHorizontalTextPosition(javax.swing.SwingConstants.CENTER);
        jButtonAboAendern.setOpaque(false);
        jButtonAboAendern.setVerticalTextPosition(javax.swing.SwingConstants.BOTTOM);
        jToolBar.add(jButtonAboAendern);
        jToolBar.add(filler5);

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

        jMenuItemBeenden.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/beenden_16.png"))); // NOI18N
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

        jMenuBar.add(jMenuFilme);

        jMenuDownload.setText("Downloads");

        jMenuItemDownloadsAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/alle_starten_16.png"))); // NOI18N
        jMenuItemDownloadsAlleStarten.setText("alle Downloads starten");
        jMenuDownload.add(jMenuItemDownloadsAlleStarten);

        jMenuItemDownloadWartendeStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png"))); // NOI18N
        jMenuItemDownloadWartendeStoppen.setText("wartende stoppen");
        jMenuItemDownloadWartendeStoppen.setToolTipText("wartende Downloads stoppen");
        jMenuDownload.add(jMenuItemDownloadWartendeStoppen);

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

        jMenuItemDownloadVorziehen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/move_up_16.png"))); // NOI18N
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

        jMenuItemDownloadsAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/clear_16.png"))); // NOI18N
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

        jMenuItemAbosLoeschen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_DELETE, 0));
        jMenuItemAbosLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png"))); // NOI18N
        jMenuItemAbosLoeschen.setText("löschen");
        jMenuAbos.add(jMenuItemAbosLoeschen);

        jMenuItemAbosAendern.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_ENTER, 0));
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

        jCheckBoxMenuItemFilterAnzeigen.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F8, 0));
        jCheckBoxMenuItemFilterAnzeigen.setText("Filter anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemFilterAnzeigen);

        jCheckBoxMenuItemBeschreibung.setAccelerator(javax.swing.KeyStroke.getKeyStroke(java.awt.event.KeyEvent.VK_F9, 0));
        jCheckBoxMenuItemBeschreibung.setText("Beschreibung anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemBeschreibung);

        jCheckBoxMenuItemVideoplayer.setText("Buttons anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemVideoplayer);

        jCheckBoxMenuItemMeldungen.setText("Meldungen anzeigen");
        jMenuAnsicht.add(jCheckBoxMenuItemMeldungen);

        jMenuBar.add(jMenuAnsicht);

        jMenuHilfe.setText("Hilfe");

        jMenuItemAnleitung.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/help_16.png"))); // NOI18N
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
    private javax.swing.JButton jButtonInfo;
    private javax.swing.JCheckBoxMenuItem jCheckBoxIconKlein;
    private javax.swing.JCheckBoxMenuItem jCheckBoxMenuItemBeschreibung;
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
    private javax.swing.JMenuItem jMenuItemDownloadWartendeStoppen;
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
