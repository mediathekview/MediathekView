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

import com.jidesoft.utils.SystemInfo;
import java.awt.BorderLayout;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedList;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.DefaultComboBoxModel;
import javax.swing.InputMap;
import javax.swing.JComboBox;
import javax.swing.JComponent;
import javax.swing.JFrame;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.SwingUtilities;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.controller.Log;
import mediathek.controller.MVUsedUrl;
import mediathek.controller.starter.Start;
import mediathek.daten.Daten;
import mediathek.daten.DatenAbo;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenPset;
import mediathek.gui.dialog.DialogBeendenZeit;
import mediathek.gui.dialog.DialogEditAbo;
import mediathek.gui.dialog.DialogEditDownload;
import mediathek.gui.dialog.DialogMediaDB;
import mediathek.gui.dialog.MVFilmInformation;
import mediathek.res.GetIcon;
import mediathek.tool.BeobTableHeader;
import mediathek.tool.CellRendererDownloads;
import mediathek.tool.DirOpenAction;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.MVConfig;
import mediathek.tool.MVFilmSize;
import mediathek.tool.MVMessageDialog;
import mediathek.tool.MVTable;
import mediathek.tool.OpenPlayerAction;
import mediathek.tool.TModelDownload;
import msearch.daten.DatenFilm;
import msearch.filmeSuchen.MSListenerFilmeLaden;
import msearch.filmeSuchen.MSListenerFilmeLadenEvent;
import msearch.tool.Datum;

public class GuiDownloads extends PanelVorlage {

    private final MVFilmInformation filmInfoHud;
    private final PanelFilmBeschreibung panelBeschreibung;
    private long lastUpdate = 0;
    private boolean showAbos = true;
    private boolean showDownloads = true;
    private static final String COMBO_DISPLAY_ALL = "alles";
    private static final String COMBO_DISPLAY_DOWNLOADS_ONLY = "nur Downloads";
    private static final String COMBO_DISPLAY_ABOS_ONLY = "nur Abos";
    /**
     * The internally used model.
     */
    private TModelDownload model;

    public GuiDownloads(Daten d, JFrame parentComponent) {
        super(d, parentComponent);
        initComponents();

        if (SystemInfo.isWindows()) {
            // zum Abfangen der Win-F4 für comboboxen
            InputMap im = cbDisplayCategories.getInputMap();
            im.put(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0), "einstellungen");
            ActionMap am = cbDisplayCategories.getActionMap();
            am.put("einstellungen", new AbstractAction() {

                @Override
                public void actionPerformed(ActionEvent e) {
                    daten.mediathekGui.showDialogPreferences();
                }
            });
        }

        tabelle = new MVTable(MVTable.TABELLE_TAB_DOWNLOADS);
        jScrollPane1.setViewportView(tabelle);
        filmInfoHud = daten.filmInfoHud;

        panelBeschreibung = new PanelFilmBeschreibung(daten.mediathekGui, daten);
        jPanelBeschreibung.add(panelBeschreibung, BorderLayout.CENTER);

        init();
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
        addListenerMediathekView();
        cbDisplayCategories.setModel(getDisplaySelectionModel());
        cbDisplayCategories.addActionListener(new DisplayCategoryListener());

        SwingUtilities.invokeLater(new Runnable() {
            @Override
            public void run() {
                // erst wenn das Programm geladen ist
                downloadsAktualisieren();
            }
        });
    }

    @Override
    public void isShown() {
        super.isShown();
        if (!solo) {
            daten.mediathekGui.setToolbar(MVToolBar.TOOLBAR_TAB_DOWNLOADS);
            daten.mediathekGui.getStatusBar().setIndexForLeftDisplay(MVStatusBar.StatusbarIndex.DOWNLOAD);
        }
        aktFilmSetzen();
    }

    public void aktualisieren() {
        downloadsAktualisieren();
    }

    public void filmAbspielen() {
        filmAbspielen_();
    }

    public void starten(boolean alle) {
        filmStartenWiederholenStoppen(alle, true /* starten */);
    }

    public void startAtTime(boolean alle) {
        filmStartAtTime();
    }

    public void stoppen(boolean alle) {
        filmStartenWiederholenStoppen(alle, false /* starten */);
    }

    public void wartendeStoppen() {
        wartendeDownloadsStoppen();
    }

    public void vorziehen() {
        downloadsVorziehen();
    }

    public void zurueckstellen() {
        downloadLoeschen(false);
    }

    public void loeschen() {
        downloadLoeschen(true);
    }

    public void aufraeumen() {
        downloadsAufraeumen();
    }

    public void aendern() {
        downloadAendern();
    }

    //===================================
    //private
    //===================================
    private void init() {
        //Tabelle einrichten
        ActionMap am = tabelle.getActionMap();
        InputMap im = tabelle.getInputMap();
        im.put(KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0), "aendern");
        am.put("aendern", new AbstractAction() {

            @Override
            public void actionPerformed(ActionEvent e) {
                downloadAendern();
            }
        });

        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_T, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "tabelle");
        this.getActionMap().put("tabelle", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                tabelle.requestFocusSelelct(jScrollPane1);
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_D, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "download");
        this.getActionMap().put("download", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                filmStartenWiederholenStoppen(false, true /* starten */);
            }
        });
        this.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW).put(KeyStroke.getKeyStroke(KeyEvent.VK_I, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()), "info");
        this.getActionMap().put("info", new AbstractAction() {
            @Override
            public void actionPerformed(ActionEvent e) {
                if (!filmInfoHud.isVisible()) {
                    filmInfoHud.show();
                }
            }
        });
        panelBeschreibungSetzen();

        final CellRendererDownloads cellRenderer = new CellRendererDownloads();
        tabelle.setDefaultRenderer(Object.class, cellRenderer);
        tabelle.setDefaultRenderer(Datum.class, cellRenderer);
        tabelle.setDefaultRenderer(MVFilmSize.class, cellRenderer);
        tabelle.setDefaultRenderer(Integer.class, cellRenderer);

        model = new TModelDownload(new Object[][]{}, DatenDownload.COLUMN_NAMES);
        tabelle.setModel(model);
        tabelle.addMouseListener(new BeobMausTabelle());
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        tabelle.getTableHeader().addMouseListener(new BeobTableHeader(tabelle, DatenDownload.COLUMN_NAMES, DatenDownload.spaltenAnzeigen,
                new int[]{DatenDownload.DOWNLOAD_BUTTON_START_NR, DatenDownload.DOWNLOAD_BUTTON_DEL_NR, DatenDownload.DOWNLOAD_REF_NR},
                new int[]{DatenDownload.DOWNLOAD_BUTTON_START_NR, DatenDownload.DOWNLOAD_BUTTON_DEL_NR},
                true /*Icon*/));

        Daten.filmeLaden.addAdListener(new MSListenerFilmeLaden() {
            @Override
            public void fertig(MSListenerFilmeLadenEvent event) {
                Daten.listeDownloads.filmEintragen();
                if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    downloadsAktualisieren();
                } else {
                    reloadTable(); // damit die Filmnummern richtig angezeigt werden
                    // ToDo beim Neuladen ändert sich die Filmnummer aber der Link datenDonwnload.film existiert ja
                    // noch, funktioniert auch damit, stimmt nur die FilmNr nicht
                }
            }
        });
    }

    private void addListenerMediathekView() {
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_BLACKLIST_GEAENDERT, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ABOS_SOFORT_SUCHEN))
                        && Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_BLACKLIST_AUCH_ABO))) {
                    // nur auf Blacklist reagieren, wenn auch für Abos eingeschaltet
                    downloadsAktualisieren();
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{ListenerMediathekView.EREIGNIS_BLACKLIST_AUCH_FUER_ABOS,
            ListenerMediathekView.EREIGNIS_LISTE_ABOS}, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_ABOS_SOFORT_SUCHEN))) {
                    downloadsAktualisieren();
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS,
            ListenerMediathekView.EREIGNIS_REIHENFOLGE_DOWNLOAD, ListenerMediathekView.EREIGNIS_RESET_INTERRUPT}, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                reloadTable();
                daten.allesSpeichern(); // damit nichts verloren geht
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{ListenerMediathekView.EREIGNIS_START_EVENT}, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                Daten.listeDownloads.setModelProgressAlleStart(model);
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_GEO, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(new int[]{ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT}, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (lastUpdate < (new Date().getTime() - 500)) {
                    // nur alle 500ms aufrufen
                    lastUpdate = new Date().getTime();
                    Daten.listeDownloads.setModelProgress(model);
                    // ist ein Kompromiss: beim Sortieren nach Progress wird die Tabelle nicht neu sortiert!
                    //tabelle.fireTableDataChanged(true /*setSpalten*/);
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_PANEL_BESCHREIBUNG_ANZEIGEN, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                panelBeschreibungSetzen();
            }
        });
    }

    private void panelBeschreibungSetzen() {
        jPanelBeschreibung.setVisible(Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_PANEL_BESCHREIBUNG_ANZEIGEN)));
    }

    private synchronized void reloadTable() {
        // nur Downloads die schon in der Liste sind werden geladen
        stopBeob = true;
        tabelle.getSpalten();

        Daten.listeDownloads.getModel(model, showAbos, showDownloads);
        tabelle.setSpalten();
        stopBeob = false;
        aktFilmSetzen();
        setInfo();
    }

    private synchronized void downloadsAktualisieren() {
        // erledigte entfernen, nicht gestartete Abos entfernen und neu nach Abos suchen
        Daten.listeDownloads.abosPutzen();
        Daten.listeDownloads.zurueckgestellteWiederAktivieren();
        Daten.listeDownloads.abosSuchen(parentComponent);
        reloadTable();
        if (Boolean.parseBoolean(Daten.mVConfig.get(MVConfig.SYSTEM_DOWNLOAD_SOFORT_STARTEN))) {
            // und wenn gewollt auch gleich starten
            filmStartenWiederholenStoppen(true /*alle*/, true /*starten*/, false /*fertige wieder starten*/);
        }
    }

    private synchronized void downloadsAufraeumen() {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        Daten.listeDownloads.listePutzen();
    }

    private synchronized void downloadsAufraeumen(DatenDownload datenDownload) {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        Daten.listeDownloads.listePutzen(datenDownload);
    }

    private synchronized void downloadAendern() {
        int row = tabelle.getSelectedRow();
        if (row != -1) {
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
            boolean gestartet = false;
            if (datenDownload.start != null) {
                if (datenDownload.start.status >= Start.STATUS_RUN) {
                    gestartet = true;
                }
            }
            DatenDownload datenDownloadKopy = datenDownload.getCopy();
            DialogEditDownload dialog = new DialogEditDownload(parentComponent, true, datenDownloadKopy, gestartet);
            dialog.setVisible(true);
            if (dialog.ok) {
                datenDownload.aufMichKopieren(datenDownloadKopy);
                reloadTable();
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void downloadsVorziehen() {
        String[] urls;
        // ==========================
        // erst mal die URLs sammeln
        int[] rows = tabelle.getSelectedRows();
        urls = new String[rows.length];
        if (rows.length >= 0) {
            for (int i = 0; i < rows.length; i++) {
                int row = tabelle.convertRowIndexToModel(rows[i]);
                urls[i] = tabelle.getModel().getValueAt(row, DatenDownload.DOWNLOAD_URL_NR).toString();
            }
            Daten.listeDownloads.downloadsVorziehen(urls);
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void zielordnerOeffnen() {
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
            String s = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR];
            DirOpenAction.zielordnerOeffnen(parentComponent, s);
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void filmAbspielen_() {
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
            String s = datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR];
            OpenPlayerAction.filmAbspielen(parentComponent, s);
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void filmLoeschen_() {
        int row = tabelle.getSelectedRow();
        if (row < 0) {
            new HinweisKeineAuswahl().zeigen(parentComponent);
            return;
        }
        DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
        // Download nur löschen wenn er nicht läuft
        if (datenDownload.start != null) {
            if (datenDownload.start.status < Start.STATUS_FERTIG) {
                MVMessageDialog.showMessageDialog(parentComponent, "Download erst stoppen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
        }
        try {
            File file = new File(datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
            if (!file.exists()) {
                MVMessageDialog.showMessageDialog(parentComponent, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
            int ret = JOptionPane.showConfirmDialog(parentComponent,
                    datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR], "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret != JOptionPane.OK_OPTION) {
                return;
            }

            // und jetzt die Datei löschen
            Log.systemMeldung(new String[]{"Datei löschen: ", file.getAbsolutePath()});
            if (!file.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parentComponent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            Log.fehlerMeldung(915236547, "Fehler beim löschen: " + datenDownload.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_DATEINAME_NR]);
        }
    }

    private void downloadLoeschen(boolean dauerhaft) {
        String zeit = new SimpleDateFormat("dd.MM.yyyy").format(new Date());
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            ArrayList<String> arrayUrls = new ArrayList<>();
            LinkedList<MVUsedUrl> urlAboList = new LinkedList<>();
            for (int row : rows) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
                if (dauerhaft) {
                    arrayUrls.add(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                    if (datenDownload.istAbo()) {
                        // ein Abo wird zusätzlich ins Logfile geschrieben
                        urlAboList.add(new MVUsedUrl(zeit,
                                datenDownload.arr[DatenDownload.DOWNLOAD_THEMA_NR],
                                datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                                datenDownload.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]));
                    }
                } else {
                    // wenn nicht dauerhaft
                    datenDownload.zurueckstellen();
                }
            }
            if (!urlAboList.isEmpty()) {
                daten.erledigteAbos.zeilenSchreiben(urlAboList);
            }
            Daten.listeDownloads.downloadLoeschen(arrayUrls);
            reloadTable();
            // ausrichten
            if (tabelle.getRowCount() > 0 && rows.length >= 1) {
                int s = rows[0];
                if (s >= tabelle.getRowCount()) {
                    s = tabelle.getRowCount() - 1;
                    if (s < 0) {
                        s = 0;
                    }
                }
                tabelle.setRowSelectionInterval(s, s);
                tabelle.scrollToCenter(s);
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void filmStartAtTime() {
        // bezieht sich immer auf "alle"
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // wird immer vom Benutzer aufgerufen
        String[] urls;
        ArrayList<String> listeUrlsDownloadLoeschen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();
        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        if (tabelle.getRowCount() == 0) {
            return;
        }
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            // um in der Reihenfolge zu starten
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF_NR);
            Daten.listeDownloads.remove(datenDownload);
            Daten.listeDownloads.add(datenDownload);
        }
        // ==========================
        // erst mal die URLs sammeln
        urls = new String[tabelle.getRowCount()];
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            urls[i] = tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_URL_NR).toString();
        }
        // ========================
        // und jetzt abarbeiten
        for (String url : urls) {
            DatenDownload download = Daten.listeDownloads.getDownloadByUrl(url);
            // ==========================================
            // starten
            if (download.start != null) {
                if (download.start.status == Start.STATUS_RUN) {
                    // dann läuft er schon
                    continue;
                }
                if (download.start.status > Start.STATUS_RUN) {
                    // wenn er noch läuft gibts nix
                    // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                    //TODO in auto dialog umwandeln!
                    int a = JOptionPane.showConfirmDialog(parentComponent, "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL_NR],
                            "Fertiger Download", JOptionPane.YES_NO_OPTION);
                    if (a != JOptionPane.YES_OPTION) {
                        // weiter mit der nächsten URL
                        continue;
                    }
                    listeUrlsDownloadLoeschen.add(url);
                    if (download.istAbo()) {
                        // wenn er schon feritg ist und ein Abos ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                        daten.erledigteAbos.urlAusLogfileLoeschen(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]);
                    }
                }
            }
            listeDownloadsStarten.add(download);
        }
        // ========================
        // jetzt noch die Starts stoppen
        Daten.listeDownloads.downloadAbbrechen(listeUrlsDownloadLoeschen);

        // und die Downloads starten oder stoppen
        //alle Downloads starten/wiederstarten
        DialogBeendenZeit dialogBeenden = new DialogBeendenZeit(daten.mediathekGui, daten, listeDownloadsStarten);
        //dialogBeenden.setModal(true);
        dialogBeenden.setVisible(true);
        if (dialogBeenden.applicationCanTerminate()) {
            // fertig und beenden
            daten.mediathekGui.beenden(false /*Dialog auf "sofort beenden" einstellen*/, dialogBeenden.isShutdownRequested());
        }

        reloadTable();
    }

    private void filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */) {
        filmStartenWiederholenStoppen(alle, starten, true /*auch fertige wieder starten*/);
    }

    private void filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */, boolean fertige /*auch fertige wieder starten*/) {
        // bezieht sich immer auf "alle" oder nur die markierten
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // bei !starten wird der Film gestoppt
        // wird immer vom Benutzer aufgerufen
        String[] urls;
        ArrayList<String> listeUrlsDownloadLoeschen = new ArrayList<>();
        ArrayList<DatenDownload> listeDownloadsStarten = new ArrayList<>();
        // ==========================
        // erst mal die Liste nach der Tabelle sortieren
        if (starten && alle) {
            if (tabelle.getRowCount() == 0) {
                return;
            }
            for (int i = 0; i < tabelle.getRowCount(); ++i) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF_NR);
                Daten.listeDownloads.remove(datenDownload);
                Daten.listeDownloads.add(datenDownload);
            }
        }
        // ==========================
        // erst mal die URLs sammeln
        if (alle) {
            urls = new String[tabelle.getRowCount()];
            for (int i = 0; i < tabelle.getRowCount(); ++i) {
                urls[i] = tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_URL_NR).toString();
            }
        } else {
            int[] rows = tabelle.getSelectedRows();
            urls = new String[rows.length];
            if (rows.length > 0) {
                for (int i = 0; i < rows.length; i++) {
                    int row = tabelle.convertRowIndexToModel(rows[i]);
                    urls[i] = tabelle.getModel().getValueAt(row, DatenDownload.DOWNLOAD_URL_NR).toString();
                }
            } else {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            }
        }
        if (!starten) {
            // dann das Starten von neuen Downloads etwas Pausieren
            daten.starterClass.pause();
        }
        // ========================
        // und jetzt abarbeiten
        int antwort = -1;
        for (String url : urls) {
            DatenDownload download = Daten.listeDownloads.getDownloadByUrl(url);
            if (starten) {
                // ==========================================
                // starten
                if (download.start != null) {
                    if (download.start.status == Start.STATUS_RUN
                            || !fertige && download.start.status > Start.STATUS_RUN) {
                        // wenn er noch läuft gibts nix
                        // fertige bleiben auch unverändert
                        continue;
                    }
                    if (download.start.status > Start.STATUS_RUN) {
                        // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                        //TODO in auto dialog umwandeln!
                        if (antwort == -1) {
                            // nur einmal fragen
                            String text;
                            if (urls.length > 1) {
                                text = "Es sind bereits fertige Filme dabei,\n"
                                        + "diese nochmal starten?";
                            } else {
                                text = "Film nochmal starten?  ==> " + download.arr[DatenDownload.DOWNLOAD_TITEL_NR];
                            }
                            antwort = JOptionPane.showConfirmDialog(parentComponent, text,
                                    "Fertiger Download", JOptionPane.YES_NO_OPTION);
                        }
                        if (antwort != JOptionPane.YES_OPTION) {
                            // weiter mit der nächsten URL
                            continue;
                        }
                        listeUrlsDownloadLoeschen.add(url);
                        if (download.istAbo()) {
                            // wenn er schon feritg ist und ein Abos ist, Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                            daten.erledigteAbos.urlAusLogfileLoeschen(download.arr[DatenDownload.DOWNLOAD_HISTORY_URL_NR]);
                        }
                    }
                }
                listeDownloadsStarten.add(download);
            } else {
                // ==========================================
                // stoppen
                if (download.start != null) {
                    // wenn kein s -> dann gibts auch nichts zum stoppen oder wieder-starten
                    if (download.start.status <= Start.STATUS_RUN) {
                        // löschen -> nur wenn noch läuft, sonst gibts nichts mehr zum löschen
                        listeUrlsDownloadLoeschen.add(url);
                    }
                }
            }
        }
        // ========================
        // jetzt noch die Starts stoppen
        Daten.listeDownloads.downloadAbbrechen(listeUrlsDownloadLoeschen);
        // und die Downloads starten oder stoppen
        if (starten) {
            //alle Downloads starten/wiederstarten
            DatenDownload.startenDownloads(daten, listeDownloadsStarten);
        }
        reloadTable();
    }

    private void wartendeDownloadsStoppen() {
        // es werden alle noch nicht gestarteten Downloads gelöscht
        ArrayList<String> urls = new ArrayList<>();
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF_NR);
            if (datenDownload.start != null) {
                if (datenDownload.start.status < Start.STATUS_RUN) {
                    urls.add(datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR]);
                }
            }
        }
        Daten.listeDownloads.downloadAbbrechen(urls);
    }

    private void setInfo() {
        // Infopanel setzen
        daten.mediathekGui.getStatusBar().setTextForLeftDisplay();
    }

    /**
     * Return the model used for the display categories {@link javax.swing.JComboBox}.
     *
     * @return The selection model.
     */
    private DefaultComboBoxModel<String> getDisplaySelectionModel() {
        return new DefaultComboBoxModel<>(new String[]{COMBO_DISPLAY_ALL, COMBO_DISPLAY_DOWNLOADS_ONLY, COMBO_DISPLAY_ABOS_ONLY});
    }

    private void aktFilmSetzen() {
        if (this.isShowing()) {
            DatenFilm aktFilm = null;
            final int selectedTableRow = tabelle.getSelectedRow();
            if (selectedTableRow >= 0) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(selectedTableRow), DatenDownload.DOWNLOAD_REF_NR);
                if (datenDownload != null) {
                    aktFilm = datenDownload.film;
                }
            }
            filmInfoHud.updateCurrentFilm(aktFilm);
            // Beschreibung setzen
            panelBeschreibung.setAktFilm(aktFilm);
        }
    }

    /**
     * This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel2 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        javax.swing.JTable jTable1 = new javax.swing.JTable();
        javax.swing.JPanel jPanelFilter = new javax.swing.JPanel();
        javax.swing.JLabel jLabel1 = new javax.swing.JLabel();
        cbDisplayCategories = new javax.swing.JComboBox<String>();
        jPanelBeschreibung = new javax.swing.JPanel();

        jLabel2.setText("jLabel2");

        setLayout(new java.awt.BorderLayout());

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        add(jScrollPane1, java.awt.BorderLayout.CENTER);

        java.awt.FlowLayout flowLayout1 = new java.awt.FlowLayout(java.awt.FlowLayout.LEFT);
        flowLayout1.setAlignOnBaseline(true);
        jPanelFilter.setLayout(flowLayout1);

        jLabel1.setText("Anzeigen:");
        jPanelFilter.add(jLabel1);
        jPanelFilter.add(cbDisplayCategories);

        add(jPanelFilter, java.awt.BorderLayout.PAGE_START);

        jPanelBeschreibung.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));
        jPanelBeschreibung.setLayout(new java.awt.BorderLayout());
        add(jPanelBeschreibung, java.awt.BorderLayout.PAGE_END);
    }// </editor-fold>//GEN-END:initComponents

    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JComboBox<String> cbDisplayCategories;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JPanel jPanelBeschreibung;
    private javax.swing.JScrollPane jScrollPane1;
    // End of variables declaration//GEN-END:variables

    private class BeobachterTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                aktFilmSetzen();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private Point p;
        DatenDownload datenDownload = null;

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() == 1) {
                    p = arg0.getPoint();
                    int row = tabelle.rowAtPoint(p);
                    int column = tabelle.columnAtPoint(p);
                    if (row >= 0) {
                        buttonTable(row, column);
                    }
                } else if (arg0.getClickCount() > 1) {
                    downloadAendern();
                }
            }
        }

        @Override
        public void mousePressed(MouseEvent arg0) {
            p = arg0.getPoint();
            int row = tabelle.rowAtPoint(p);
            if (row >= 0) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
            }
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        @Override
        public void mouseReleased(MouseEvent arg0) {
            p = arg0.getPoint();
            int row = tabelle.rowAtPoint(p);
            if (row >= 0) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
            }
            if (arg0.isPopupTrigger()) {
                showMenu(arg0);
            }
        }

        private void buttonTable(int row, int column) {
            if (row != -1) {
                datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
                if (tabelle.convertColumnIndexToModel(column) == DatenDownload.DOWNLOAD_BUTTON_START_NR) {
                    // filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */)
                    if (datenDownload.start != null) {
                        if (datenDownload.start.status == Start.STATUS_FERTIG) {
                            filmAbspielen_();
                        } else if (datenDownload.start.status == Start.STATUS_ERR) {
                            // Download starten
                            filmStartenWiederholenStoppen(false, true /*starten*/);
                        } else {
                            // Download stoppen
                            filmStartenWiederholenStoppen(false, false /*starten*/);
                        }
                    } else {
                        // Download starten
                        filmStartenWiederholenStoppen(false, true /*starten*/);
                    }
                } else if (tabelle.convertColumnIndexToModel(column) == DatenDownload.DOWNLOAD_BUTTON_DEL_NR) {
                    if (datenDownload.start != null) {
                        if (datenDownload.start.status >= Start.STATUS_FERTIG) {
                            downloadsAufraeumen(datenDownload);
                        } else {
                            // Download dauerhaft löschen
                            downloadLoeschen(true);
                        }
                    } else {
                        // Download dauerhaft löschen
                        downloadLoeschen(true);
                    }
                }
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();

            //Film vorziehen
            int row = tabelle.getSelectedRow();
            boolean wartenOderLaufen = false;
            if (row >= 0) {
                DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF_NR);
                if (datenDownload.start != null) {
                    if (datenDownload.start.status <= Start.STATUS_RUN) {
                        wartenOderLaufen = true;
                    }
                }
            }
            // Download starten
            JMenuItem itemStarten = new JMenuItem("Download starten");
            itemStarten.setIcon(GetIcon.getProgramIcon("download_start_16.png"));
            itemStarten.setEnabled(!wartenOderLaufen);
            jPopupMenu.add(itemStarten);
            itemStarten.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(false /* alle */, true /* starten */);
                }
            });

            // Download stoppen
            JMenuItem itemStoppen = new JMenuItem("Download stoppen");
            itemStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
            itemStoppen.setEnabled(wartenOderLaufen);
            jPopupMenu.add(itemStoppen);
            itemStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(false /* alle */, false /* starten */);
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################

            JMenuItem itemVorziehen = new JMenuItem("Download vorziehen");
            itemVorziehen.setIcon(GetIcon.getProgramIcon("move_up_16.png"));
            jPopupMenu.add(itemVorziehen);
            itemVorziehen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadsVorziehen();
                }
            });
            JMenuItem itemLoeschen = new JMenuItem("Download zurückstellen");
            itemLoeschen.setIcon(GetIcon.getProgramIcon("undo_16.png"));
            jPopupMenu.add(itemLoeschen);
            itemLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(false /* dauerhaft */);
                }
            });
            //dauerhaft löschen
            JMenuItem itemDauerhaftLoeschen = new JMenuItem("Download aus Liste entfernen");
            itemDauerhaftLoeschen.setIcon(GetIcon.getProgramIcon("download_del_16.png"));
            jPopupMenu.add(itemDauerhaftLoeschen);
            itemDauerhaftLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(true /* dauerhaft */);
                }
            });
            //Download ändern
            JMenuItem itemAendern = new JMenuItem("Download ändern");
            itemAendern.setIcon(GetIcon.getProgramIcon("configure_16.png"));
            jPopupMenu.add(itemAendern);
            itemAendern.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadAendern();
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################

            JMenuItem itemAlleStarten = new JMenuItem("alle Downloads starten");
            itemAlleStarten.setIcon(GetIcon.getProgramIcon("download_alleStarten_16.png"));
            jPopupMenu.add(itemAlleStarten);
            itemAlleStarten.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(true /* alle */, true /* starten */);
                }
            });
            JMenuItem itemAlleStoppen = new JMenuItem("alle Downloads stoppen");
            itemAlleStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
            jPopupMenu.add(itemAlleStoppen);
            itemAlleStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(true /* alle */, false /* starten */);
                }
            });
            JMenuItem itemWartendeStoppen = new JMenuItem("wartende Downloads stoppen");
            itemWartendeStoppen.setIcon(GetIcon.getProgramIcon("download_stop_16.png"));
            jPopupMenu.add(itemWartendeStoppen);
            itemWartendeStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    wartendeDownloadsStoppen();
                }
            });
            JMenuItem itemAktualisieren = new JMenuItem("Liste der Downloads aktualisieren");
            itemAktualisieren.setIcon(GetIcon.getProgramIcon("view-refresh_16.png"));
            jPopupMenu.add(itemAktualisieren);
            itemAktualisieren.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadsAktualisieren();
                }
            });
            JMenuItem itemAufraeumen = new JMenuItem("Liste aufräumen");
            itemAufraeumen.setIcon(GetIcon.getProgramIcon("download_clear_16.png"));
            jPopupMenu.add(itemAufraeumen);
            itemAufraeumen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadsAufraeumen();
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################

            // Film abspielen
            JMenuItem itemPlayerDownload = new JMenuItem("gespeicherten Film (Datei) abspielen");
            itemPlayerDownload.setIcon(GetIcon.getProgramIcon("film_start_16.png"));

            itemPlayerDownload.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    filmAbspielen_();
                }
            });
            jPopupMenu.add(itemPlayerDownload);
            // Film löschen
            JMenuItem itemDeleteDownload = new JMenuItem("gespeicherten Film (Datei) löschen");
            itemDeleteDownload.setIcon(GetIcon.getProgramIcon("film_del_16.png"));

            itemDeleteDownload.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    filmLoeschen_();
                }
            });
            jPopupMenu.add(itemDeleteDownload);
            // Zielordner öffnen
            JMenuItem itemOeffnen = new JMenuItem("Zielordner öffnen");
            itemOeffnen.setIcon(GetIcon.getProgramIcon("fileopen_16.png"));
            jPopupMenu.add(itemOeffnen);
            itemOeffnen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    zielordnerOeffnen();
                }
            });

            //#######################################
            jPopupMenu.addSeparator();
            //#######################################

            // Film in der MediaDB suchen
            JMenuItem itemDb = new JMenuItem("Film in den gespeicherten Filmen suchen");
            itemDb.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenDownload.DOWNLOAD_REF_NR);
                        if (datenDownload != null) {
                                new DialogMediaDB(parentComponent, datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR]).setVisible(true);
                        }
                    }
                }
            });
            jPopupMenu.add(itemDb);

            // URL abspielen
            JMenuItem itemPlayer = new JMenuItem("Film (URL) abspielen");
            itemPlayer.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        DatenPset gruppe = Daten.listePset.getPsetAbspielen();
                        if (gruppe != null) {
                            DatenDownload datenDownload = (DatenDownload) tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr), DatenDownload.DOWNLOAD_REF_NR);
                            if (datenDownload != null) {
                                if (datenDownload.film != null) {
                                    DatenFilm filmDownload = datenDownload.film.getCopy();
                                    // und jetzt die tatsächlichen URLs des Downloads eintragen
                                    filmDownload.arr[DatenFilm.FILM_URL_NR] = datenDownload.arr[DatenDownload.DOWNLOAD_URL_NR];
                                    filmDownload.arr[DatenFilm.FILM_URL_RTMP_NR] = datenDownload.arr[DatenDownload.DOWNLOAD_URL_RTMP_NR];
                                    filmDownload.arr[DatenFilm.FILM_URL_KLEIN_NR] = "";
                                    filmDownload.arr[DatenFilm.FILM_URL_RTMP_KLEIN_NR] = "";
                                    // und starten
                                    daten.starterClass.urlMitProgrammStarten(gruppe, filmDownload, "" /*Auflösung*/);
                                }
                            }
                        } else {
                            String menuPath;
                            if (SystemInfo.isMacOSX()) {
                                menuPath = "MediathekView->Einstellungen…->Aufzeichnen und Abspielen";
                            } else {
                                menuPath = "Datei->Einstellungen->Aufzeichnen und Abspielen";
                            }
                            MVMessageDialog.showMessageDialog(parentComponent, "Bitte legen Sie im Menü \"" + menuPath + "\" ein Programm zum Abspielen fest.",
                                    "Kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                        }
                    }
                }
            });
            jPopupMenu.add(itemPlayer);

            // URL kopieren
            JMenuItem itemUrl = new JMenuItem("URL kopieren");
            itemUrl.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        GuiFunktionen.copyToClipboard(
                                tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr),
                                        DatenDownload.DOWNLOAD_URL_NR).toString());
                    }
                }
            });
            jPopupMenu.add(itemUrl);

            // Infos
            JMenuItem itemInfo = new JMenuItem("Filminformation anzeigen");
            itemInfo.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    if (!filmInfoHud.isVisible()) {
                        filmInfoHud.show();
                    }
                }
            });
            jPopupMenu.add(itemInfo);

            //Abo ändern
            JMenuItem itemChangeAboFilter = new JMenuItem("Abo ändern");
            if (datenDownload == null) {
                itemChangeAboFilter.setEnabled(false);
            } else if (datenDownload.film == null) {
                itemChangeAboFilter.setEnabled(false);
            } else {
                // dann können wir auch ändern
                final DatenAbo datenAbo = Daten.listeAbo.getAboFuerFilm_schnell(datenDownload.film, false /*die Länge nicht prüfen*/);
                if (datenAbo != null) {
                    itemChangeAboFilter.addActionListener(new ActionListener() {

                        @Override
                        public void actionPerformed(ActionEvent e) {
                            stopBeob = true;
                            DialogEditAbo dialog = new DialogEditAbo(daten.mediathekGui, true, daten, datenAbo);
                            dialog.setVisible(true);
                            if (dialog.ok) {
                                Daten.listeAbo.aenderungMelden();
                            }
                            stopBeob = false;
                        }
                    });
                }
            }
            jPopupMenu.add(itemChangeAboFilter);

            // ######################
            // Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    /**
     * This class filters the shown table items based on the made selection.
     */
    private class DisplayCategoryListener implements ActionListener {

        @Override
        @SuppressWarnings("unchecked")
        public void actionPerformed(ActionEvent e) {
            JComboBox<String> box = (JComboBox<String>) e.getSource();
            final String action = (String) box.getSelectedItem();

            switch (action) {
                case COMBO_DISPLAY_ALL:
                    showAbos = true;
                    showDownloads = true;
                    break;

                case COMBO_DISPLAY_DOWNLOADS_ONLY:
                    showAbos = false;
                    showDownloads = true;
                    break;

                case COMBO_DISPLAY_ABOS_ONLY:
                    showAbos = true;
                    showDownloads = false;
                    break;
            }

            reloadTable();
        }
    }
}
