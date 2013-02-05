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

import java.awt.Component;
import java.awt.Desktop;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import javax.swing.AbstractAction;
import javax.swing.ActionMap;
import javax.swing.InputMap;
import javax.swing.JMenuItem;
import javax.swing.JOptionPane;
import javax.swing.JPopupMenu;
import javax.swing.KeyStroke;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import mediathek.MediathekGui;
import mediathek.controller.filmeLaden.ListenerFilmeLaden;
import mediathek.controller.filmeLaden.ListenerFilmeLadenEvent;
import mediathek.controller.io.starter.Start;
import mediathek.daten.DDaten;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPset;
import mediathek.gui.dialog.DialogDatenFilm;
import mediathek.gui.dialog.DialogEditDownload;
import mediathek.gui.dialog.DialogProgrammOrdnerOeffnen;
import mediathek.tool.BeobMpanel;
import mediathek.tool.CellRendererDownloads;
import mediathek.tool.Datum;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.JTableMed;
import mediathek.tool.Konstanten;
import mediathek.tool.ListenerMediathekView;
import mediathek.tool.Log;
import mediathek.tool.TModelDownload;

public class GuiDownloads extends PanelVorlage {

    private DialogDatenFilm dialogDatenFilm = null;

    public GuiDownloads(DDaten d, Component parentComponent) {
        super(d, parentComponent);
        initComponents();
        tabelle = new JTableMed(JTableMed.TABELLE_TAB_DOWNLOADS);
        jScrollPane1.setViewportView(tabelle);
        dialogDatenFilm = new DialogDatenFilm(null, false, ddaten);
        init();
        downloadsAktualisieren(); // die Tabelle wird dabei gleich geladen
        tabelle.initTabelle();
        if (tabelle.getRowCount() > 0) {
            tabelle.setRowSelectionInterval(0, 0);
        }
    }
    //===================================
    //public
    //===================================

    @Override
    public void isShown() {
        super.isShown();
        ddaten.mediathekGui.setToolbar(MediathekGui.ButtonDonwload);
        ddaten.infoPanel.setIdx(InfoPanel.IDX_GUI_DOWNLOAD);
    }

    public void aktualisieren() {
        downloadsAktualisieren();
    }

    public void starten(boolean alle) {
        filmStartenWiederholenStoppen(alle, true /* starten */);
    }

    public void stoppen(boolean alle) {
        filmStartenWiederholenStoppen(alle, false /* starten */);
    }

    public void vorziehen() {
        downloadVorziehen();
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
        jRadioButtonAbos.setForeground(GuiKonstanten.ABO_FOREGROUND);
        jRadioButtonDownloads.setForeground(GuiKonstanten.DOWNLOAD_FOREGROUND);
        tabelle.setDefaultRenderer(Object.class, new CellRendererDownloads(ddaten));
        tabelle.setDefaultRenderer(Datum.class, new CellRendererDownloads(ddaten));
        tabelle.setModel(new TModelDownload(new Object[][]{}, DatenDownload.DOWNLOAD_COLUMN_NAMES));
        tabelle.addMouseListener(new BeobMausTabelle());
        tabelle.getSelectionModel().addListSelectionListener(new BeobachterTableSelect());
        //aendern
        ActionMap am = tabelle.getActionMap();
        InputMap im = tabelle.getInputMap();
        //aendern
        am.put("aendern", new BeobAbstractActionAendern());
        KeyStroke enter = KeyStroke.getKeyStroke(KeyEvent.VK_ENTER, 0);
        im.put(enter, "aendern");
        //
        jRadioButtonAlles.addActionListener(new BeobAnzeige());
        jRadioButtonAbos.addActionListener(new BeobAnzeige());
        jRadioButtonDownloads.addActionListener(new BeobAnzeige());
        jCheckBoxFilter.addActionListener(new BeobMpanel(jCheckBoxFilter, jPanelFilter, "Filter"));
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_DOWNLOADS, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelleLaden();
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_ART_DOWNLOAD_PROZENT, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
                //panelUpdate();
            }
        });
        DDaten.filmeLaden.addAdListener(new ListenerFilmeLaden() {
            @Override
            public void fertig_(ListenerFilmeLadenEvent event) {
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN_NR])) {
                    downloadsAktualisieren();
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_LISTE_ABOS, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                if (Boolean.parseBoolean(Daten.system[Konstanten.SYSTEM_ABOS_SOFORT_SUCHEN_NR])) {
                    downloadsAktualisieren();
                }
            }
        });
        ListenerMediathekView.addListener(new ListenerMediathekView(ListenerMediathekView.EREIGNIS_START_EVENT, GuiDownloads.class.getSimpleName()) {
            @Override
            public void ping() {
                tabelle.fireTableDataChanged(true /*setSpalten*/);
                setInfo();
            }
        });
    }

    private synchronized void tabelleLaden() {
        // nur Downloads die schon in der Liste sind werden geladen
        boolean abo, download;
        tabelle.getSpalten();
        if (jRadioButtonAlles.isSelected()) {
            abo = true;
            download = true;
        } else if (jRadioButtonAbos.isSelected()) {
            abo = true;
            download = false;
        } else {
            abo = false;
            download = true;
        }
        ddaten.listeDownloads.getModel((TModelDownload) tabelle.getModel(), abo, download);
        tabelle.setSpalten();
        setInfo();
    }

    private synchronized void downloadsAktualisieren() {
        // erledigte entfernen, nicht gestartete Abos entfernen und neu nach Abos suchen
        downloadsAufraeumen();
        ddaten.listeDownloads.zurueckgestellteWiederAktivieren();
        ddaten.listeDownloads.abosLoschenWennNochNichtGestartet();
        ddaten.listeDownloads.abosSuchen();
        tabelleLaden();
    }

    private synchronized void downloadsAufraeumen() {
        // abgeschlossene Downloads werden aus der Tabelle/Liste entfernt
        // die Starts dafür werden auch gelöscht
        ddaten.listeDownloads.listePutzen();
    }

    private synchronized void downloadAendern() {
        int row = tabelle.getSelectedRow();
        if (row != -1) {
            int delRow = tabelle.convertRowIndexToModel(row);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
            DatenDownload d = download.getCopy();
            DialogEditDownload dialog = new DialogEditDownload(null, true, d);
            dialog.setVisible(true);
            if (dialog.ok) {
                download.aufMichKopieren(d);
                tabelle.getSelected();
                tabelleLaden();
                tabelle.setSelected();
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void downloadVorziehen() {
        int row = tabelle.getSelectedRow();
        if (row != -1) {
            int delRow = tabelle.convertRowIndexToModel(row);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            ddaten.listeDownloads.downloadVorziehen(url);
            tabelleLaden();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void zielordnerOeffnen() {
        boolean gut = false;
        File sFile = null;
        int row = tabelle.getSelectedRow();
        if (row >= 0) {
            String url = tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(row), DatenDownload.DOWNLOAD_URL_NR).toString();
            DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
            String s = download.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR];
            if (!s.endsWith(File.separator)) {
                s += File.separator;
            }
            try {
                sFile = new File(s);
                if (!sFile.exists()) {
                    sFile = sFile.getParentFile();
                }
                if (Desktop.isDesktopSupported()) {
                    Desktop d = Desktop.getDesktop();
                    if (d.isSupported(Desktop.Action.OPEN)) {
                        d.open(sFile);
                        gut = true;
                    }
                }
            } catch (Exception ex) {
                try {
                    gut = false;
                    String programm = "";
                    if (Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR].equals("")) {
                        DialogProgrammOrdnerOeffnen dialog = new DialogProgrammOrdnerOeffnen(ddaten.mediathekGui, ddaten, true, "", "Dateimanager suchen");
                        dialog.setVisible(true);
                        if (dialog.ok) {
                            programm = dialog.ziel;
                        }
                    } else {
                        programm = Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR];
                    }
                    if (sFile != null) {
                        Runtime.getRuntime().exec(programm + " " + sFile.getAbsolutePath());
                        Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR] = programm;
                        ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_ORDNER_OEFFNEN, GuiDownloads.class.getSimpleName());
                        gut = true;
                    }
                } catch (Exception eex) {
                    Log.fehlerMeldung(306590789, Log.FEHLER_ART_PROG, GuiDownloads.class.getName(), ex, "Ordner öffnen: " + download.arr[DatenDownload.DOWNLOAD_ZIEL_PFAD_NR]);
                }
            } finally {
                if (!gut) {
                    Daten.system[Konstanten.SYSTEM_ORDNER_OEFFNEN_NR] = "";
                    ListenerMediathekView.notify(ListenerMediathekView.EREIGNIS_PROGRAMM_ORDNER_OEFFNEN, GuiDownloads.class.getSimpleName());
                    JOptionPane.showMessageDialog(parentComponent, "Kann den Dateimanager nicht öffnen!",
                            "Fehler", JOptionPane.ERROR_MESSAGE);
                }
            }
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void downloadLoeschen(boolean dauerhaft) {
        int rows[] = tabelle.getSelectedRows();
        if (rows.length > 0) {
            String[] urls = new String[rows.length];
            for (int i = 0; i < rows.length; ++i) {
                urls[i] = tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(rows[i]), DatenDownload.DOWNLOAD_URL_NR).toString();
            }
            for (String url : urls) {
                DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
                if (dauerhaft) {
                    if (download.istAbo()) {
                        // ein Abo wird zusätzlich ins Logfile geschrieben
                        ddaten.erledigteAbos.zeileSchreiben(download.arr[DatenDownload.DOWNLOAD_THEMA_NR], download.arr[DatenDownload.DOWNLOAD_TITEL_NR], url);
                    }
                    ddaten.listeDownloads.delDownloadByUrl(url);
                } else {
                    // wenn nicht dauerhaft
                    download.zurueckstellen();
                }
                ddaten.starterClass.filmLoeschen(url);
            }
            tabelleLaden();
        } else {
            new HinweisKeineAuswahl().zeigen(parentComponent);
        }
    }

    private void filmStartenWiederholenStoppen(boolean alle, boolean starten /* starten/wiederstarten oder stoppen */) {
        // bezieht sich immer auf "alle" oder nur die markierten
        // Film der noch keinen Starts hat wird gestartet
        // Film dessen Start schon auf fertig/fehler steht wird wieder gestartet
        // bei !starten wird der Film gestoppt
        String[] urls;
        ArrayList<DatenDownload> arrayDownload = new ArrayList<DatenDownload>();
        // ==========================
        // erst mal die URLs sammeln
        if (alle) {
            urls = new String[tabelle.getRowCount()];
            for (int i = 0; i < tabelle.getRowCount(); ++i) {
                urls[i] = tabelle.getModel().getValueAt(i, DatenDownload.DOWNLOAD_URL_NR).toString();
            }
        } else {
            int[] rows = tabelle.getSelectedRows();
            urls = new String[rows.length];
            if (rows.length >= 0) {
                for (int i = 0; i < rows.length; i++) {
                    int row = tabelle.convertRowIndexToModel(rows[i]);
                    urls[i] = tabelle.getModel().getValueAt(row, DatenDownload.DOWNLOAD_URL_NR).toString();
                }
            } else {
                new HinweisKeineAuswahl().zeigen(parentComponent);
            }
        }
        // ========================
        // und jetzt abarbeiten
        for (String url : urls) {
            Start s = ddaten.starterClass.getStart(url);
            DatenDownload download = ddaten.listeDownloads.getDownloadByUrl(url);
            if (starten) {
                // --------------
                // starten
                if (s != null) {
                    if (s.status > Start.STATUS_RUN) {
                        // wenn er noch läuft gibts nix
                        // wenn er schon fertig ist, erst mal fragen vor dem erneuten Starten
                        int a = JOptionPane.showConfirmDialog(parentComponent, "Film nochmal starten?  ==> " + s.datenDownload.arr[DatenDownload.DOWNLOAD_TITEL_NR], "Fertiger Download", JOptionPane.YES_NO_OPTION);
                        if (a != JOptionPane.YES_OPTION) {
                            // weiter mit der nächsten URL
                            continue;
                        }
                        ddaten.starterClass.filmLoeschen(url);
                        if (s.datenDownload.istAbo()) {
                            // bei Abos Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                            ddaten.erledigteAbos.urlAusLogfileLoeschen(url);
                        }
                    }
                }
                arrayDownload.add(download);
            } else {
                // ---------------
                // stoppen
                if (s != null) {
                    // wenn kein s -> dann gibts auch nichts zum stoppen oder wieder-starten
                    if (s.status <= Start.STATUS_RUN) {
                        // löschen -> nur wenn noch läuft, sonst gibts nichts mehr zum löschen
                        ddaten.starterClass.filmLoeschen(url);
                        if (s.datenDownload.istAbo()) {
                            // bei Abos Url auch aus dem Logfile löschen, der Film ist damit wieder auf "Anfang"
                            ddaten.erledigteAbos.urlAusLogfileLoeschen(url);
                        }
                    }
                    arrayDownload.add(download);
                }
            }
        }
        if (starten) {
            //alle Downloads starten/wiederstarten
            DatenDownload.starten(ddaten, arrayDownload);
        } else {
            //oder alle Downloads stoppen
            DatenDownload.statusMelden(arrayDownload, DatenDownload.PROGRESS_NICHT_GESTARTET);
        }
    }

    private void stopWartende() {
        // es werden alle noch nicht gestarteten Downloads gelöscht
        ArrayList<String> urls = new ArrayList<String>();
        for (int i = 0; i < tabelle.getRowCount(); ++i) {
            int delRow = tabelle.convertRowIndexToModel(i);
            String url = tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString();
            Start s = ddaten.starterClass.getStart(url);
            if (s != null) {
                if (s.status < Start.STATUS_RUN) {
                    urls.add(url);
                }
            }
        }
        for (String url : urls) {
            ddaten.starterClass.filmLoeschen(url);
        }
    }

    private void setInfo() {
        String textLinks;
        // Text links: Zeilen Tabelle
        int laufen = ddaten.starterClass.getDownloadsLaufen();
        int warten = ddaten.starterClass.getDownloadsWarten();
        int gesamt = tabelle.getModel().getRowCount();
        if (gesamt == 1) {
            textLinks = "1 Download,";
        } else {
            textLinks = gesamt + " Downloads,";
        }
        textLinks += " (";
        if (laufen == 1) {
            textLinks += "1 läuft,";
        } else {
            textLinks += laufen + " laufen,";
        }
        if (warten == 1) {
            textLinks += " 1 wartet";
        } else {
            textLinks += " " + warten + " warten";
        }
        textLinks += ")";
        // Infopanel setzen
        ddaten.infoPanel.setTextLinks(InfoPanel.IDX_GUI_DOWNLOAD, textLinks);
    }

    private void dialogDatenFilmSetzen() {
        DatenFilm aktFilm = new DatenFilm();
        int selectedTableRow = tabelle.getSelectedRow();
        if (selectedTableRow >= 0) {
            int selectedModelRow = tabelle.convertRowIndexToModel(selectedTableRow);
            DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenDownload.DOWNLOAD_URL_NR).toString());
            if (film != null) {
                aktFilm = film;
            }
        }
        dialogDatenFilm.setAktFilm(aktFilm);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        buttonGroup1 = new javax.swing.ButtonGroup();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jPanelFilter = new javax.swing.JPanel();
        jCheckBoxFilter = new javax.swing.JCheckBox();
        jPanelFilterInnen = new javax.swing.JPanel();
        jRadioButtonAlles = new javax.swing.JRadioButton();
        jRadioButtonDownloads = new javax.swing.JRadioButton();
        jRadioButtonAbos = new javax.swing.JRadioButton();

        jTable1.setAutoCreateRowSorter(true);
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jScrollPane1.setViewportView(jTable1);

        jPanelFilter.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(153, 153, 153)));

        jCheckBoxFilter.setBackground(new java.awt.Color(217, 217, 217));
        jCheckBoxFilter.setFont(new java.awt.Font("Dialog", 1, 10)); // NOI18N
        jCheckBoxFilter.setText("Filter");

        buttonGroup1.add(jRadioButtonAlles);
        jRadioButtonAlles.setSelected(true);
        jRadioButtonAlles.setText(" alle");
        jRadioButtonAlles.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonAlles.setBorderPainted(true);

        buttonGroup1.add(jRadioButtonDownloads);
        jRadioButtonDownloads.setText(" Downloads ");
        jRadioButtonDownloads.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonDownloads.setBorderPainted(true);

        buttonGroup1.add(jRadioButtonAbos);
        jRadioButtonAbos.setText(" Abos ");
        jRadioButtonAbos.setBorder(javax.swing.BorderFactory.createLineBorder(new java.awt.Color(204, 204, 204)));
        jRadioButtonAbos.setBorderPainted(true);

        javax.swing.GroupLayout jPanelFilterInnenLayout = new javax.swing.GroupLayout(jPanelFilterInnen);
        jPanelFilterInnen.setLayout(jPanelFilterInnenLayout);
        jPanelFilterInnenLayout.setHorizontalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jRadioButtonAlles)
                .addGap(18, 18, 18)
                .addComponent(jRadioButtonDownloads)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jRadioButtonAbos)
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        jPanelFilterInnenLayout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jRadioButtonAbos, jRadioButtonAlles, jRadioButtonDownloads});

        jPanelFilterInnenLayout.setVerticalGroup(
            jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterInnenLayout.createSequentialGroup()
                .addContainerGap()
                .addGroup(jPanelFilterInnenLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jRadioButtonAlles)
                    .addComponent(jRadioButtonDownloads)
                    .addComponent(jRadioButtonAbos))
                .addContainerGap(javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
        );

        javax.swing.GroupLayout jPanelFilterLayout = new javax.swing.GroupLayout(jPanelFilter);
        jPanelFilter.setLayout(jPanelFilterLayout);
        jPanelFilterLayout.setHorizontalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
            .addComponent(jCheckBoxFilter, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
        );
        jPanelFilterLayout.setVerticalGroup(
            jPanelFilterLayout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(jPanelFilterLayout.createSequentialGroup()
                .addComponent(jCheckBoxFilter)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jPanelFilterInnen, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
        );

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(this);
        this.setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 670, Short.MAX_VALUE)
                    .addComponent(jPanelFilter, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE))
                .addContainerGap())
        );
        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(javax.swing.GroupLayout.Alignment.TRAILING, layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jPanelFilter, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 357, Short.MAX_VALUE)
                .addContainerGap())
        );
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.ButtonGroup buttonGroup1;
    private javax.swing.JCheckBox jCheckBoxFilter;
    private javax.swing.JPanel jPanelFilter;
    private javax.swing.JPanel jPanelFilterInnen;
    private javax.swing.JRadioButton jRadioButtonAbos;
    private javax.swing.JRadioButton jRadioButtonAlles;
    private javax.swing.JRadioButton jRadioButtonDownloads;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JTable jTable1;
    // End of variables declaration//GEN-END:variables

    private class BeobachterTableSelect implements ListSelectionListener {

        @Override
        public void valueChanged(ListSelectionEvent event) {
            if (!event.getValueIsAdjusting()) {
                dialogDatenFilmSetzen();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private Point p;

        public BeobMausTabelle() {
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
                    downloadAendern();
                }
//            } else if (arg0.getButton() == MouseEvent.BUTTON3) {
//                showMenu(arg0);
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
                int delRow = tabelle.convertRowIndexToModel(row);
                Start s = ddaten.starterClass.getStart(tabelle.getModel().getValueAt(delRow, DatenDownload.DOWNLOAD_URL_NR).toString());
                if (s != null) {
                    if (s.status <= Start.STATUS_RUN) {
                        wartenOderLaufen = true;
                    }
                }
            }

            // Download starten
            JMenuItem itemStarten = new JMenuItem("Download starten");
            itemStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_play_16.png")));
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
            itemStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            itemStoppen.setEnabled(wartenOderLaufen);
            jPopupMenu.add(itemStoppen);
            itemStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(false /* alle */, false /* starten */);
                }
            });

            // Zielordner öffnen
            JMenuItem itemOeffnen = new JMenuItem("Zielordner öffnen");
            itemOeffnen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/fileopen_16.png")));
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

            JMenuItem itemVorziehen = new JMenuItem("Download vorziehen");
            itemVorziehen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/up_blue_16.png")));
            jPopupMenu.add(itemVorziehen);
            itemVorziehen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadVorziehen();
                }
            });
            JMenuItem itemLoeschen = new JMenuItem("Download zurückstellen");
            itemLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/undo_16.png")));
            jPopupMenu.add(itemLoeschen);
            itemLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(false /* dauerhaft */);
                }
            });
            //dauerhaft löschen
            JMenuItem itemDauerhaftLoeschen = new JMenuItem("Download dauerhaft löschen");
            itemDauerhaftLoeschen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/del_16.png")));
            jPopupMenu.add(itemDauerhaftLoeschen);
            itemDauerhaftLoeschen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadLoeschen(true /* dauerhaft */);
                }
            });
            //ändern
            JMenuItem itemAendern = new JMenuItem("Download Ändern");
            itemAendern.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/configure_16.png")));
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
            itemAlleStarten.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/next_16.png")));
            jPopupMenu.add(itemAlleStarten);
            itemAlleStarten.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(true /* alle */, true /* starten */);
                }
            });
            JMenuItem itemAlleStoppen = new JMenuItem("alle Downloads stoppen");
            itemAlleStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            jPopupMenu.add(itemAlleStoppen);
            itemAlleStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    filmStartenWiederholenStoppen(true /* alle */, false /* starten */);
                }
            });
            JMenuItem itemWartendeStoppen = new JMenuItem("wartende Downloads stoppen");
            itemWartendeStoppen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/player_stop_16.png")));
            jPopupMenu.add(itemWartendeStoppen);
            itemWartendeStoppen.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    stopWartende();
                }
            });
            JMenuItem itemAktualisieren = new JMenuItem("Liste der Downloads aktualisieren");
            itemAktualisieren.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/view-refresh_16.png")));
            jPopupMenu.add(itemAktualisieren);
            itemAktualisieren.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent arg0) {
                    downloadsAktualisieren();
                }
            });
            JMenuItem itemAufraeumen = new JMenuItem("Liste Aufräumen");
            itemAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear_16.png")));
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

            // url
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

            // Player
            JMenuItem itemPlayer = new JMenuItem("Film abspielen");
            itemPlayer.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    int nr = tabelle.rowAtPoint(p);
                    if (nr >= 0) {
                        DatenPset gruppe = ddaten.listePset.getPsetAbspielen();
                        if (gruppe != null) {
                            int selectedModelRow = tabelle.convertRowIndexToModel(nr);
                            DatenFilm film = Daten.listeFilme.getFilmByUrl(tabelle.getModel().getValueAt(selectedModelRow, DatenDownload.DOWNLOAD_URL_NR).toString());
                            if (film != null) {
                                // in die History eintragen
                                ddaten.history.add(film.getUrlOrg());
                                // und starten
                                ddaten.starterClass.urlStarten(gruppe, film);
                            }
                        } else {
                            JOptionPane.showMessageDialog(parentComponent, "Im Menü unter \"Datei->Optionen->Videoplayer\" ein Programm zum Abspielen festlegen.",
                                    "kein Videoplayer!", JOptionPane.INFORMATION_MESSAGE);
                        }
                    }
                }
            });
            jPopupMenu.add(itemPlayer);

            // Infos
            JMenuItem itemInfo = new JMenuItem("Infos anzeigen");
            itemInfo.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    dialogDatenFilm.setVis();
                }
            });
            jPopupMenu.add(itemInfo);

            // Tabellenspalten zurücksetzen
            JMenuItem item = new JMenuItem("Spalten zurücksetzen");
            item.addActionListener(new ActionListener() {
                @Override
                public void actionPerformed(ActionEvent e) {
                    tabelle.resetTabelle();
                }
            });
            jPopupMenu.add(item);
            // 
            // ######################
            // Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }

    private class BeobAbstractActionAendern extends AbstractAction {

        @Override
        public void actionPerformed(ActionEvent e) {
            downloadAendern();
        }
    }

    private class BeobAnzeige implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent e) {
            tabelleLaden();
        }
    }
}
