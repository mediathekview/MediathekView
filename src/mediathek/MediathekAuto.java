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

import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import javax.swing.*;
import javax.swing.event.ChangeEvent;
import javax.swing.event.ChangeListener;
import mediathek.controller.filme.BeobFilmeLaden;
import mediathek.controller.io.starter.ListeStarts;
import mediathek.controller.io.starter.StartEvent;
import mediathek.controller.io.starter.StartListener;
import mediathek.controller.io.starter.Starts;
import mediathek.daten.DDaten;
import mediathek.daten.DatenFilm;
import mediathek.daten.DatenPgruppe;
import mediathek.gui.OSXAdapter;
import mediathek.gui.beobachter.CellRendererFilme;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.GuiKonstanten;
import mediathek.tool.HinweisKeineAuswahl;
import mediathek.tool.TModel;

public class MediathekAuto extends javax.swing.JFrame {

    private DDaten daten;
    private boolean podsLadenJetzt = false; // Download starten
    private boolean abosLadenJetzt = false; // Download starten
    private boolean keinePods = false; // gibt nix zum Laden
    private boolean keineAbos = false; // gibt nix zum Laden
    private TModel tModel;
    // Check that we are on Mac OS X.  This is crucial to loading and using the OSXAdapter class.
    public static boolean MAC_OS_X = (System.getProperty("os.name").toLowerCase().startsWith("mac os x"));

    public MediathekAuto(String[] ar) {
        boolean debug = false;
        String pfad = "";
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
        }
        jProgressDownloads.setForeground(new java.awt.Color(102, 153, 255));
        daten = new DDaten(pfad);
////////        daten.auto = true;
//////////        daten.allesLaden = false;
        this.setTitle(Konstanten.PROGRAMMNAME + "-Auto");
        if (daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR].equals("")) {
            jSpinnerDownload.setValue(1);
            daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] = "1";
        } else {
            jSpinnerDownload.setValue(Integer.parseInt(daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR]));
        }
//        daten.fehlerFensterAnzeigen = false; //in diesem Modus keine Fehler anzeigen
        if (daten.system[Konstanten.SYSTEM_LOOK_NR].equals("")) {
            daten.system[Konstanten.SYSTEM_LOOK_NR] = "0";
        }
        GuiFunktionen.setLook(this);
        // Set up our application to respond to the Mac OS X application menu
        registerForMacOSXEvents();
        //#########################################
        initListener();
//        tabelleLaden();
        tModel = ListeStarts.getNewModel();
        jTable1.setModel(tModel);
        jTable1.setDefaultRenderer(Object.class, new CellRendererFilme(daten));
        jTable1.setAutoResizeMode(javax.swing.JTable.AUTO_RESIZE_OFF);
        jTable1.addMouseListener(new BeobMausTabelle(jTable1));
        this.pack();
        this.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        //nachschauen nach updates und server aktualisieren
////////////        daten.filmUpdateServer.suchen(true, false);
//////////        daten.filmeLaden.filmeImportServer();
    }

    private void initListener() {
//        jCheckBoxAbos.addActionListener(new BeobAbo());
        jSpinnerDownload.addChangeListener(new BeobSpinnerDownload());
        daten.starterClass.addListener(new BeobachterStart());
        jButtonStop.addActionListener(new AlleAbbrechen());
        jButtonNachFilm.addActionListener(new AlleAbbrechenNachFilm());
        jButtonAufraeumen.addActionListener(new BeobachterAufraeumen());
        addWindowListener(new java.awt.event.WindowAdapter() {

            @Override
            public void windowClosing(java.awt.event.WindowEvent evt) {
                beenden();
            }
        });
        //zum Laden
        DDaten.filmeLaden.addAdListener(new BeobachterLadenFilme());
    }

    private void progressBar(JProgressBar bar, int max, int progress, String text, boolean seiten /* anz. Seiten anzeigen */) {
        if (max == 0) {
            bar.setIndeterminate(false);
            bar.setMaximum(0);
            bar.setMinimum(0);
            bar.setValue(0);
            bar.setStringPainted(false);
        } else if (max == 1) {
            bar.setIndeterminate(true);
            bar.setMaximum(1);
            bar.setMinimum(0);
            bar.setValue(0);
            bar.setStringPainted(false);
            bar.setVisible(true);
        } else {
            bar.setIndeterminate(false);
            bar.setMaximum(max);
            bar.setMinimum(0);
            int proz = 0;
            if (progress != 0) {
                proz = progress * 100 / max;
                if (proz >= 100) {
                    proz = 99;
                }
            }
            if (seiten) {
//////////                bar.setString("( " + daten.filmeLaden.getSeitenZaehlerLauf() + " Seiten  /  " + proz + "% von " + max + " Themen )  " + Funktionen.textLaenge(50, text));
            } else {
                bar.setString("( " + proz + "% von " + max + " Themen )  " + GuiFunktionen.textLaenge(50, text));
            }
            bar.setStringPainted(true);
            bar.setValue(progress);
        }
    }

    private void progressBarFilmliste(int max, int progress, String text) {
        if (max == 0) {
            jProgressFilme.setString(GuiFunktionen.textLaenge(text));
            jProgressFilme.setIndeterminate(false);
            jProgressFilme.setMaximum(0);
            jProgressFilme.setMinimum(0);
            jProgressFilme.setValue(0);
            jProgressFilme.setVisible(false);
            jButtonStop.setVisible(false);
        } else if (max == 1) {
            jProgressFilme.setString(GuiFunktionen.textLaenge(text));
            jProgressFilme.setIndeterminate(true);
            jProgressFilme.setMaximum(1);
            jProgressFilme.setMinimum(0);
            jProgressFilme.setValue(0);
            jProgressFilme.setVisible(true);
            jButtonStop.setVisible(true);
        } else {
            jProgressFilme.setIndeterminate(false);
            jProgressFilme.setVisible(true);
            jButtonStop.setVisible(true);
            jProgressFilme.setMaximum(max);
            jProgressFilme.setMinimum(0);
            int proz = 0;
            if (progress != 0) {
                proz = progress * 100 / max;
                if (proz >= 100) {
                    proz = 99;
                }
            }
            jProgressFilme.setString(text + "  ( " + proz + "% )");
            jProgressFilme.setValue(progress);
        }
        jProgressFilme.setStringPainted(true);
        this.validate();
    }

    private void panelUpdate() {
        jTable1.repaint();
        this.validate();
    }

    private void tabelleLaden() {
        tModel = daten.starterClass.getStarterModell(tModel);
        jTable1.updateUI();
    }

    private void setProgressBarDownload(StartEvent ev) {
        panelUpdate();
        if (ev.nixTun()) {
            jLabelDownload.setText("Downloads:");
            jProgressDownloads.setMaximum(0);
            jProgressDownloads.setMinimum(0);
            jProgressDownloads.setValue(0);
            jProgressDownloads.setStringPainted(false);
        } else {
            jLabelDownload.setText("Downloads (" + ev.getMax() + "):");
            jProgressDownloads.setMaximum(ev.getMax());
            jProgressDownloads.setMinimum(0);
            jProgressDownloads.setValue(ev.getProgress());
            jProgressDownloads.setStringPainted(true);
        }
        if (ev.getDown() == 0) {
            jTextFieldDownloads.setText(String.valueOf(""));
            jTextFieldDownloads.setBackground(javax.swing.UIManager.getDefaults().getColor("TextField.inactiveBackground"));
        } else {
            jTextFieldDownloads.setText(String.valueOf(ev.getDown()));
            jTextFieldDownloads.setBackground(GuiKonstanten.DOWNLOAD_FARBE_RUN);
        }
        this.repaint();
    }

    private void beenden() {
        daten.allesAbbrechen();
        if (daten.isGeaendert()) {
            daten.allesSpeichern();
        }
    }

    private void undTschuess() {
        daten.allesSpeichern();
        this.dispose();
        System.exit(0);
    }

    private synchronized void alleDownloadsAbbrechen() {
        daten.allesAbbrechen();
//////////        Iterator<MediathekReader> it = daten.filmeLaden.mediathekListe.iterator();
//////////        while (it.hasNext()) {
//////////            it.next().stoppen();
//////////        }
    }

    private synchronized void alleDownloadsAbbrechenNachFilm() {
        daten.allesAbbrechenNachFilm();
    }

    private void downloadPods() {
//////////        if (!GuiPodder.downloadAll(daten, daten.feedReaderPods.getListe().addPodModel(new TModelFilm(new Object[][]{}, DatenFilm.FILME_COLUMN_NAMES)), "")) {
//////////            keinePods = true;
//////////        }
        podsLadenJetzt = true;
        tabelleLaden();
        check();
    }

    private void downloadFilme() {
//////////////        if (!GuiAbo.downloadAll(daten, daten.filmeLaden.listeFilme.addAboModel(new TModelFilm(new Object[][]{}, DatenFilm.FILME_COLUMN_NAMES)), "")) {
//////////////            keineAbos = true;
//////////////        }
        abosLadenJetzt = true;
        tabelleLaden();
        check();
    }

    private void check() {
        if (keineAbos && keinePods) {
            undTschuess();
        }
    }

    private void aufraeumen() {
        if (jTable1.getModel() != null) {
            if (jTable1.getModel().getRowCount() > 0) {
                TModel tModel = (TModel) jTable1.getModel();
                for (int i = 0; i < tModel.getRowCount(); ++i) {
                    int s = daten.starterClass.getState(tModel.getValueAt(i, DatenFilm.FILM_URL_NR).toString());
                    if (s >= Starts.STATUS_FERTIG) {
                        tModel.removeRow(i);
                        --i;
                    }
                }
            }
        }
        daten.starterClass.aufraeumen();
    }

    private void downloadLoeschen(boolean dauerhaft) {
        int rows[] = jTable1.getSelectedRows();
        if (rows.length > 0) {
            TModel tModel = (TModel) jTable1.getModel();
            for (int i = rows.length - 1; i >= 0; --i) {
                int delRow = jTable1.convertRowIndexToModel(rows[i]);
                if (dauerhaft) {
                    // boolean zeileSchreiben(String datum, String thema, String titel, String url, String logdatei) {
                    daten.log.zeileSchreiben(tModel.getValueAt(delRow, DatenFilm.FILM_THEMA_NR).toString(),
                            tModel.getValueAt(delRow, DatenFilm.FILM_TITEL_NR).toString(),
                            tModel.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString());
                }
                daten.starterClass.filmLoeschen(tModel.getValueAt(delRow, DatenFilm.FILM_URL_NR).toString());
                tModel.removeRow(delRow);
            }
        } else {
            new HinweisKeineAuswahl().zeigen();
        }
    }

    private void open(DatenPgruppe gruppe) {
////        DatenFilm ersterFilm = new DatenFilm();
////        int select = jTable1.convertRowIndexToModel(jTable1.getSelectedRow());
////        for (int i = 0; i < DatenFilm.FILME_MAX_ELEM; ++i) {
////            ersterFilm.arr[i] = jTable1.getModel().getValueAt(select, i).toString();
////        }
////        daten.starterClass.urlStarten(gruppe, ersterFilm);
    }

    /** This method is called from within the constructor to
     * initialize the form.
     * WARNING: Do NOT modify this code. The content of this method is
     * always regenerated by the Form Editor.
     */
    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    private void initComponents() {

        jLabel = new javax.swing.JLabel();
        jProgressDownloads = new javax.swing.JProgressBar();
        jButtonStop = new javax.swing.JButton();
        jLabel2 = new javax.swing.JLabel();
        jTextFieldDownloads = new javax.swing.JTextField();
        jLabelDownload = new javax.swing.JLabel();
        jProgressPods = new javax.swing.JProgressBar();
        jLabel1 = new javax.swing.JLabel();
        jProgressFilme = new javax.swing.JProgressBar();
        jLabel3 = new javax.swing.JLabel();
        jScrollPane1 = new javax.swing.JScrollPane();
        jTable1 = new javax.swing.JTable();
        jSpinnerDownload = new javax.swing.JSpinner();
        jButtonAufraeumen = new javax.swing.JButton();
        jButtonNachFilm = new javax.swing.JButton();

        setDefaultCloseOperation(javax.swing.WindowConstants.EXIT_ON_CLOSE);

        jLabel.setText("Liste Podcasts:");

        jProgressDownloads.setFont(new java.awt.Font("Monospaced", 1, 12)); // NOI18N

        jButtonStop.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/process-stop.png"))); // NOI18N
        jButtonStop.setText("sofort stoppen");
        jButtonStop.setToolTipText("Downloads sofort stoppen");

        jLabel2.setText("laufende Downloads:");

        jTextFieldDownloads.setEditable(false);
        jTextFieldDownloads.setHorizontalAlignment(javax.swing.JTextField.CENTER);

        jLabelDownload.setText("Downloads:");

        jProgressPods.setFont(new java.awt.Font("Monospaced", 1, 12)); // NOI18N

        jLabel1.setText("Liste Filme:");

        jProgressFilme.setFont(new java.awt.Font("Monospaced", 1, 12)); // NOI18N

        jLabel3.setText("Downloads:");

        jTable1.setModel(new javax.swing.table.DefaultTableModel(
            new Object [][] {
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null},
                {null, null, null, null}
            },
            new String [] {
                "Title 1", "Title 2", "Title 3", "Title 4"
            }
        ));
        jScrollPane1.setViewportView(jTable1);

        jSpinnerDownload.setModel(new javax.swing.SpinnerNumberModel(1, 1, 9, 1));

        jButtonAufraeumen.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/edit-clear.png"))); // NOI18N
        jButtonAufraeumen.setText("Aufräumen");
        jButtonAufraeumen.setToolTipText("Abgeschlossene Downloads aus der Tabelle entfernen");

        jButtonNachFilm.setIcon(new javax.swing.ImageIcon(getClass().getResource("/mediathek/res/emblem-unreadable.png"))); // NOI18N
        jButtonNachFilm.setText("nach Film stoppen");
        jButtonNachFilm.setToolTipText("Downloads nach dem Laufendem Film abbrechen");

        javax.swing.GroupLayout layout = new javax.swing.GroupLayout(getContentPane());
        getContentPane().setLayout(layout);
        layout.setHorizontalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
                    .addComponent(jScrollPane1, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 830, Short.MAX_VALUE)
                    .addComponent(jLabel)
                    .addComponent(jProgressPods, javax.swing.GroupLayout.DEFAULT_SIZE, 830, Short.MAX_VALUE)
                    .addComponent(jLabel1)
                    .addComponent(jProgressFilme, javax.swing.GroupLayout.DEFAULT_SIZE, 830, Short.MAX_VALUE)
                    .addComponent(jLabel3)
                    .addComponent(jProgressDownloads, javax.swing.GroupLayout.Alignment.TRAILING, javax.swing.GroupLayout.DEFAULT_SIZE, 830, Short.MAX_VALUE)
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jLabelDownload)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED, 507, Short.MAX_VALUE)
                        .addComponent(jLabel2)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jTextFieldDownloads, javax.swing.GroupLayout.PREFERRED_SIZE, 38, javax.swing.GroupLayout.PREFERRED_SIZE)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                        .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE))
                    .addGroup(layout.createSequentialGroup()
                        .addComponent(jButtonStop)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonNachFilm)
                        .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.UNRELATED)
                        .addComponent(jButtonAufraeumen)))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.HORIZONTAL, new java.awt.Component[] {jButtonAufraeumen, jButtonNachFilm, jButtonStop});

        layout.setVerticalGroup(
            layout.createParallelGroup(javax.swing.GroupLayout.Alignment.LEADING)
            .addGroup(layout.createSequentialGroup()
                .addContainerGap()
                .addComponent(jLabel)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jProgressPods, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jLabel1)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jProgressFilme, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addGap(18, 18, 18)
                .addComponent(jLabel3)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jProgressDownloads, javax.swing.GroupLayout.PREFERRED_SIZE, 23, javax.swing.GroupLayout.PREFERRED_SIZE)
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.CENTER)
                    .addComponent(jLabelDownload)
                    .addComponent(jSpinnerDownload, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jTextFieldDownloads, javax.swing.GroupLayout.PREFERRED_SIZE, javax.swing.GroupLayout.DEFAULT_SIZE, javax.swing.GroupLayout.PREFERRED_SIZE)
                    .addComponent(jLabel2))
                .addPreferredGap(javax.swing.LayoutStyle.ComponentPlacement.RELATED)
                .addComponent(jScrollPane1, javax.swing.GroupLayout.DEFAULT_SIZE, 143, Short.MAX_VALUE)
                .addGap(18, 18, 18)
                .addGroup(layout.createParallelGroup(javax.swing.GroupLayout.Alignment.BASELINE)
                    .addComponent(jButtonStop)
                    .addComponent(jButtonNachFilm)
                    .addComponent(jButtonAufraeumen))
                .addContainerGap())
        );

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jProgressDownloads, jProgressFilme, jProgressPods});

        layout.linkSize(javax.swing.SwingConstants.VERTICAL, new java.awt.Component[] {jButtonAufraeumen, jButtonNachFilm, jButtonStop});

        pack();
    }// </editor-fold>//GEN-END:initComponents
    /**
     * @param args the command line arguments
     */
    // Variables declaration - do not modify//GEN-BEGIN:variables
    private javax.swing.JButton jButtonAufraeumen;
    private javax.swing.JButton jButtonNachFilm;
    private javax.swing.JButton jButtonStop;
    private javax.swing.JLabel jLabel;
    private javax.swing.JLabel jLabel1;
    private javax.swing.JLabel jLabel2;
    private javax.swing.JLabel jLabel3;
    private javax.swing.JLabel jLabelDownload;
    private javax.swing.JProgressBar jProgressDownloads;
    private javax.swing.JProgressBar jProgressFilme;
    private javax.swing.JProgressBar jProgressPods;
    private javax.swing.JScrollPane jScrollPane1;
    private javax.swing.JSpinner jSpinnerDownload;
    private javax.swing.JTable jTable1;
    private javax.swing.JTextField jTextFieldDownloads;
    // End of variables declaration//GEN-END:variables

    private class AlleAbbrechen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            alleDownloadsAbbrechen();
        }
    }

    private class AlleAbbrechenNachFilm implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            alleDownloadsAbbrechenNachFilm();
        }
    }

    private class BeobachterStart implements StartListener {

        @Override
        public void starter(StartEvent ev) {
            setProgressBarDownload(ev);
            if (ev.nixTun()) {
                if (podsLadenJetzt && abosLadenJetzt) {
                    //Downloads sind jetzt fertig
                    undTschuess();
                }

            }
        }
    }

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

//    private class BeobachterLadenPods extends BeobFilmeLaden {
//
//        @Override
//        public void initProgressBar(String sender, int threads, int max, int progress, String text) {
//            progressBar(jProgressPods, max, progress, text, false);
//        }
//
//        @Override
//        public void beenden() {
//            downloadPods();
//        }
//    }
    private class BeobachterLadenFilme extends BeobFilmeLaden {
//////        @Override
//////        public void initProgressBar(String sender, int max, int progress, String text) {
//////            progressBar(jProgressFilme, max, progress, text, true);
//////        }
//////
////////////////////        @Override
////////////////////        public void progressBar(int threads, int max, int progress, String text) {
////////////////////            progressBarFilmliste(max, progress, text);
////////////////////        }
//////        @Override
//////        public void beenden() {
//////            downloadFilme();
//////        }
    }

    private class BeobSpinnerDownload implements ChangeListener {

        @Override
        public void stateChanged(ChangeEvent arg0) {
            daten.system[Konstanten.SYSTEM_MAX_DOWNLOAD_NR] =
                    String.valueOf(((Number) jSpinnerDownload.getModel().getValue()).intValue());
            daten.setGeaendert();
        }
    }

    private class BeobachterAufraeumen implements ActionListener {

        @Override
        public void actionPerformed(ActionEvent arg0) {
            aufraeumen();
        }
    }

    private class BeobDownloadLoeschen implements ActionListener {

        boolean dauerhaft = false;

        public BeobDownloadLoeschen(boolean d) {
            dauerhaft = d;
        }

        @Override
        public void actionPerformed(ActionEvent arg0) {
            downloadLoeschen(dauerhaft);
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

        private BeobUrl beobUrl = new BeobUrl();
        private Point p;
        private JTable tabelle;

        public BeobMausTabelle(JTable ttabelle) {
            tabelle = ttabelle;
        }

        private class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                int nr = tabelle.rowAtPoint(p);
                if (nr >= 0) {
                    GuiFunktionen.copyToClipboard(
                            tabelle.getModel().getValueAt(tabelle.convertRowIndexToModel(nr),
                            DatenFilm.FILM_URL_NR).toString());
                }
            }
        }

        @Override
        public void mouseClicked(MouseEvent arg0) {
            if (arg0.getButton() == MouseEvent.BUTTON1) {
                if (arg0.getClickCount() > 1) {
            DatenPgruppe gruppe = daten.listePgruppe.getPgruppeAbspielen();
            if (gruppe != null) {
                open(gruppe);
                    } else {
                        JOptionPane.showMessageDialog(null, "unter \"Pfade\" einen Standardbutton festlegen.",
                                "keine Standardbutton!", JOptionPane.INFORMATION_MESSAGE);
                    }
                }
            } else if (arg0.getButton() == MouseEvent.BUTTON3) {
                showMenu(arg0);
            }
        }

        private void showMenu(MouseEvent evt) {
            p = evt.getPoint();
            int nr = tabelle.rowAtPoint(p);
            if (nr >= 0) {
                tabelle.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu menu = new JPopupMenu();
            //url
            JMenuItem itemVor = new JMenuItem("URL kopieren");
            itemVor.addActionListener(beobUrl);
            menu.add(itemVor);
            //##Trenner##
            menu.addSeparator();
            //löschen
            JMenuItem itemLoeschen = new JMenuItem("Download löschen");
            itemLoeschen.addActionListener(new BeobDownloadLoeschen(false));
            menu.add(itemLoeschen);
            //dauerhaft löschen
            JMenuItem itemDauerhaftLoeschen = new JMenuItem("Download dauerhaft löschen");
            itemDauerhaftLoeschen.addActionListener(new BeobDownloadLoeschen(true));
            menu.add(itemDauerhaftLoeschen);
            //Menü anzeigen
            menu.show(evt.getComponent(), evt.getX(), evt.getY());
        }
    }
}
