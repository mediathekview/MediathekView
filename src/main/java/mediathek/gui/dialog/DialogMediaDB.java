package mediathek.gui.dialog;

import jiconfont.icons.FontAwesome;
import jiconfont.swing.IconFontSwing;
import mediathek.config.Daten;
import mediathek.config.Icons;
import mediathek.config.MVConfig;
import mediathek.daten.DatenMediaDB;
import mediathek.file.GetFile;
import mediathek.gui.messages.mediadb.MediaDbDialogVisibleEvent;
import mediathek.gui.messages.mediadb.MediaDbStartEvent;
import mediathek.gui.messages.mediadb.MediaDbStopEvent;
import mediathek.tool.*;
import mediathek.tool.cellrenderer.CellRendererMediaDB;
import mediathek.tool.models.TModelMediaDB;
import mediathek.tool.table.MVMediaDbTable;
import mediathek.tool.table.MVTable;
import net.engio.mbassy.listener.Handler;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.swing.*;
import javax.swing.event.DocumentEvent;
import javax.swing.event.DocumentListener;
import java.awt.*;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.io.File;

@SuppressWarnings("serial")
public class DialogMediaDB extends JDialog {
    private final JFrame parent;
    private final Daten daten;
    private final MVTable tabelleFilme;

    @Handler
    private void handleMediaDbStartEvent(MediaDbStartEvent e) {
        SwingUtilities.invokeLater(() -> {
            // neue DB suchen
            makeIndex(true);
            jLabelSum.setText("0");
        });
    }

    @Handler
    private void handleMediaDbStopEvent(MediaDbStopEvent e) {
        SwingUtilities.invokeLater(() -> {
            // neue DB liegt vor
            makeIndex(false);
            jLabelSum.setText(Integer.toString(daten.getListeMediaDB().size()));
            searchFilmInDb();
        });
    }

    public DialogMediaDB(JFrame pparent) {
        super(pparent, false);
        daten = Daten.getInstance();
        initComponents();

        daten.getMessageBus().subscribe(this);

        this.parent = pparent;
        this.addWindowListener(new WindowAdapter() {
            @Override
            public void windowClosing(WindowEvent e) {
                beenden();
            }
        });

        tabelleFilme = new MVMediaDbTable();
        jScrollPane3.setViewportView(tabelleFilme);

        TModelMediaDB modelFilm = new TModelMediaDB(new Object[][]{}, DatenMediaDB.COLUMN_NAMES);
        final CellRendererMediaDB cellRenderer = new CellRendererMediaDB();
        tabelleFilme.setDefaultRenderer(Object.class, cellRenderer);
        tabelleFilme.setModel(modelFilm);
        tabelleFilme.addMouseListener(new BeobMausTabelle());
        tabelleFilme.getSelectionModel().addListSelectionListener(event -> {
            if (!event.getValueIsAdjusting()) {
                aktFilmSetzen();
            }
        });
        tabelleFilme.setAutoResizeMode(JTable.AUTO_RESIZE_SUBSEQUENT_COLUMNS);
        tabelleFilme.initTabelle();

        progress.setVisible(false);
        progress.setIndeterminate(true);
        progress.setMaximum(0);
        progress.setMinimum(0);
        progress.setValue(0);

        jTextFieldSearch.addActionListener(e -> searchFilmInDb());
        jTextFieldSearch.getDocument().addDocumentListener(new BeobDoc());

        jButtonIndex.addActionListener(e -> daten.getListeMediaDB().createMediaDB(""));

        jButtonHelp.setIcon(Icons.ICON_BUTTON_HELP);
        jButtonHelp.addActionListener(e -> new DialogHilfe(parent, true, new GetFile().getHilfeSuchen(GetFile.PFAD_HILFETEXT_DIALOG_MEDIA_DB)).setVisible(true));
        jButtonSearch.addActionListener(e -> searchFilmInDb());
        jButtonBeenden.addActionListener(e -> beenden());

        EscapeKeyHandler.installHandler(this, this::beenden);

        GuiFunktionen.setSize(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_GROESSE, this, parent);
    }



    @Override
    public void setVisible(boolean vis) {
        super.setVisible(vis);
        if (vis && daten.getListeMediaPath().isEmpty()) {
            JOptionPane.showMessageDialog(parent, "Erst in den Einstellungen eine Mediensammlung einrichten.", "Mediensammlung leer!", JOptionPane.ERROR_MESSAGE);
        }
    }

    public final void setVis() {
        this.setVisible(Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN)));
        daten.getMessageBus().publishAsync(new MediaDbDialogVisibleEvent());
    }

    public void tabelleSpeichern() {
        if (tabelleFilme != null) {
            tabelleFilme.tabelleNachDatenSchreiben();
        }
    }

    public void setFilter(String titel) {
        titel = FilenameUtils.replaceLeerDateiname(titel, false /*pfad*/,
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_USE_REPLACETABLE)),
                Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_ONLY_ASCII))); // mit den eingestellten Ersetzungen bearbeiten
        jTextFieldSearch.setText(titel);
    }

    private synchronized void searchFilmInDb() {
        TModelMediaDB model = new TModelMediaDB(new Object[][]{}, DatenMediaDB.COLUMN_NAMES);
        daten.getListeMediaDB().searchFilmInDB(model, jTextFieldSearch.getText());
        tabelleFilme.getSpalten();
        tabelleFilme.setModel(model);
        tabelleFilme.setSpalten();
        jLabelSizeFound.setText(model.getRowCount() + "");
    }

    private void makeIndex(boolean makeIndex) {
        progress.setVisible(makeIndex);
        jTextFieldSearch.setEnabled(!makeIndex);
        jButtonSearch.setEnabled(!makeIndex);
        jButtonIndex.setEnabled(!makeIndex);
    }

    private void zielordnerOeffnen() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String s = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            DirOpenAction.zielordnerOeffnen(parent, s);
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void filmAbspielen_() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            OpenPlayerAction.filmAbspielen(parent, path + File.separator + file);
        } else {
            NoSelectionErrorDialog.show();
        }
    }

    private void aktFilmSetzen() {
        int row = tabelleFilme.getSelectedRow();
        if (row >= 0) {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            jTextFieldFilmTitle.setText(file);
            jTextFieldFilmPfad.setText(path);
        } else {
            jTextFieldFilmTitle.setText("");
            jTextFieldFilmPfad.setText("");
        }
    }

    private void filmLoeschen_() {
        String del = "";
        int row = tabelleFilme.getSelectedRow();
        if (row < 0) {
            NoSelectionErrorDialog.show();
            return;
        }
        try {
            String file = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_NAME);
            String path = (String) tabelleFilme.getModel().getValueAt(tabelleFilme.convertRowIndexToModel(row), DatenMediaDB.MEDIA_DB_PATH);
            del = path + File.separator + file;
            File delFile = new File(del);
            if (!delFile.exists()) {
                MVMessageDialog.showMessageDialog(parent, "Die Datei existiert nicht!", "Film löschen", JOptionPane.ERROR_MESSAGE);
                return;
            }
            int ret = JOptionPane.showConfirmDialog(parent, delFile.getAbsolutePath(), "Film Löschen?", JOptionPane.YES_NO_OPTION);
            if (ret != JOptionPane.OK_OPTION) {
                return;
            }

            // und jetzt die Datei löschen
            logger.info(new String[]{"Datei löschen: ", delFile.getAbsolutePath()});
            if (!delFile.delete()) {
                throw new Exception();
            }
        } catch (Exception ex) {
            MVMessageDialog.showMessageDialog(parent, "Konnte die Datei nicht löschen!", "Film löschen", JOptionPane.ERROR_MESSAGE);
            logger.error("Fehler beim löschen: " + del);
        }
    }

    private static final Logger logger = LogManager.getLogger(DialogMediaDB.class);

    private void beenden() {
        MVConfig.add(MVConfig.Configs.SYSTEM_MEDIA_DB_DIALOG_ANZEIGEN, Boolean.FALSE.toString());
        setVis();
    }

    private class BeobDoc implements DocumentListener {

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
            Filter.validatePatternInput(jTextFieldSearch);
            if (Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_MEDIA_DB_ECHTZEITSUCHE))) {
                searchFilmInDb();
            }
        }
    }

    public class BeobMausTabelle extends MouseAdapter {

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
            Point p = evt.getPoint();
            int nr = tabelleFilme.rowAtPoint(p);
            if (nr >= 0) {
                tabelleFilme.setRowSelectionInterval(nr, nr);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();

            // Film abspielen
            JMenuItem itemPlayerDownload = new JMenuItem("gespeicherten Film (Datei) abspielen");
            itemPlayerDownload.setIcon(IconFontSwing.buildIcon(FontAwesome.PLAY, 16));
            itemPlayerDownload.addActionListener(e -> filmAbspielen_());
            jPopupMenu.add(itemPlayerDownload);

            // Film löschen
            JMenuItem itemDeleteDownload = new JMenuItem("gespeicherten Film (Datei) löschen");
            itemDeleteDownload.setIcon(Icons.ICON_BUTTON_DEL);
            itemDeleteDownload.addActionListener(e -> filmLoeschen_());
            jPopupMenu.add(itemDeleteDownload);

            // Zielordner öffnen
            JMenuItem itemOeffnen = new JMenuItem("Zielordner öffnen");
            itemOeffnen.setIcon(Icons.ICON_MENUE_FILE_OPEN);
            jPopupMenu.add(itemOeffnen);
            itemOeffnen.addActionListener(arg0 -> zielordnerOeffnen());

            jPopupMenu.addSeparator();
            // Reset Tabelle
            JMenuItem itemResetTab = new JMenuItem("Tabelle zurücksetzen");
            jPopupMenu.add(itemResetTab);
            itemResetTab.addActionListener(arg0 -> tabelleFilme.resetTabelle());

            // ######################
            // Menü anzeigen
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        jButtonBeenden = new JButton();
        jButtonHelp = new JButton();
        jTextFieldSearch = new JTextField();
        jButtonSearch = new JButton();
        jScrollPane3 = new JScrollPane();
        var jLabel1 = new JLabel();
        jLabelSizeFound = new JLabel();
        var jLabel4 = new JLabel();
        jButtonIndex = new JButton();
        jLabelSum = new JLabel();
        progress = new JProgressBar();
        var jLabel3 = new JLabel();
        jTextFieldFilmTitle = new JTextField();
        var jLabel5 = new JLabel();
        jTextFieldFilmPfad = new JTextField();

        //======== this ========
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        setTitle("Mediensammlung durchsuchen"); //NON-NLS
        var contentPane = getContentPane();

        //---- jButtonBeenden ----
        jButtonBeenden.setText("Schlie\u00dfen"); //NON-NLS

        //---- jButtonHelp ----
        jButtonHelp.setIcon(new ImageIcon(getClass().getResource("/mediathek/res/muster/button-help.png"))); //NON-NLS
        jButtonHelp.setToolTipText("Hilfe anzeigen"); //NON-NLS

        //---- jButtonSearch ----
        jButtonSearch.setText("Suchen"); //NON-NLS

        //---- jLabel1 ----
        jLabel1.setText("Treffer:"); //NON-NLS

        //---- jLabelSizeFound ----
        jLabelSizeFound.setText("0"); //NON-NLS

        //---- jLabel4 ----
        jLabel4.setText("Anzahl Medien gesamt:"); //NON-NLS

        //---- jButtonIndex ----
        jButtonIndex.setText("Index neu aufbauen"); //NON-NLS

        //---- jLabelSum ----
        jLabelSum.setText("0"); //NON-NLS

        //---- jLabel3 ----
        jLabel3.setText("Titel:"); //NON-NLS

        //---- jTextFieldFilmTitle ----
        jTextFieldFilmTitle.setEditable(false);

        //---- jLabel5 ----
        jLabel5.setText("Pfad:"); //NON-NLS

        //---- jTextFieldFilmPfad ----
        jTextFieldFilmPfad.setEditable(false);

        GroupLayout contentPaneLayout = new GroupLayout(contentPane);
        contentPane.setLayout(contentPaneLayout);
        contentPaneLayout.setHorizontalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addComponent(jScrollPane3, GroupLayout.DEFAULT_SIZE, 386, Short.MAX_VALUE)
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(jTextFieldSearch)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonSearch)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonHelp))
                        .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                            .addComponent(progress, GroupLayout.DEFAULT_SIZE, 122, Short.MAX_VALUE)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonIndex)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jButtonBeenden, GroupLayout.PREFERRED_SIZE, 93, GroupLayout.PREFERRED_SIZE))
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(jLabel4)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabelSum)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED, GroupLayout.DEFAULT_SIZE, Short.MAX_VALUE)
                            .addComponent(jLabel1)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jLabelSizeFound))
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(jLabel3)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jTextFieldFilmTitle))
                        .addGroup(contentPaneLayout.createSequentialGroup()
                            .addComponent(jLabel5)
                            .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                            .addComponent(jTextFieldFilmPfad)))
                    .addContainerGap())
        );
        contentPaneLayout.setVerticalGroup(
            contentPaneLayout.createParallelGroup()
                .addGroup(GroupLayout.Alignment.TRAILING, contentPaneLayout.createSequentialGroup()
                    .addContainerGap()
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jTextFieldSearch, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                            .addComponent(jButtonSearch))
                        .addComponent(jButtonHelp))
                    .addGap(18, 18, 18)
                    .addGroup(contentPaneLayout.createParallelGroup()
                        .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel4)
                            .addComponent(jLabelSum))
                        .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                            .addComponent(jLabel1)
                            .addComponent(jLabelSizeFound)))
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addComponent(jScrollPane3, GroupLayout.DEFAULT_SIZE, 80, Short.MAX_VALUE)
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel3)
                        .addComponent(jTextFieldFilmTitle, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                    .addPreferredGap(LayoutStyle.ComponentPlacement.RELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.BASELINE)
                        .addComponent(jLabel5)
                        .addComponent(jTextFieldFilmPfad, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE))
                    .addPreferredGap(LayoutStyle.ComponentPlacement.UNRELATED)
                    .addGroup(contentPaneLayout.createParallelGroup(GroupLayout.Alignment.CENTER)
                        .addComponent(progress, GroupLayout.PREFERRED_SIZE, GroupLayout.DEFAULT_SIZE, GroupLayout.PREFERRED_SIZE)
                        .addComponent(jButtonIndex)
                        .addComponent(jButtonBeenden))
                    .addContainerGap())
        );
        contentPaneLayout.linkSize(SwingConstants.VERTICAL, new Component[] {jButtonHelp, jButtonSearch, jTextFieldSearch});
        pack();
        setLocationRelativeTo(getOwner());
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    private JButton jButtonBeenden;
    private JButton jButtonHelp;
    private JTextField jTextFieldSearch;
    private JButton jButtonSearch;
    private JScrollPane jScrollPane3;
    private JLabel jLabelSizeFound;
    private JButton jButtonIndex;
    private JLabel jLabelSum;
    private JProgressBar progress;
    private JTextField jTextFieldFilmTitle;
    private JTextField jTextFieldFilmPfad;
    // End of variables declaration//GEN-END:variables
}
