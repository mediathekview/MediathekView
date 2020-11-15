package mediathek.gui.history;

import mediathek.config.Daten;
import mediathek.controller.history.AboHistoryController;
import mediathek.controller.history.MVUsedUrl;
import mediathek.controller.history.MVUsedUrlModelHelper;
import mediathek.daten.DatenDownload;
import mediathek.daten.DatenFilm;
import mediathek.gui.dialog.DialogAddDownload;
import mediathek.gui.filmInformation.InfoDialog;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.FileDialogs;
import mediathek.tool.GermanStringSorter;
import mediathek.tool.GuiFunktionen;
import mediathek.tool.models.NonEditableTableModel;
import net.miginfocom.layout.AC;
import net.miginfocom.layout.CC;
import net.miginfocom.layout.LC;
import net.miginfocom.swing.MigLayout;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import javax.swing.table.TableModel;
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseAdapter;
import java.awt.event.MouseEvent;
import java.io.File;
import java.util.ArrayList;
import java.util.List;

@SuppressWarnings("serial")
public abstract class PanelErledigteUrls extends JPanel {
    protected final Daten daten;
    protected AboHistoryController workList;

    public PanelErledigteUrls() {
        this.daten = Daten.getInstance();

        initComponents();
        jTable1.addMouseListener(new BeobMausTabelle());
        jButtonExport.addActionListener((ActionEvent e) -> export());
        initListeners();
    }

    protected void changeListHandler() {
        if (jToggleButtonLaden.isSelected())
            updateModelAndRecalculate(createDataModel());
    }

    private void initListeners() {
        jToggleButtonLaden.addActionListener((ActionEvent e) -> {
            if (jToggleButtonLaden.isSelected()) {
                updateModelAndRecalculate(createDataModel());
            } else {
                updateModelAndRecalculate(new NonEditableTableModel(null, MVUsedUrlModelHelper.TITLE_HEADER));
            }
        });
    }

    private void updateModelAndRecalculate(@NotNull TableModel model) {
        jTable1.setModel(model);
        setsum();
    }

    protected TableModel createDataModel() {
        final var data = MVUsedUrlModelHelper.getObjectData(workList.getListeUrlsSortDate());
        return new NonEditableTableModel(data, MVUsedUrlModelHelper.TITLE_HEADER);
    }

    private void setsum() {
        if (jTable1.getRowCount() <= 0) {
            jLabelSum.setText("");
        } else {
            jLabelSum.setText("Anzahl: " + jTable1.getRowCount());
        }
    }

    protected List<MVUsedUrl> getExportableList() {
        ArrayList<MVUsedUrl> ret = new ArrayList<>(workList.getListeUrlsSortDate());
        GermanStringSorter sorter = GermanStringSorter.getInstance();
        ret.sort((o1, o2) -> sorter.compare(o1.getTitel(), o2.getTitel()));

        return ret;
    }

    private void export() {
        if (jTable1.getModel().getRowCount() <= 0)
            return;

        var title = "Filmtitel speichern";
            title = "Abo-Historie speichern";

        var destFile = FileDialogs.chooseSaveFileLocation(MediathekGui.ui(),title,GuiFunktionen.getHomePath() + File.separator + "Mediathek-Filme.txt");
        if (destFile != null) {
            new HistoryWriterThread(destFile.getAbsolutePath(), getExportableList()).start();
        }
    }

    class BeobMausTabelle extends MouseAdapter {

        //rechte Maustaste in der Tabelle
        private final BeobLoeschen beobLoeschen = new BeobLoeschen();
        private final BeobUrl beobUrl = new BeobUrl();
        private DatenFilm film;

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
            int nr = jTable1.rowAtPoint(p);
            if (nr >= 0) {
                jTable1.setRowSelectionInterval(nr, nr);
                String url = jTable1.getValueAt(jTable1.convertRowIndexToModel(nr), MVUsedUrlModelHelper.USED_URL_URL).toString();
                film = daten.getListeFilme().getFilmByUrl(url);
            }
            JPopupMenu jPopupMenu = new JPopupMenu();
            //löschen
            JMenuItem item = new JMenuItem("Url aus der Liste löschen");
            item.addActionListener(beobLoeschen);
            jPopupMenu.add(item);
            //Url
            item = new JMenuItem("URL kopieren");
            item.addActionListener(beobUrl);
            jPopupMenu.add(item);
            // Infos anzeigen
            item = new JMenuItem("Infos zum Film anzeigen");
            item.addActionListener(new BeobInfo());
            jPopupMenu.add(item);
            if (film == null) {
                item.setEnabled(false);
            }
            // Download anlegen
            item = new JMenuItem("Download noch einmal anlegen");
            item.addActionListener(new BeobDownload());
            jPopupMenu.add(item);
            if (film == null) {
                item.setEnabled(false);
            }
            jPopupMenu.show(evt.getComponent(), evt.getX(), evt.getY());
        }

        class BeobDownload implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                DatenDownload datenDownload = daten.getListeDownloads().getDownloadUrlFilm(film.getUrl());
                if (datenDownload != null) {
                    int ret = JOptionPane.showConfirmDialog(getParent(), "Download für den Film existiert bereits.\n"
                            + "Noch einmal anlegen?", "Anlegen?", JOptionPane.YES_NO_OPTION);
                    if (ret != JOptionPane.OK_OPTION) {
                        return;
                    }
                }
                // weiter
                DialogAddDownload dialog = new DialogAddDownload(MediathekGui.ui(), film, null, "");
                dialog.setVisible(true);
            }

        }

        class BeobInfo implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final InfoDialog dlg = MediathekGui.ui().getFilmInfoDialog();
                dlg.updateCurrentFilm(film);
                dlg.showInfo();
            }
        }

        class BeobUrl implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrlModelHelper.USED_URL_URL).toString();
                    GuiFunktionen.copyToClipboard(del);
                }
            }
        }

        private class BeobLoeschen implements ActionListener {

            @Override
            public void actionPerformed(ActionEvent e) {
                final int selectedTableRow = jTable1.getSelectedRow();
                if (selectedTableRow != -1) {
                    String del = jTable1.getValueAt(jTable1.convertRowIndexToModel(selectedTableRow), MVUsedUrlModelHelper.USED_URL_URL).toString();
                    workList.urlAusLogfileLoeschen(del);
                }
            }
        }
    }

    // <editor-fold defaultstate="collapsed" desc="Generated Code">//GEN-BEGIN:initComponents
    // Generated using JFormDesigner non-commercial license
    private void initComponents() {
        var jScrollPane1 = new JScrollPane();
        jTable1 = new JTable();
        var panel1 = new JPanel();
        jToggleButtonLaden = new JToggleButton();
        jButtonExport = new JButton();
        jLabelSum = new JLabel();

        //======== this ========
        setLayout(new MigLayout(
            new LC().insets("5").hideMode(3).gridGap("0", "0"), //NON-NLS
            // columns
            new AC()
                .grow().fill(),
            // rows
            new AC()
                .grow().fill().gap()
                .fill()));

        //======== jScrollPane1 ========
        {
            jScrollPane1.setViewportView(jTable1);
        }
        add(jScrollPane1, new CC().cell(0, 0));

        //======== panel1 ========
        {
            panel1.setLayout(new MigLayout(
                new LC().insets("5 5 0 5").hideMode(3).gridGap("5", "5"), //NON-NLS
                // columns
                new AC()
                    .fill().gap()
                    .grow().fill().gap()
                    .fill(),
                // rows
                new AC()
                    .fill()));

            //---- jToggleButtonLaden ----
            jToggleButtonLaden.setText("Laden"); //NON-NLS
            panel1.add(jToggleButtonLaden, new CC().cell(0, 0));

            //---- jButtonExport ----
            jButtonExport.setText("Liste exportieren"); //NON-NLS
            panel1.add(jButtonExport, new CC().cell(2, 0));
            panel1.add(jLabelSum, new CC().cell(1, 0));
        }
        add(panel1, new CC().cell(0, 1));
    }// </editor-fold>//GEN-END:initComponents
    // Variables declaration - do not modify//GEN-BEGIN:variables
    // Generated using JFormDesigner non-commercial license
    protected JTable jTable1;
    protected JToggleButton jToggleButtonLaden;
    private JButton jButtonExport;
    private JLabel jLabelSum;
    // End of variables declaration//GEN-END:variables
}
