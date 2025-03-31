package mediathek.tool.table;

import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.gui.messages.DownloadQueueRankChangedEvent;
import mediathek.tool.MessageBus;
import mediathek.tool.models.TModelDownload;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;

import javax.activation.DataHandler;
import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DragSource;
import java.awt.event.MouseEvent;
import java.util.ArrayList;
import java.util.Optional;

public class MVDownloadsTable extends MVTable {
    public MVDownloadsTable() {
        super(DatenDownload.MAX_ELEM, DatenDownload.spaltenAnzeigen,
                Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_KLEIN),
                Optional.of(MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS));

        setupDragnDrop();

        setModel(new TModelDownload());
    }

    @Override public String getToolTipText(MouseEvent e) {
        var p = e.getPoint(); // MouseEvent
        final int viewColumn = columnAtPoint(p);
        final int modelColumnIndex = convertColumnIndexToModel(viewColumn);

        //only show title as tooltip for TITEL column...
        if (modelColumnIndex != DatenDownload.DOWNLOAD_TITEL)
            return super.getToolTipText(e);

        String toolTipText = null;
        final int viewRow = rowAtPoint(p);
        var comp = prepareRenderer(getCellRenderer(viewRow, viewColumn), viewRow, viewColumn);
        var bounds = getCellRect(viewRow, viewColumn, false);


        try {
            //comment row, exclude heading
            if (comp.getPreferredSize().width > bounds.width) {
                final int modelRowIndex = convertRowIndexToModel(viewRow);
                final DatenDownload datenDownload = (DatenDownload) getModel().getValueAt(modelRowIndex, DatenDownload.DOWNLOAD_REF);
                if (datenDownload.film != null)
                    toolTipText = datenDownload.film.getTitle();
                else
                    toolTipText = "";
            }
        } catch (RuntimeException ignored) {
            //catch null pointer exception if mouse is over an empty line
        }

        return toolTipText;
    }

    private void setupDragnDrop() {
        setDragEnabled(true);
        setDropMode(DropMode.INSERT_ROWS);
        setTransferHandler(new TableRowTransferHandlerDownload(this));
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetDownloadsTab(i);
        }

        super.resetTabelle();
    }

    private void resetDownloadsTab(int i) {
        reihe[i] = i;
        breite[i] = 200;
        switch (i) {
            case DatenDownload.DOWNLOAD_NR, DatenDownload.DOWNLOAD_FILM_NR -> breite[i] = 75;
            case DatenDownload.DOWNLOAD_BUTTON_START, DatenDownload.DOWNLOAD_BUTTON_DEL,
                    DatenDownload.DOWNLOAD_PROGRAMM_RESTART, DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER,
                    DatenDownload.DOWNLOAD_UNTERBROCHEN, DatenDownload.DOWNLOAD_SPOTLIGHT,
                    DatenDownload.DOWNLOAD_SUBTITLE, DatenDownload.DOWNLOAD_INFODATEI,
                    DatenDownload.DOWNLOAD_HD, DatenDownload.DOWNLOAD_UT -> breite[i] = 50;
            case DatenDownload.DOWNLOAD_TITEL -> breite[i] = 250;
            case DatenDownload.DOWNLOAD_ABO, DatenDownload.DOWNLOAD_THEMA -> breite[i] = 150;
            case DatenDownload.DOWNLOAD_DATUM, DatenDownload.DOWNLOAD_ZEIT, DatenDownload.DOWNLOAD_GROESSE,
                    DatenDownload.DOWNLOAD_BANDBREITE, DatenDownload.DOWNLOAD_SENDER, DatenDownload.DOWNLOAD_PROGRESS,
                    DatenDownload.DOWNLOAD_RESTZEIT, DatenDownload.DOWNLOAD_DAUER,
                    DatenDownload.DOWNLOAD_GEO -> breite[i] = 100;
        }
    }

    /**
     * Don´t know exactly why this is actually called or needed...
     * but it sorts the list of downloads to be the same as the view order of this table.
     */
    public synchronized void sortDownloadListByTableRows()
    {
        final var rowCount = getRowCount();
        final var tableModel = getModel();
        final var listeDownloads = Daten.getInstance().getListeDownloads();

        for (int i = 0; i < rowCount; ++i) {
            DatenDownload datenDownload = (DatenDownload) tableModel.getValueAt(convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF);
            listeDownloads.remove(datenDownload);
            listeDownloads.add(datenDownload);
        }
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            switch (i) {
                case DatenDownload.DOWNLOAD_FILM_URL, DatenDownload.DOWNLOAD_URL_RTMP,
                        DatenDownload.DOWNLOAD_URL_SUBTITLE, DatenDownload.DOWNLOAD_PROGRAMM,
                        DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF, DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY,
                        DatenDownload.DOWNLOAD_PROGRAMM_RESTART, DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER,
                        DatenDownload.DOWNLOAD_ZIEL_DATEINAME, DatenDownload.DOWNLOAD_ZIEL_PFAD,
                        DatenDownload.DOWNLOAD_ART, DatenDownload.DOWNLOAD_QUELLE, DatenDownload.DOWNLOAD_ZURUECKGESTELLT,
                        DatenDownload.DOWNLOAD_HISTORY_URL, DatenDownload.DOWNLOAD_REF, DatenDownload.DOWNLOAD_SPOTLIGHT,
                        DatenDownload.DOWNLOAD_INFODATEI, DatenDownload.DOWNLOAD_SUBTITLE,
                        DatenDownload.DOWNLOAD_UNTERBROCHEN -> breite[i] = 0;
            }
        }
    }

    private class TableRowTransferHandlerDownload extends TransferHandler {
        private final DataFlavor localObjectFlavor = new DataFlavor(Integer.class, "Integer Row Index");
        private final JTable table;
        private int[] transferedRows;

        public TableRowTransferHandlerDownload(JTable table) {
            this.table = table;
        }

        @Override
        protected Transferable createTransferable(JComponent c) {
            assert (c.equals(table));
            transferedRows = table.getSelectedRows();
            return new DataHandler(table.getSelectedRow(), localObjectFlavor.getMimeType());
        }

        @Override
        public boolean canImport(TransferHandler.TransferSupport info) {
            try {
                boolean b = info.getComponent() == table && info.isDrop() && info.isDataFlavorSupported(localObjectFlavor);
                table.setCursor(b ? DragSource.DefaultMoveDrop : DragSource.DefaultMoveNoDrop);
                return b;
                // here's the problem
                // canImport is called during drags AND before drop is accepted
            } catch (Exception e) {
                logger.error("canImport", e);
            }
            return true;
        }

        private static final Logger logger = LogManager.getLogger();

        @Override
        public int getSourceActions(JComponent c) {
            return TransferHandler.COPY_OR_MOVE;
        }

        @Override
        public boolean importData(TransferHandler.TransferSupport info) {
            try {
                JTable target = (JTable) info.getComponent();
                JTable.DropLocation dl = (JTable.DropLocation) info.getDropLocation();
                int index = dl.getRow();
                int max = table.getModel().getRowCount();
                if (index < 0 || index > max) {
                    index = max;
                }
                target.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
                if (transferedRows != null) {
                    reorder(index, transferedRows);
                    transferedRows = null;
                    return true;
                }
            } catch (Exception ex) {
                logger.error("importData", ex);
            }
            return false;
        }

        private void reorder(int index, int[] rowFrom) {
            saveSelectedTableRows();

            final var daten = Daten.getInstance();
            final var tModel = (TModelDownload) getModel();
            // listeDownloads neu nach der Reihenfolge in der Tabelle erstellen
            sortDownloadListByTableRows();

            // Downloads zum Verschieben suchen
            ArrayList<DatenDownload> liste = new ArrayList<>();
            for (int row : rowFrom) {
                if (index > row) {
                    --index;
                }
                final DatenDownload d = ((DatenDownload) tModel.getValueAt(convertRowIndexToModel(row), DatenDownload.DOWNLOAD_REF));
                liste.add(d);
                daten.getListeDownloads().remove(d);
            }

            // an der richtigen Stelle einfügen
            daten.getListeDownloads().addAll(index, liste);
            // die Tabellensortierung löschen, die wird jetzt mit der Liste wieder gefüllt
            getRowSorter().setSortKeys(null);
            setRowSorter(null);
            setAutoCreateRowSorter(true);
            restoreSelectedTableRows();

            MessageBus.getMessageBus().publishAsync(new DownloadQueueRankChangedEvent());
        }

        @Override
        protected void exportDone(JComponent c, Transferable t, int act) {
            if (act == TransferHandler.MOVE) {
                table.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
        }
    }
}
