package mediathek.tool.table;

import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.daten.DatenDownload;
import mediathek.tool.MVFont;
import mediathek.tool.TModel;
import mediathek.tool.TModelDownload;

import javax.activation.DataHandler;
import javax.swing.*;
import java.awt.*;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.Transferable;
import java.awt.dnd.DragSource;
import java.util.ArrayList;

public class MVDownloadsTable extends MVTable {
    private static final long serialVersionUID = 6933494912098350123L;
    private final Daten daten = Daten.getInstance();

    @Override
    protected void setupTableType() {
        maxSpalten = DatenDownload.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(DatenDownload.spaltenAnzeigen, DatenDownload.MAX_ELEM);
        indexSpalte = DatenDownload.DOWNLOAD_NR;
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_DOWNLOADS;
        iconAnzeigenStr = MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_ANZEIGEN;
        iconKleinStr = MVConfig.Configs.SYSTEM_TAB_DOWNLOAD_ICON_KLEIN;

        setupDragnDrop();

        setModel(new TModelDownload(new Object[][]{}, DatenDownload.COLUMN_NAMES));
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
            case DatenDownload.DOWNLOAD_NR:
            case DatenDownload.DOWNLOAD_FILM_NR:
                breite[i] = 75;
                break;
            case DatenDownload.DOWNLOAD_BUTTON_START:
            case DatenDownload.DOWNLOAD_BUTTON_DEL:
            case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
            case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
            case DatenDownload.DOWNLOAD_UNTERBROCHEN:
            case DatenDownload.DOWNLOAD_SPOTLIGHT:
            case DatenDownload.DOWNLOAD_SUBTITLE:
            case DatenDownload.DOWNLOAD_INFODATEI:
            case DatenDownload.DOWNLOAD_HD:
            case DatenDownload.DOWNLOAD_UT:
                breite[i] = 50;
                break;
            case DatenDownload.DOWNLOAD_TITEL:
                breite[i] = 250;
                break;
            case DatenDownload.DOWNLOAD_ABO:
            case DatenDownload.DOWNLOAD_THEMA:
                breite[i] = 150;
                break;
            case DatenDownload.DOWNLOAD_DATUM:
            case DatenDownload.DOWNLOAD_ZEIT:
            case DatenDownload.DOWNLOAD_GROESSE:
            case DatenDownload.DOWNLOAD_BANDBREITE:
            case DatenDownload.DOWNLOAD_SENDER:
            case DatenDownload.DOWNLOAD_PROGRESS:
            case DatenDownload.DOWNLOAD_RESTZEIT:
            case DatenDownload.DOWNLOAD_DAUER:
            case DatenDownload.DOWNLOAD_GEO:
                breite[i] = 100;
                break;
        }
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            switch (i) {
                case DatenDownload.DOWNLOAD_FILM_URL:
                case DatenDownload.DOWNLOAD_URL_RTMP:
                case DatenDownload.DOWNLOAD_URL_SUBTITLE:
                case DatenDownload.DOWNLOAD_PROGRAMM:
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF:
                case DatenDownload.DOWNLOAD_PROGRAMM_AUFRUF_ARRAY:
                case DatenDownload.DOWNLOAD_PROGRAMM_RESTART:
                case DatenDownload.DOWNLOAD_PROGRAMM_DOWNLOADMANAGER:
                case DatenDownload.DOWNLOAD_ZIEL_DATEINAME:
                case DatenDownload.DOWNLOAD_ZIEL_PFAD:
                case DatenDownload.DOWNLOAD_ART:
                case DatenDownload.DOWNLOAD_QUELLE:
                case DatenDownload.DOWNLOAD_ZURUECKGESTELLT:
                case DatenDownload.DOWNLOAD_HISTORY_URL:
                case DatenDownload.DOWNLOAD_REF:
                case DatenDownload.DOWNLOAD_SPOTLIGHT:
                case DatenDownload.DOWNLOAD_INFODATEI:
                case DatenDownload.DOWNLOAD_SUBTITLE:
                case DatenDownload.DOWNLOAD_UNTERBROCHEN:
                    breite[i] = 0;
                    break;
            }
        }
    }

    @Override
    protected int getSizeArea() {
        int sizeArea = 0;

        if (lineBreak) {
            sizeArea = MVFont.fontSize * 4;
        }

        return sizeArea;
    }

    @Override
    public void getSelected() {
        super.getSelected();

        int selIndex = -1;
        if (selRow >= 0) {
            selIndex = (int) getModel().getValueAt(convertRowIndexToModel(selRow), indexSpalte);
        }

        if (selIndex >= 0) {
            selIndexes = new int[selRows.length];
            int k = 0;
            for (int i : selRows) {
                selIndexes[k++] = (int) getModel().getValueAt(convertRowIndexToModel(i), indexSpalte);
            }
        } else {
            selIndexes = null;
        }
    }

    @Override
    protected void setSelected() {
        boolean found = false;

        if (selIndexes != null) {
            int r;
            selectionModel.setValueIsAdjusting(true);
            TModel tModel = (TModel) getModel();
            for (int i : selIndexes) {
                r = tModel.getIdxRow(i);
                if (r >= 0) {
                    // ansonsten gibts die Zeile nicht mehr
                    r = convertRowIndexToView(r);
                    addRowSelectionInterval(r, r);
                    found = true;
                }
            }
            if (!found && selRow >= 0 && this.getRowCount() > selRow) {
                // große Frage was da besser ist???
                for (int i = selRow; i >= 0; --i) {
                    setRowSelectionInterval(i, i);
                    break;
                }
            } else if (!found && selRow >= 0 && this.getRowCount() > 0) {
                setRowSelectionInterval(tModel.getRowCount() - 1, tModel.getRowCount() - 1);
            }
            selectionModel.setValueIsAdjusting(false);
        }
        selIndexes = null;
    }

    private class TableRowTransferHandlerDownload extends TransferHandler {
        private static final long serialVersionUID = -2351971599010859779L;
        private final DataFlavor localObjectFlavor = new DataFlavor(Integer.class, "Integer Row Index");
        private final JTable table;
        private int[] transferedRows = null;

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
                e.printStackTrace();
            }
            return true;
        }

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
                TModel tModel = (TModel) table.getModel();
                int max = tModel.getRowCount();
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
                ex.printStackTrace();
            }
            return false;
        }

        private void reorder(int index, int[] rowFrom) {
            getSelected();

            final TModel tModel = (TModelDownload) getModel();
            // listeDownloads neu nach der Reihenfolge in der Tabelle erstellen
            for (int i = 0; i < getRowCount(); ++i) {
                DatenDownload d = ((DatenDownload) tModel.getValueAt(convertRowIndexToModel(i), DatenDownload.DOWNLOAD_REF));
                if (d != null) {
                    daten.getListeDownloads().remove(d);
                    daten.getListeDownloads().add(d);
                }
            }
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
            // an der richtigen Stellei einfügen
            daten.getListeDownloads().addAll(index, liste);
            // die Tabellensortierung löschen, die wird jetzt mit der Liste wieder gefüllt
            getRowSorter().setSortKeys(null);
            setRowSorter(null);
            setAutoCreateRowSorter(true);
            setSelected();
            Listener.notify(Listener.EREIGNIS_REIHENFOLGE_DOWNLOAD, MVTable.class.getSimpleName());
        }

        @Override
        protected void exportDone(JComponent c, Transferable t, int act) {
            if (act == TransferHandler.MOVE) {
                table.setCursor(Cursor.getPredefinedCursor(Cursor.DEFAULT_CURSOR));
            }
        }
    }
}
