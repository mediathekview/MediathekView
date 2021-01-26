package mediathek.tool.table;

import mediathek.tool.models.TModel;

/**
 * Base functions only used in Abos, Downloads and Film table
 */
public abstract class ASelectableMVTable extends MVTable {
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
                setRowSelectionInterval(selRow, selRow);
            } else if (!found && selRow >= 0 && this.getRowCount() > 0) {
                setRowSelectionInterval(tModel.getRowCount() - 1, tModel.getRowCount() - 1);
            }
            selectionModel.setValueIsAdjusting(false);
        }
        selIndexes = null;
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
}
