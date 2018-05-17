package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.daten.DatenAbo;
import mediathek.tool.MVFont;
import mediathek.tool.TModel;
import mediathek.tool.TModelAbo;

public class MVAbosTable extends MVTable {
    private static final long serialVersionUID = 679215520194372998L;

    @Override
    protected void setupTableType() {
        maxSpalten = DatenAbo.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(DatenAbo.spaltenAnzeigen, DatenAbo.MAX_ELEM);
        indexSpalte = DatenAbo.ABO_NR;
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_ABOS;
        iconAnzeigenStr = MVConfig.Configs.SYSTEM_TAB_ABO_ICON_ANZEIGEN;
        iconKleinStr = MVConfig.Configs.SYSTEM_TAB_ABO_ICON_KLEIN;

        setModel(new TModelAbo(new Object[][]{}, DatenAbo.COLUMN_NAMES));
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            resetAbosTab(i);
        }

        super.resetTabelle();
    }

    private void resetAbosTab(int i) {
        reihe[i] = i;
        breite[i] = 200;
        if (i == DatenAbo.ABO_NR
                || i == DatenAbo.ABO_EINGESCHALTET
                || i == DatenAbo.ABO_MIN) {
            breite[i] = 75;
        } else if (i == DatenAbo.ABO_DOWN_DATUM
                || i == DatenAbo.ABO_SENDER) {
            breite[i] = 100;
        }
    }

    private void spaltenAusschaltenAbos(int i) {
        if (i == DatenAbo.ABO_ZIELPFAD
                || i == DatenAbo.ABO_PSET
                || i == DatenAbo.ABO_MINDESTDAUER
                || i == DatenAbo.ABO_MIN
                || i == DatenAbo.ABO_DOWN_DATUM) {
            breite[i] = 0;
        }
    }

    @Override
    protected void spaltenAusschalten() {
        for (int i = 0; i < maxSpalten; ++i) {
            spaltenAusschaltenAbos(i);
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
        // gemerkte Einstellungen der Tabelle wieder setzten
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
                //TODO was soll die "loop" machen?
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
}
