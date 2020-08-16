package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.daten.DatenMediaDB;
import mediathek.tool.models.NonEditableTableModel;

public class MVMediaDbTable extends MVTable {
    private static final long serialVersionUID = 5220131462717851342L;

    @Override
    protected void setupTableType() {
        maxSpalten = DatenMediaDB.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(new boolean[DatenMediaDB.MAX_ELEM], DatenMediaDB.MAX_ELEM);
        nrDatenSystem = MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_MEDIA_DB;

        setModel(new NonEditableTableModel(new Object[][]{}, DatenMediaDB.COLUMN_NAMES));
    }

    @Override
    public void resetTabelle() {
        for (int i = 0; i < maxSpalten; ++i) {
            reihe[i] = i;
            breite[i] = 200;
        }

        super.resetTabelle();
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }

    @Override
    protected void setSelected() {
        //do nothing
    }
}
