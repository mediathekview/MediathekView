package mediathek.tool.table;

import mediathek.daten.DatenPset;
import mediathek.tool.models.TModel;

public class MVPsetTable extends MVTable {
    private static final long serialVersionUID = 7582553351667887172L;

    public MVPsetTable() {
        super(DatenPset.MAX_ELEM);
    }

    @Override
    protected void setupTableType() {
        spaltenAnzeigen = activateAllColumns(DatenPset.spaltenAnzeigen);

        setModel(new TModel(new Object[][]{}, DatenPset.COLUMN_NAMES));
        setRowSorter(null);
        setAutoCreateRowSorter(false); // Reihenfolge ist die Anzeige der Button!
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }

}
