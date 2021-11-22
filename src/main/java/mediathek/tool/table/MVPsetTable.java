package mediathek.tool.table;

import mediathek.daten.DatenPset;
import mediathek.tool.models.TModel;

public class MVPsetTable extends MVTable {
    @Override
    protected void setupTableType() {
        maxSpalten = DatenPset.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(DatenPset.spaltenAnzeigen, DatenPset.MAX_ELEM);

        setModel(new TModel(new Object[][]{}, DatenPset.COLUMN_NAMES));
        setRowSorter(null);
        setAutoCreateRowSorter(false); // Reihenfolge ist die Anzeige der Button!
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }

}
