package mediathek.tool.table;

import mediathek.daten.DatenPset;
import mediathek.tool.models.TModel;

import java.util.Optional;

public class MVPsetTable extends MVTable {
    public MVPsetTable() {
        super(DatenPset.MAX_ELEM, new boolean[DatenPset.MAX_ELEM],
                Optional.empty(),
                Optional.empty(),
                Optional.empty());

        setModel(new TModel(new Object[][]{}, DatenPset.COLUMN_NAMES));
        setRowSorter(null);
        setAutoCreateRowSorter(false); // Reihenfolge ist die Anzeige der Button!
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }

    @Override
    public void calculateRowHeight() {
        // do nothing
    }
}
