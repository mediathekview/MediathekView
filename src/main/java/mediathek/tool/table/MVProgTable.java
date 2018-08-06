package mediathek.tool.table;

import mediathek.daten.DatenProg;
import mediathek.tool.TModel;

public class MVProgTable extends MVTable {
    private static final long serialVersionUID = -2363550088890708511L;

    @Override
    protected void setupTableType() {
        maxSpalten = DatenProg.MAX_ELEM;
        spaltenAnzeigen = getSpaltenEinAus(DatenProg.spaltenAnzeigen, DatenProg.MAX_ELEM);

        setModel(new TModel(new Object[][]{}, DatenProg.COLUMN_NAMES));
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }
}
