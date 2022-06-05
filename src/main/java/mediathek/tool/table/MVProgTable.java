package mediathek.tool.table;

import mediathek.daten.DatenProg;
import mediathek.tool.models.TModel;

public class MVProgTable extends MVTable {
    private static final long serialVersionUID = -2363550088890708511L;

    public MVProgTable() {
        super(DatenProg.MAX_ELEM, DatenProg.spaltenAnzeigen);
    }

    @Override
    protected void setupTableType() {
        setModel(new TModel(new Object[][]{}, DatenProg.COLUMN_NAMES));
    }

    @Override
    protected void spaltenAusschalten() {
        //do nothing
    }
}
