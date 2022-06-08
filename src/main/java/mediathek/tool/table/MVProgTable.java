package mediathek.tool.table;

import mediathek.daten.DatenProg;
import mediathek.tool.models.TModel;

import java.util.Optional;

public class MVProgTable extends MVTable {
    public MVProgTable() {
        super(DatenProg.MAX_ELEM, new boolean[DatenProg.MAX_ELEM],
                Optional.empty(),
                Optional.empty(),
                Optional.empty());

        setModel(new TModel(new Object[][]{}, DatenProg.COLUMN_NAMES));
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
