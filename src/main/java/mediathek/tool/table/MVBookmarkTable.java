package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.config.MVConfig.Configs;
import mediathek.daten.DatenFilm;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.tool.table.MVFilmTable.MyRowSorter;
import org.jetbrains.annotations.NotNull;
import javax.swing.table.TableModel;
import java.util.Optional;

public class MVBookmarkTable extends MVTable{
    private MyRowSorter<TableModel> sorter;
    public MVBookmarkTable() {

        super(DatenFilm.MAX_ELEM, GuiFilme.VISIBLE_COLUMNS,
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN),
                Optional.of(MVConfig.Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_FILME));

        setAutoCreateRowSorter(false);
        addPropertyChangeListener("model", evt -> {
            //we need to setup sorter later as the model is invalid at ctor point...
            var model = (TableModel) evt.getNewValue();
            if (sorter == null) {
                sorter = new MyRowSorter<>(model);
                sorter.setModel(model);
                setRowSorter(sorter);
            }
            else
                sorter.setModel(model);
        });
    }

    @Override
    protected void spaltenAusschalten() {

    }

}
