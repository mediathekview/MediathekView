package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.config.MVConfig.Configs;
import mediathek.daten.DatenFilm;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.gui.tabs.tab_film.GuiFilme;
import java.util.Optional;

/*
    Created by: Markus
    Created at: 31.03.2025
*/
public class MVBookmarkTable extends MVTable{

    public MVBookmarkTable(){
        super(DatenBookmark.MAX_ELEM, DatenBookmark.spaltenAnzeigen,
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN),
                Optional.of(Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_BOOKMARK));
    }

    @Override
    protected void spaltenAusschalten() {

    }

}
