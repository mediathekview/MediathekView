package mediathek.tool.table;

import mediathek.config.MVConfig;
import mediathek.config.MVConfig.Configs;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.gui.bookmark.BookmarkDialogSwing;
import java.util.Optional;

public class MVBookmarkTable extends MVTable{
    public MVBookmarkTable() {

        super(DatenBookmark.MAX_ELEM, BookmarkDialogSwing.VISIBLE_COLUMNS,
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_ANZEIGEN),
                Optional.of(MVConfig.Configs.SYSTEM_TAB_FILME_ICON_KLEIN),
                Optional.of(Configs.SYSTEM_EIGENSCHAFTEN_TABELLE_BOOKMARK));
    }

    @Override
    protected void spaltenAusschalten() {

    }

}
