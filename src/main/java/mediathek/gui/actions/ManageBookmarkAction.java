package mediathek.gui.actions;

import mediathek.config.Daten;
import mediathek.daten.bookmark.DatenBookmark;
import mediathek.daten.bookmark.ListeBookmark;
import mediathek.gui.bookmark.BookmarkDialogSwing;
import mediathek.javafx.bookmark.BookmarkData;
import mediathek.javafx.bookmark.BookmarkDataList;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SVGIconUtilities;

import javax.swing.*;
import java.awt.event.ActionEvent;

public class ManageBookmarkAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public ManageBookmarkAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.NAME, "Merkliste verwalten...");
        putValue(Action.SMALL_ICON, SVGIconUtilities.createSVGIcon("icons/fontawesome/file-lines.svg"));
        putValue(Action.SHORT_DESCRIPTION, "Merkliste verwalten");
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        BookmarkDialogSwing dialog = new BookmarkDialogSwing(mediathekGui, false);
        dialog.setVisible(true);
    }
}
