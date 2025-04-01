package mediathek.gui.actions;

import java.awt.event.ActionEvent;
import javax.swing.*;
import mediathek.gui.bookmark.BookmarkDialog;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.SVGIconUtilities;

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

    BookmarkDialog dialogSwing = new BookmarkDialog(mediathekGui);
        dialogSwing.setVisible(true);
    }
}
