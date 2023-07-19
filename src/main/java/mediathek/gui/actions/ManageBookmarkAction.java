package mediathek.gui.actions;

import javafx.application.Platform;
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
        Platform.runLater(() -> mediathekGui.tabFilme.showBookmarkWindow());
    }
}
