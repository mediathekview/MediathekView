package mediathek.gui.actions;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import mediathek.mainwindow.MediathekGui;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;

public class LoadFilmListAction extends AbstractAction {
    private final MediathekGui mediathekGui;

    public static FlatSVGIcon createSVGIcon(String resource, float height) {
        FlatSVGIcon icon = new FlatSVGIcon("icons/fontawesome/cloud-arrow-down.svg");
        float scaleFactor = (1f / icon.getIconHeight()) * 14f;
        return icon.derive(scaleFactor);
    }

    public LoadFilmListAction(MediathekGui mediathekGui) {
        this.mediathekGui = mediathekGui;
        putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_F5, 0));
        putValue(Action.SMALL_ICON, createSVGIcon("icons/fontawesome/cloud-arrow-down.svg", 14f));
        putValue(Action.NAME, "Neue Filmliste laden...");
        putValue(Action.SHORT_DESCRIPTION, "Neue Filmliste laden");
    }
    @Override
    public void actionPerformed(ActionEvent e) {
        mediathekGui.performFilmListLoadOperation(false);
    }
}
