package mediathek.gui.tools;

import mediathek.MediathekGui;
import mediathek.config.Daten;

import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import java.awt.*;

/**
 * Switches to the appropriate tab based on menu selection
 */
public class AutomaticTabSwitcherMenuListener implements MenuListener {

    private final MediathekGui.TABS tabs;
    private final JTabbedPane tabPane;

    public AutomaticTabSwitcherMenuListener(JTabbedPane jTabbedPane, MediathekGui.TABS tabs) {
        this.tabs = tabs;
        this.tabPane = jTabbedPane;
    }

    @Override
    public void menuSelected(MenuEvent e) {
        findTab(tabs);
    }

    @Override
    public void menuDeselected(MenuEvent e) {
    }

    @Override
    public void menuCanceled(MenuEvent e) {
    }

    private void findTab(MediathekGui.TABS state) {
        switch (state) {
            case NIX:
                break;
            case FILME:
                setTabIfContain(Daten.guiFilme);
                break;
            case DOWNLOADS:
                setTabIfContain(Daten.guiDownloads);
                break;
            case ABOS:
                setTabIfContain(Daten.guiAbo);
                break;
        }
    }

    private void setTabIfContain(Component check) {
        for (int i = 0; i < tabPane.getTabCount(); ++i) {
            Component c = tabPane.getComponentAt(i);
            if (c.equals(check)) {
                tabPane.setSelectedIndex(i);
                return;
            }
        }
    }
}
