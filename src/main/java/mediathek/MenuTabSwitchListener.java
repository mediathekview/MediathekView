package mediathek;

import mediathek.tool.TABS;

import javax.swing.*;
import javax.swing.event.MenuEvent;
import javax.swing.event.MenuListener;
import java.awt.*;

class MenuTabSwitchListener implements MenuListener {

    private final MediathekGui mediathekGui;
    private final TABS tabs;
    private final JTabbedPane tabbedPane;

    MenuTabSwitchListener(MediathekGui mediathekGui, TABS tabs) {
        this.mediathekGui = mediathekGui;
        this.tabs = tabs;
        tabbedPane = mediathekGui.getTabbedPane();

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

    private void findTab(TABS state) {
        switch (state) {
            case TAB_FILME:
                setTabIfContain(mediathekGui.tabFilme);
                break;
            case TAB_DOWNLOADS:
                setTabIfContain(mediathekGui.tabDownloads);
                break;

            default:
                break;
        }
    }

    private void setTabIfContain(Component check) {
        for (int i = 0; i < tabbedPane.getTabCount(); ++i) {
            Component c = tabbedPane.getComponentAt(i);
            if (c.equals(check)) {
                tabbedPane.setSelectedIndex(i);
                return;
            }
        }
    }
}
