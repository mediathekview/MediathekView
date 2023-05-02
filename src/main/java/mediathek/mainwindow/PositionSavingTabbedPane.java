package mediathek.mainwindow;

import mediathek.tool.ApplicationConfiguration;

import javax.swing.*;

public class PositionSavingTabbedPane extends JTabbedPane {

    public PositionSavingTabbedPane() {
    }

    public void installChangeListener() {
        addChangeListener(l -> saveTabPosition());
    }

    public void restoreSavedTabPosition() {
        var idx = ApplicationConfiguration.getConfiguration().getInt(Config.TAB_POSITION, -1);
        setSelectedIndex(Math.max(idx, 0));
    }

    private void saveTabPosition() {
        ApplicationConfiguration.getConfiguration().setProperty(Config.TAB_POSITION, getSelectedIndex());
    }

    private static class Config {
        public static final String TAB_POSITION = "app.ui.tab_position";
    }
}
