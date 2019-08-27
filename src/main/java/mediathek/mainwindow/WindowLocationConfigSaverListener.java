package mediathek.mainwindow;

import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

class WindowLocationConfigSaverListener extends ComponentAdapter {
    @Override
    public void componentResized(ComponentEvent e) {
        JFrame mainWindow = (JFrame)e.getComponent(); //we know it´s our main window

        var config = ApplicationConfiguration.getConfiguration();
        var dims = mainWindow.getSize();

        //safety check if the window gets too small..on some systems it is really hard to get it bigger again. (Linux)
        if (dims.width < 320)
            dims.width = 320;
        if (dims.height < 240)
            dims.height = 240;

        try {
            config.lock(LockMode.WRITE);
            final boolean isMaximized = (mainWindow.getExtendedState() & JFrame.MAXIMIZED_BOTH) == JFrame.MAXIMIZED_BOTH;
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_MAXIMIZED, isMaximized);

            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_WIDTH, dims.width);
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_HEIGHT, dims.height);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    @Override
    public void componentMoved(ComponentEvent e) {
        JFrame mainWindow = (JFrame)e.getComponent(); //we know it´s our main window

        var config = ApplicationConfiguration.getConfiguration();
        var pt = mainWindow.getLocation();

        try {
            config.lock(LockMode.WRITE);
            final boolean isMaximized = (mainWindow.getExtendedState() & JFrame.MAXIMIZED_BOTH) == JFrame.MAXIMIZED_BOTH;
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_MAXIMIZED, isMaximized);

            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_X, pt.x);
            config.setProperty(ApplicationConfiguration.APPLICATION_UI_MAINWINDOW_LOCATION_Y, pt.y);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }
}
