package mediathek.mainwindow;

import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;

import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;

class WindowLocationConfigSaverListener extends ComponentAdapter {
    @Override
    public void componentResized(ComponentEvent e) {
        var config = ApplicationConfiguration.getConfiguration();
        var dims = e.getComponent().getSize();
        try {
            config.lock(LockMode.WRITE);
            System.out.println("main window resized to w: " + dims.width + " and h: " + dims.height);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }

    @Override
    public void componentMoved(ComponentEvent e) {
        var config = ApplicationConfiguration.getConfiguration();
        var pt = e.getComponent().getLocation();
        try {
            config.lock(LockMode.WRITE);
            System.out.println("main window moved to x: " + pt.x + " and y: " + pt.y);
        }
        finally {
            config.unlock(LockMode.WRITE);
        }
    }
}
