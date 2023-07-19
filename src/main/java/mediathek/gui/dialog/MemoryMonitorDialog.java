package mediathek.gui.dialog;

import mediathek.mainwindow.MemoryUsagePanel;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.NoSuchElementException;
import java.util.concurrent.TimeUnit;

public class MemoryMonitorDialog extends JDialog {
    public MemoryMonitorDialog(@NotNull JFrame parent) {
        super(parent, "Speicherverbrauch", false);
        setType(Type.UTILITY);

        MemoryUsagePanel panel = new MemoryUsagePanel(2, TimeUnit.MINUTES);
        panel.setPreferredSize(new Dimension(480, 240));
        getContentPane().add(panel, BorderLayout.CENTER);
        pack();
        panel.new MemoryUsageDataGenerator(1, TimeUnit.SECONDS).start();

        restoreLocation();

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                saveLocation();
            }

            @Override
            public void componentMoved(ComponentEvent e) {
                saveLocation();
            }

            @Override
            public void componentShown(ComponentEvent e) {
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.MemoryMonitorDialog.VISIBLE, true);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.MemoryMonitorDialog.VISIBLE, false);
            }
        });
    }

    private void restoreLocation() {
        var config = ApplicationConfiguration.getConfiguration();
        config.lock(LockMode.READ);
        try {
            var newLocation = new Point();
            newLocation.x = config.getInt(ApplicationConfiguration.MemoryMonitorDialog.X);
            newLocation.y = config.getInt(ApplicationConfiguration.MemoryMonitorDialog.Y);
            setLocation(newLocation);

            int w = config.getInt(ApplicationConfiguration.MemoryMonitorDialog.WIDTH, -1);
            int h = config.getInt(ApplicationConfiguration.MemoryMonitorDialog.HEIGHT, -1);
            if (w != -1 && h != -1) {
                setPreferredSize(new Dimension(w, h));
            }
        } catch (NoSuchElementException ignored) {
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    private void saveLocation() {
        if (!isVisible())
            return;
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.WRITE);
            var location = getLocationOnScreen();
            config.setProperty(ApplicationConfiguration.MemoryMonitorDialog.X, location.x);
            config.setProperty(ApplicationConfiguration.MemoryMonitorDialog.Y, location.y);
            config.setProperty(ApplicationConfiguration.MemoryMonitorDialog.WIDTH, getWidth());
            config.setProperty(ApplicationConfiguration.MemoryMonitorDialog.HEIGHT, getHeight());
        } finally {
            config.unlock(LockMode.WRITE);
        }
    }
}
