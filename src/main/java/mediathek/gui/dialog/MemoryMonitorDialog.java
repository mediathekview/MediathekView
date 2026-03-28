package mediathek.gui.dialog;

import mediathek.mainwindow.MemoryUsagePanel;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.Configuration;
import org.apache.commons.configuration2.sync.LockMode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.WindowAdapter;
import java.awt.event.WindowEvent;
import java.time.Duration;
import java.util.Optional;

public final class MemoryMonitorDialog extends JDialog {
    private static final Duration HISTORY_WINDOW = Duration.ofMinutes(2);
    private static final Duration SAMPLE_INTERVAL = Duration.ofSeconds(1);
    private static final Dimension DEFAULT_SIZE = new Dimension(480, 240);

    private final Configuration configuration = ApplicationConfiguration.getConfiguration();
    private final MemoryUsagePanel memoryUsagePanel = new MemoryUsagePanel(HISTORY_WINDOW, SAMPLE_INTERVAL);

    public MemoryMonitorDialog(@NotNull JFrame parent) {
        super(parent, "Speicherverbrauch", false);

        setType(Type.UTILITY);
        setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);

        memoryUsagePanel.setPreferredSize(DEFAULT_SIZE);
        add(memoryUsagePanel, BorderLayout.CENTER);
        pack();
        restoreBounds();

        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent event) {
                storeBounds();
            }

            @Override
            public void componentMoved(ComponentEvent event) {
                storeBounds();
            }
        });
        addWindowListener(new WindowAdapter() {
            @Override
            public void windowOpened(WindowEvent event) {
                storeVisibility(true);
            }

            @Override
            public void windowClosed(WindowEvent event) {
                storeVisibility(false);
            }
        });
    }

    @Override
    public void dispose() {
        memoryUsagePanel.close();
        super.dispose();
    }

    private void restoreBounds() {
        readStoredBounds().ifPresentOrElse(this::applyBounds, this::applyDefaultBounds);
    }

    private Optional<DialogBounds> readStoredBounds() {
        configuration.lock(LockMode.READ);
        try {
            var width = configuration.getInt(ApplicationConfiguration.MemoryMonitorDialog.WIDTH, -1);
            var height = configuration.getInt(ApplicationConfiguration.MemoryMonitorDialog.HEIGHT, -1);
            var x = configuration.getInt(ApplicationConfiguration.MemoryMonitorDialog.X, Integer.MIN_VALUE);
            var y = configuration.getInt(ApplicationConfiguration.MemoryMonitorDialog.Y, Integer.MIN_VALUE);

            if (width <= 0 || height <= 0 || x == Integer.MIN_VALUE || y == Integer.MIN_VALUE) {
                return Optional.empty();
            }

            return Optional.of(new DialogBounds(x, y, width, height));
        } finally {
            configuration.unlock(LockMode.READ);
        }
    }

    private void applyBounds(DialogBounds bounds) {
        setBounds(bounds.x(), bounds.y(), bounds.width(), bounds.height());
    }

    private void applyDefaultBounds() {
        setSize(DEFAULT_SIZE);
        setLocationRelativeTo(getOwner());
    }

    private void storeBounds() {
        if (!isShowing()) {
            return;
        }

        var bounds = getBounds();
        configuration.lock(LockMode.WRITE);
        try {
            configuration.setProperty(ApplicationConfiguration.MemoryMonitorDialog.X, bounds.x);
            configuration.setProperty(ApplicationConfiguration.MemoryMonitorDialog.Y, bounds.y);
            configuration.setProperty(ApplicationConfiguration.MemoryMonitorDialog.WIDTH, bounds.width);
            configuration.setProperty(ApplicationConfiguration.MemoryMonitorDialog.HEIGHT, bounds.height);
        } finally {
            configuration.unlock(LockMode.WRITE);
        }
    }

    private void storeVisibility(boolean visible) {
        configuration.setProperty(ApplicationConfiguration.MemoryMonitorDialog.VISIBLE, visible);
    }

    private record DialogBounds(int x, int y, int width, int height) {
    }
}
