package mediathek.gui.filterpanel.filterpanel;

import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.configuration2.sync.LockMode;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.awt.event.KeyEvent;
import java.util.NoSuchElementException;

public class OldSwingFilterDialog extends JDialog {
    private final JPanel contentPanel = new JPanel();
    private final JToggleButton filterToggleButton;

    public OldSwingFilterDialog(Frame owner, CommonViewSettingsPaneSwing content, @NotNull JToggleButton filterToggleButton) {
        super(owner);
        this.filterToggleButton = filterToggleButton;

        ToggleVisibilityKeyHandler handler = new ToggleVisibilityKeyHandler(this);
        handler.installHandler(filterToggleButton.getAction());

        setDefaultCloseOperation(HIDE_ON_CLOSE);
        setTitle("Filter");
        setType(Type.UTILITY);
        setContentPane(contentPanel);
        contentPanel.setLayout(new BorderLayout());
        contentPanel.add(content, BorderLayout.CENTER);

        pack();
        restoreWindowSizeFromConfig();
        restoreDialogVisibility();
        addComponentListener(new FilterDialogComponentListener());

        MessageBus.getMessageBus().subscribe(this);

        Daten.getInstance().getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                setEnabled(false);
                contentPanel.setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                setEnabled(true);
                contentPanel.setEnabled(true);
            }
        });
    }

    @Handler
    private void handleTableModelChangeEvent(TableModelChangeEvent e) {
        SwingUtilities.invokeLater(() -> setEnabled(!e.active));
    }

    private void restoreDialogVisibility() {
        var config = ApplicationConfiguration.getConfiguration();
        final boolean visible = config.getBoolean(ApplicationConfiguration.FilterDialog.VISIBLE, false);
        setVisible(visible);
    }

    private void storeDialogVisibility() {
        var config = ApplicationConfiguration.getConfiguration();
        config.setProperty(ApplicationConfiguration.FilterDialog.VISIBLE, isVisible());
    }

    private void restoreWindowSizeFromConfig() {
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            final int width = config.getInt(ApplicationConfiguration.FilterDialog.WIDTH);
            final int height = config.getInt(ApplicationConfiguration.FilterDialog.HEIGHT);
            final int x = config.getInt(ApplicationConfiguration.FilterDialog.X);
            final int y = config.getInt(ApplicationConfiguration.FilterDialog.Y);

            setBounds(x, y, width, height);
        } catch (NoSuchElementException ignored) {
            // do not restore anything
        } finally {
            config.unlock(LockMode.READ);
        }
    }

    static class ToggleVisibilityKeyHandler {
        private static final String TOGGLE_FILTER_VISIBILITY = "toggle_dialog_visibility";
        private final JRootPane rootPane;

        public ToggleVisibilityKeyHandler(JDialog dlg) {
            this.rootPane = dlg.getRootPane();
        }

        public void installHandler(Action action) {
            final var inputMap = rootPane.getInputMap(JComponent.WHEN_IN_FOCUSED_WINDOW);
            inputMap.put(KeyStroke.getKeyStroke(KeyEvent.VK_F12, 0), TOGGLE_FILTER_VISIBILITY);
            rootPane.getActionMap().put(TOGGLE_FILTER_VISIBILITY, action);
        }
    }

    public class FilterDialogComponentListener extends ComponentAdapter {
        @Override
        public void componentResized(ComponentEvent e) {
            storeWindowPosition(e);
        }

        @Override
        public void componentMoved(ComponentEvent e) {
            storeWindowPosition(e);
        }

        @Override
        public void componentShown(ComponentEvent e) {
            storeDialogVisibility();
            filterToggleButton.setSelected(true);
        }

        @Override
        public void componentHidden(ComponentEvent e) {
            storeWindowPosition(e);
            storeDialogVisibility();
            filterToggleButton.setSelected(false);
        }

        private void storeWindowPosition(ComponentEvent e) {
            var config = ApplicationConfiguration.getConfiguration();
            var component = e.getComponent();

            var dims = component.getSize();
            var loc = component.getLocation();
            try {
                config.lock(LockMode.WRITE);
                config.setProperty(ApplicationConfiguration.FilterDialog.WIDTH, dims.width);
                config.setProperty(ApplicationConfiguration.FilterDialog.HEIGHT, dims.height);
                config.setProperty(ApplicationConfiguration.FilterDialog.X, loc.x);
                config.setProperty(ApplicationConfiguration.FilterDialog.Y, loc.y);
            } finally {
                config.unlock(LockMode.WRITE);
            }
        }
    }
}

