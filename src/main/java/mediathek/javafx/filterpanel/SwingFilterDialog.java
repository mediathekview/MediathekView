package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.layout.VBox;
import mediathek.config.Daten;
import mediathek.filmeSuchen.ListenerFilmeLaden;
import mediathek.filmeSuchen.ListenerFilmeLadenEvent;
import mediathek.tool.ApplicationConfiguration;
import org.apache.commons.configuration2.sync.LockMode;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ComponentAdapter;
import java.awt.event.ComponentEvent;
import java.util.NoSuchElementException;

public class SwingFilterDialog extends JDialog {
    private final JFXPanel fxPanel = new JFXPanel();

    public SwingFilterDialog(Frame owner, VBox content) {
        super(owner);
        setDefaultCloseOperation(HIDE_ON_CLOSE);
        setTitle("Filter");
        setType(Type.UTILITY);
        setContentPane(fxPanel);
        Platform.runLater(() -> {
            fxPanel.setScene(new Scene(content));
            SwingUtilities.invokeLater(() -> {
                pack();
                restoreWindowSizeFromConfig();
                registerWindowSizeListener();
            });
        });

        Daten.getInstance().getFilmeLaden().addAdListener(new ListenerFilmeLaden() {
            @Override
            public void start(ListenerFilmeLadenEvent event) {
                setEnabled(false);
            }

            @Override
            public void fertig(ListenerFilmeLadenEvent event) {
                setEnabled(true);
            }
        });
    }

    private void restoreWindowSizeFromConfig() {
        var config = ApplicationConfiguration.getConfiguration();
        try {
            config.lock(LockMode.READ);
            final int width = config.getInt(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_WIDTH);
            final int height = config.getInt(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_HEIGHT);
            final int x = config.getInt(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_LOCATION_X);
            final int y = config.getInt(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_LOCATION_Y);

            setBounds(x, y, width, height);
        } catch (NoSuchElementException ignored) {
            //do not restore anything
        } finally {
            config.unlock(LockMode.READ);
        }

    }

    private void registerWindowSizeListener() {
        addComponentListener(new ComponentAdapter() {
            @Override
            public void componentResized(ComponentEvent e) {
                storeWindowPosition(e);
            }

            @Override
            public void componentMoved(ComponentEvent e) {
                storeWindowPosition(e);
            }

            @Override
            public void componentHidden(ComponentEvent e) {
                storeWindowPosition(e);
            }

            private void storeWindowPosition(ComponentEvent e) {
                var config = ApplicationConfiguration.getConfiguration();
                var component = e.getComponent();

                var dims = component.getSize();
                var loc = component.getLocation();
                try {
                    config.lock(LockMode.WRITE);
                    config.setProperty(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_WIDTH, dims.width);
                    config.setProperty(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_HEIGHT, dims.height);
                    config.setProperty(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_LOCATION_X, loc.x);
                    config.setProperty(ApplicationConfiguration.APPLICATION_UI_FILTER_DIALOG_LOCATION_Y, loc.y);
                }
                finally {
                    config.unlock(LockMode.WRITE);
                }
            }
        });

    }
}
