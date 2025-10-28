package mediathek.gui.tabs;

import mediathek.config.Daten;
import mediathek.controller.history.SeenHistoryController;
import mediathek.daten.DatenFilm;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.gui.tabs.tab_film.FilmDescriptionPanel;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.MessageBus;
import org.jetbrains.annotations.NotNull;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import java.util.List;
import java.util.Optional;
import java.util.function.IntConsumer;
import java.util.function.Supplier;

public abstract class AGuiTabPanel extends JPanel {
    protected final JTabbedPane descriptionTab = new JTabbedPane();
    protected Daten daten;
    protected MediathekGui mediathekGui;
    protected FilmDescriptionPanel descriptionPanel;

    /**
     * Show description panel based on settings.
     */
    protected void makeDescriptionTabVisible(boolean visible) {
        if (visible) {
            if (descriptionTab.indexOfComponent(descriptionPanel) == -1) {
                descriptionTab.add(descriptionPanel, 0);
                descriptionTab.setTitleAt(0, "Beschreibung");
            }
        }
        else {
            if (descriptionTab.indexOfComponent(descriptionPanel) != -1) {
                descriptionTab.remove(descriptionPanel);
            }
        }
    }

    protected void setupDescriptionTab(@NotNull JTable table, @NotNull JCheckBoxMenuItem cbmi,
                                       @NotNull String configKey,
                                       @NotNull Supplier<Optional<DatenFilm>> datenFilmSupplier) {
        descriptionPanel.install(descriptionTab, table, datenFilmSupplier);
        descriptionTab.putClientProperty("JTabbedPane.tabClosable", true);
        descriptionTab.putClientProperty("JTabbedPane.tabCloseCallback",
                (IntConsumer) tabIndex -> {
                    // close description tab here
                    // must use doClick to trigger model change
                    cbmi.doClick();
                });

        setupShowFilmDescriptionMenuItem();
        initDescriptionTabVisibility(configKey);
    }

    protected void updateSelectedListItemsCount(@NotNull JTable table) {
        final int sel = table.getSelectedRowCount();
        mediathekGui.selectedListItemsProperty.setSelectedItems(sel);
    }

    protected abstract void setupShowFilmDescriptionMenuItem();

    protected void initDescriptionTabVisibility(@NotNull String configKey) {
        boolean visible = ApplicationConfiguration.getConfiguration().getBoolean(configKey, true);

        makeDescriptionTabVisible(visible);
    }

    public abstract void tabelleSpeichern();

    /**
     * Get the list of currently selected films.
     *
     * @return List of Films
     */
    protected abstract List<DatenFilm> getSelFilme();

    public abstract Optional<DatenFilm> getCurrentlySelectedFilm();

    protected void updateStartInfoProperty() {
        MessageBus.getMessageBus().publishAsync(new UpdateStatusBarLeftDisplayEvent());
    }

    public abstract void installMenuEntries(JMenu menu);

    public class MarkFilmAsSeenAction extends AbstractAction {
        public MarkFilmAsSeenAction() {
            super("Filme als gesehen markieren");
            putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_G, KeyEvent.CTRL_DOWN_MASK));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var listFilms = getSelFilme();
            try (var controller = new SeenHistoryController()) {
                controller.markSeen(listFilms);
            }
        }
    }

    public class MarkFilmAsUnseenAction extends AbstractAction {
        public MarkFilmAsUnseenAction() {
            super("Filme als ungesehen markieren");
            putValue(Action.ACCELERATOR_KEY, KeyStroke.getKeyStroke(KeyEvent.VK_N, KeyEvent.CTRL_DOWN_MASK));
        }

        @Override
        public void actionPerformed(ActionEvent e) {
            var listFilms = getSelFilme();
            try (var controller = new SeenHistoryController()) {
                controller.markUnseen(listFilms);
            }
        }
    }
}