package mediathek.javafx.filmtab;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.messages.DownloadInfoUpdateAvailableEvent;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.gui.tabs.tab_film.GuiFilme;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;
import net.engio.mbassy.listener.Handler;

public class FilmTabInfoPane extends HBox {
    private final FilmTabDownloadInformationLabel downloadInformationLabel;
    private final FilmInfoLabel filmInfoLabel;

    public FilmTabInfoPane(Daten daten, GuiFilme tabFilme) {
        super();
        downloadInformationLabel = new FilmTabDownloadInformationLabel(daten);
        filmInfoLabel = new FilmInfoLabel(daten,tabFilme);

        getChildren().addAll(new CenteredBorderPane(filmInfoLabel),
                new VerticalSeparator(),
                new CenteredBorderPane(downloadInformationLabel),
                new VerticalSeparator());

        if (isVisible())
            daten.getMessageBus().subscribe(this);

        visibleProperty().addListener(new ChangeListener<>() {
            @Override
            public void changed(ObservableValue<? extends Boolean> observable, Boolean oldValue, Boolean newValue) {
                if (newValue) {
                    daten.getMessageBus().subscribe(this);
                } else {
                    daten.getMessageBus().unsubscribe(this);
                }
            }
        });
    }

    private void updateLayout() {
        filmInfoLabel.updateValues();
    }

    @Handler
    private void handleDownloadInfoUpdate(DownloadInfoUpdateAvailableEvent e) {
        Platform.runLater(downloadInformationLabel::setInfoFilme);
    }
    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        Platform.runLater(this::updateLayout);
    }


    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::updateLayout);
    }
}
