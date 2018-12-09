package mediathek.javafx.downloadtab;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.beans.value.ObservableValue;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.messages.TimerEvent;
import mediathek.gui.messages.UpdateStatusBarLeftDisplayEvent;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.InfoLabel.*;
import mediathek.javafx.VerticalSeparator;
import net.engio.mbassy.listener.Handler;


public class DownloadTabInformationLabel extends HBox {
    private static final int IDX_OVERALL_LABEL = 0;
    private static final int IDX_ABO_LABEL = 2;
    private static final int IDX_NUM_DOWNLOADS_LABEL = 4;
    private static final int IDX_ACTIVE_LABEL = 6;
    private static final int IDX_WAITING_LABEL = 7;
    private static final int IDX_FINISHED_LABEL = 8;
    private static final int IDX_ERROR_LABEL = 9;
    private final Daten daten;
    private final GesamtdownloadsLabel overallDownloadLabel = new GesamtdownloadsLabel();
    private final AboLabel aboLabel = new AboLabel();
    private final NumDownloadsLabel numDownloadsLabel = new NumDownloadsLabel();
    private final ActiveDownloadsLabel activeDownloadLabel = new ActiveDownloadsLabel();
    private final WaitingLabel waitingLabel = new WaitingLabel();
    private final FinishedLabel finishedLabel = new FinishedLabel();
    private final ErrorLabel errorLabel = new ErrorLabel();
    private final HBox finishedBox = new HBox();
    private final HBox waitingBox = new HBox();
    private final HBox activeBox = new HBox();
    public DownloadTabInformationLabel(Daten daten) {
        super();
        this.daten = daten;

        setupListeners();
        initLayout();
    }

    private void setupListeners() {
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

    private void initLayout() {
        finishedBox.getChildren().addAll(new CenteredBorderPane(finishedLabel), new VerticalSeparator());
        waitingBox.getChildren().addAll(new CenteredBorderPane(waitingLabel), new VerticalSeparator());
        activeBox.getChildren().addAll(new CenteredBorderPane(activeDownloadLabel), new VerticalSeparator());

        final var children = getChildren();
        children.addAll(new VerticalSeparator(),
                new VerticalSeparator(),
                new VerticalSeparator());
        children.add(IDX_OVERALL_LABEL, new CenteredBorderPane(overallDownloadLabel));
        children.add(IDX_ABO_LABEL, new CenteredBorderPane(aboLabel));
        children.add(IDX_NUM_DOWNLOADS_LABEL, new CenteredBorderPane(numDownloadsLabel));
        children.add(IDX_ACTIVE_LABEL, activeBox);
        children.add(IDX_WAITING_LABEL, waitingBox);
        children.add(IDX_FINISHED_LABEL, finishedBox);
        children.add(IDX_ERROR_LABEL, new CenteredBorderPane(errorLabel));
    }

    @Handler
    private void handleLeftDisplayUpdate(UpdateStatusBarLeftDisplayEvent e) {
        Platform.runLater(this::getInfoTextDownloads);
    }

    @Handler
    private void handleTimerEvent(TimerEvent e) {
        Platform.runLater(this::getInfoTextDownloads);
    }

    private void getInfoTextDownloads() {
        final var listeDownloads = daten.getListeDownloads();
        final var info = listeDownloads.getStarts();
        final var children = getChildren();

        overallDownloadLabel.updateLabel(listeDownloads, info);
        aboLabel.updateLabel(info);
        numDownloadsLabel.updateLabel(info);

        if (info.running > 0) {
            if (!children.contains(activeBox))
                children.add(IDX_ACTIVE_LABEL, activeBox);
            activeDownloadLabel.updateLabel(daten, info);
        } else
            children.remove(activeBox);

        if (info.initialized > 0) {
            if (!children.contains(waitingBox))
                children.add(IDX_WAITING_LABEL, waitingBox);
            waitingLabel.updateLabel(info);
        } else
            children.remove(waitingBox);

        if (info.finished > 0) {
            if (!children.contains(finishedBox))
                children.add(IDX_FINISHED_LABEL, finishedBox);
            finishedLabel.updateLabel(info);
        } else
            children.remove(finishedBox);

        if (info.error > 0) {
            if (!children.contains(errorLabel))
                children.add(IDX_ERROR_LABEL, errorLabel);
            errorLabel.updateLabel(info);
        } else
            children.remove(errorLabel);
    }
}
