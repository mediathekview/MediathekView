package mediathek.javafx.filterpanel;

import javafx.application.Platform;
import javafx.beans.value.ChangeListener;
import javafx.collections.ObservableList;
import javafx.event.ActionEvent;
import javafx.event.EventHandler;
import javafx.geometry.Insets;
import javafx.geometry.Orientation;
import javafx.scene.control.*;
import javafx.scene.layout.*;
import javafx.util.StringConverter;
import mediathek.gui.messages.TableModelChangeEvent;
import mediathek.tool.FilterDTO;
import mediathek.tool.MessageBus;
import net.engio.mbassy.listener.Handler;
import org.apache.commons.lang3.SystemUtils;
import org.controlsfx.glyphfont.Glyph;

public class CommonViewSettingsPane extends VBox {
    public final ThemaComboBox themaComboBox = new ThemaComboBox();
    public final FilmLenghtSliderNode filmLengthSliderNode = new FilmLenghtSliderNode();
    public final ZeitraumSpinner zeitraumSpinner = new ZeitraumSpinner();
    public final SenderBoxNode senderCheckList = new SenderBoxNode();
    public final CheckBox cbDontShowAudioVersions = new CheckBox("Hörfassungen ausblenden");
    public final CheckBox cbDontShowGebaerdensprache = new CheckBox("Gebärdensprache nicht anzeigen");
    public final CheckBox cbDontShowDuplicates = new CheckBox("Duplikate nicht anzeigen");
    public final CheckBox cbDontShowTrailers = new CheckBox("Trailer/Teaser/Vorschau nicht anzeigen");
    public final CheckBox cbShowUnseenOnly = new CheckBox("Gesehene Filme nicht anzeigen");
    public final CheckBox cbDontShowAbos = new CheckBox("Abos nicht anzeigen");
    public final CheckBox cbShowOnlyHd = new CheckBox("Nur High Quality(HQ) Filme anzeigen");
    public final CheckBox cbShowSubtitlesOnly = new CheckBox("Nur Filme mit Untertitel anzeigen");
    public final CheckBox cbShowNewOnly = new CheckBox("Nur neue Filme anzeigen");
    public final CheckBox cbShowBookMarkedOnly = new CheckBox("Nur gemerkte Filme anzeigen");
    public final CheckBox cbShowOnlyLivestreams = new CheckBox("Nur Livestream anzeigen");
    public final Button btnDeleteFilterSettings = new Button();
    public final Button btnDeleteCurrentFilter = new Button();
    public final Button btnRenameFilter = new Button();
    private final ComboBox<FilterDTO> filterSelect = new ComboBox<>();
    private final Button btnAddNewFilter = new Button();
    private boolean deleteCurrentFilterButtonDisabled;

    static class FontAwesomeGlyph extends Glyph {
        public FontAwesomeGlyph(String icon) {
            setFontFamily("FontAwesome");
            setFontSize(16d);
            setIcon(icon);
        }
    }

    private HBox createTopButtonRow() {
        HBox btnBox = new HBox();
        btnBox.setSpacing(4d);
        btnBox.getChildren().addAll(filterSelect, btnRenameFilter, btnAddNewFilter, btnDeleteCurrentFilter,
                new Separator(Orientation.VERTICAL), btnDeleteFilterSettings);
        return btnBox;
    }

    private Pane createSenderList() {
        senderCheckList.setPrefHeight(150d);
        senderCheckList.setMinHeight(100d);
        VBox.setVgrow(senderCheckList, Priority.ALWAYS);
        var titledPane = new VBox();
        titledPane.getChildren().addAll(new Label("Sender:"),
                senderCheckList);
        VBox.setVgrow(titledPane, Priority.ALWAYS);

        return titledPane;
    }

    private Pane createThemaBox() {
        var hbox = new HBox();
        hbox.setSpacing(4d);
        HBox.setHgrow(themaComboBox, Priority.ALWAYS);
        var borderPane = new BorderPane();
        var themaLabel = new Label("Thema:");
        themaLabel.setMinWidth(USE_PREF_SIZE);
        // font size is greater on tested ubuntu linux :(
        if (SystemUtils.IS_OS_LINUX)
            themaLabel.setPrefWidth(50d);
        else
            themaLabel.setPrefWidth(45d);
        borderPane.setCenter(themaLabel);
        hbox.getChildren().addAll(borderPane, themaComboBox);

        return hbox;
    }

    private void setupButtons() {
        btnRenameFilter.setTooltip(new Tooltip("Filter umbenennen"));
        btnRenameFilter.setGraphic(new FontAwesomeGlyph("EDIT"));

        btnAddNewFilter.setTooltip(new Tooltip("Neuen Filter anlegen"));
        btnAddNewFilter.setGraphic(new FontAwesomeGlyph("PLUS"));

        btnDeleteCurrentFilter.setTooltip(new Tooltip("Aktuellen Filter löschen"));
        btnDeleteCurrentFilter.setGraphic(new FontAwesomeGlyph("TRASH_ALT"));

        btnDeleteFilterSettings.setTooltip(new Tooltip("Aktuellen Filter zurücksetzen"));
        btnDeleteFilterSettings.setGraphic(new FontAwesomeGlyph("RECYCLE"));
    }

    public CommonViewSettingsPane() {
        setPadding(new Insets(5, 5, 5, 5));
        setSpacing(4d);

        filterSelect.setPromptText("Filter Auswahl");

        setupButtons();

        getChildren().addAll(createTopButtonRow(),
                new Separator(),
                cbShowNewOnly,
                cbShowBookMarkedOnly,
                cbShowOnlyHd,
                cbShowSubtitlesOnly,
                cbShowOnlyLivestreams,
                new Separator(),
                cbShowUnseenOnly,
                cbDontShowAbos,
                cbDontShowGebaerdensprache,
                cbDontShowTrailers,
                cbDontShowAudioVersions,
                cbDontShowDuplicates,
                new Separator(),
                createSenderList(),
                new Separator(),
                createThemaBox(),
                new Separator(),
                filmLengthSliderNode,
                new Separator(),
                createZeitraumSpinner());


        MessageBus.getMessageBus().subscribe(this);
    }

    private Pane createZeitraumSpinner() {
        var flowPane = new FlowPane(4d,0d);
        flowPane.getChildren().add(new Label("Zeitraum:"));
        flowPane.getChildren().add(zeitraumSpinner);
        flowPane.getChildren().add(new Label("Tage"));

        return flowPane;
    }

    /**
     * Prevent user from changing filter settings while the swing table model gets updated.
     *
     * @param evt the model event
     */
    @Handler
    private void handleTableModelChangeEvent(TableModelChangeEvent evt) {
        Platform.runLater(
                () -> {
                    final boolean disable = evt.active;
                    btnDeleteFilterSettings.setDisable(disable);
                    cbShowOnlyHd.setDisable(disable);
                    cbShowSubtitlesOnly.setDisable(disable);
                    cbShowNewOnly.setDisable(disable);
                    cbShowBookMarkedOnly.setDisable(disable);
                    cbShowOnlyLivestreams.setDisable(disable);
                    cbShowUnseenOnly.setDisable(disable);
                    cbDontShowAbos.setDisable(disable);
                    cbDontShowGebaerdensprache.setDisable(disable);
                    cbDontShowTrailers.setDisable(disable);
                    cbDontShowAudioVersions.setDisable(disable);
                    cbDontShowDuplicates.setDisable(disable);
                    senderCheckList.setDisable(disable);
                    themaComboBox.setDisable(disable);
                    filmLengthSliderNode.setDisable(disable);
                    zeitraumSpinner.setDisable(disable);
                    filterSelect.setDisable(disable);
                    btnDeleteCurrentFilter.setDisable(disable || deleteCurrentFilterButtonDisabled);
                    btnAddNewFilter.setDisable(disable);
                    btnRenameFilter.setDisable(disable);
                });
    }

    public void disableDeleteCurrentFilterButton(boolean disable) {
        deleteCurrentFilterButtonDisabled = disable;
        btnDeleteCurrentFilter.setDisable(disable);
    }

    public void setFilterSelectionChangeListener(ChangeListener<FilterDTO> changeListener) {
        filterSelect.getSelectionModel().selectedItemProperty().addListener(changeListener);
    }

    public void setAvailableFilters(ObservableList<FilterDTO> filters) {
        filterSelect.setItems(filters);
    }

    public void selectFilter(FilterDTO filter) {
        SingleSelectionModel<FilterDTO> selectionModel = filterSelect.getSelectionModel();
        if (!filter.equals(selectionModel.getSelectedItem())) {
            Platform.runLater(() -> selectionModel.select(filter));
        }
    }

    public void setAddNewFilterButtonEventHandler(EventHandler<ActionEvent> eventHandler) {
        btnAddNewFilter.setOnAction(eventHandler);
    }

    public void setFilterSelectionStringConverter(StringConverter<FilterDTO> filterStringConverter) {
        filterSelect.setConverter(filterStringConverter);
    }
}
