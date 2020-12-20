package mediathek.gui.actions;

import javafx.application.Platform;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.*;
import javafx.scene.layout.FlowPane;
import javafx.stage.Stage;
import mediathek.mainwindow.MediathekGui;
import mediathek.tool.ApplicationConfiguration;
import mediathek.tool.swing.LookAndFeelType;
import mediathek.tool.swing.SwingUIFontChanger;
import org.apache.logging.log4j.LogManager;
import org.apache.logging.log4j.Logger;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;
import java.awt.*;
import java.awt.event.ActionEvent;

public class ChangeGlobalFontSetting extends AbstractAction {
    private static final Logger logger = LogManager.getLogger();
    private JMenuItem menuItem;
    private boolean sizeChanged;
    private Stage window;

    public ChangeGlobalFontSetting() {
        putValue(Action.NAME, "Globale Schriftgröße ändern...");
    }

    public void setMenuItem(JMenuItem menuItem) {
        this.menuItem = menuItem;
    }

    private int getCurrentSize() {
        int result = -1;
        Font font;

        switch (LookAndFeelType.get(UIManager.getLookAndFeel().getClass().getName())) {
            case Windows -> {
                font = (Font) UIManager.getDefaults().get(SwingUIFontChanger.WINDOWS_DEFAULT_FONT);
                result = font.getSize();
            }
            case Nimbus -> {
                font = (Font) UIManager.getDefaults().get(SwingUIFontChanger.NIMBUS_DEFAULT_FONT);
                result = font.getSize();
            }
        }
        return result;
    }

    @Override
    public void actionPerformed(ActionEvent e) {
        var initialSize = getCurrentSize();
        sizeChanged = false;

        Platform.runLater(() -> {
            createWindow();
            window.setScene(new Scene(createLayout(initialSize)));
            window.show();
        });
    }

    private FlowPane createLayout(int initialSize) {
        Label label = new Label("Schriftgröße:");
        final Spinner<Integer> spinner = new Spinner<>();
        spinner.setEditable(true);

        var valueFactory = new SpinnerValueFactory.IntegerSpinnerValueFactory(12, 48, initialSize);
        spinner.setValueFactory(valueFactory);
        spinner.valueProperty().addListener((observableValue, oldValue, newValue) -> SwingUtilities.invokeLater(() -> spinnerUpdate(newValue)));

        FlowPane root = new FlowPane();
        root.setHgap(10);
        root.setVgap(10);
        root.setPadding(new Insets(10));

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");
        Button btnReset = new Button("", fontAwesome.create(FontAwesome.Glyph.TRASH_ALT));
        btnReset.setTooltip(new Tooltip("Schriftgröße zurücksetzen"));

        btnReset.setOnAction(evt -> resetAction());
        root.getChildren().addAll(label, spinner, btnReset);

        return root;
    }

    private void spinnerUpdate(int newValue) {
        logger.info("Updating Swing UI font size to {}", newValue);
        SwingUIFontChanger fc = new SwingUIFontChanger();
        fc.changeFontSize(newValue);
        ApplicationConfiguration.getConfiguration().setProperty(ApplicationConfiguration.APPLICATION_UI_FONT_SIZE, (float) newValue);

        SwingUtilities.updateComponentTreeUI(MediathekGui.ui());
        sizeChanged = true;
    }

    private void resetAction() {
        logger.info("Resetting Swing UI to default font size");
        ApplicationConfiguration.getConfiguration().clearProperty(ApplicationConfiguration.APPLICATION_UI_FONT_SIZE);
        sizeChanged = true;
        window.close();
    }

    private void showAppTerminationAlert() {
        Alert alert = new Alert(Alert.AlertType.CONFIRMATION, """
        Die Änderung der Schriftgröße erfordert einen Neustart.
        Möchten Sie MediathekView nun beenden?
        """, ButtonType.YES, ButtonType.NO);
        alert.setHeaderText("Schriftgröße wurde geändert");
        alert.showAndWait();
        if (alert.getResult() == ButtonType.YES) {
            SwingUtilities.invokeLater(() -> MediathekGui.ui().beenden(false, false));
        }
    }

    private void createWindow() {
        window = new Stage();
        window.setTitle("Globale Schriftgröße ändern");
        window.setResizable(false);
        window.setOnShowing(evt -> SwingUtilities.invokeLater(() -> menuItem.setEnabled(false)));
        window.setOnHidden(evt -> SwingUtilities.invokeLater(() -> {
            menuItem.setEnabled(true);
            if (sizeChanged) {
                Platform.runLater(this::showAppTerminationAlert);
            }
        }));
    }
}
