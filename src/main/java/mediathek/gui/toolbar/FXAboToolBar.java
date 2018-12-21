package mediathek.gui.toolbar;

import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import mediathek.config.Daten;
import mediathek.gui.GuiAbo;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;


public class FXAboToolBar extends ToolBar {

    private final ComboBox<String> cbSender = new ComboBox<>();

    public FXAboToolBar(GuiAbo tabAbo) {
        super();

        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        Button btnOn = new Button("", fontAwesome.create(FontAwesome.Glyph.CHECK).size(16d));
        btnOn.setTooltip(new Tooltip("Abos einschalten"));
        btnOn.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(true)));

        Button btnOff = new Button("", fontAwesome.create(FontAwesome.Glyph.REMOVE).size(16d));
        btnOff.setTooltip(new Tooltip("Abos ausschalten"));
        btnOff.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(false)));

        Button btnDelete = new Button("", fontAwesome.create(FontAwesome.Glyph.MINUS).size(16d));
        btnDelete.setTooltip(new Tooltip("Abos löschen"));
        btnDelete.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::loeschen));

        Button btnEdit = new Button("", fontAwesome.create(FontAwesome.Glyph.EDIT).size(16d));
        btnEdit.setTooltip(new Tooltip("Abo ändern"));
        btnEdit.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::editAbo));

        Button btnNewAbo = new Button("", fontAwesome.create(FontAwesome.Glyph.PLUS).size(16d));
        btnNewAbo.setTooltip(new Tooltip("Abo anlegen"));
        CreateNewAboAction newAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
        btnNewAbo.setOnAction(e -> SwingUtilities.invokeLater(() -> newAboAction.actionPerformed(null)));

        HBox hBox = new HBox();
        hBox.getChildren().addAll(new CenteredBorderPane(new Label("Abos für Sender:")), cbSender);

        getItems().addAll(btnOn,
                btnOff,
                new VerticalSeparator(),
                btnNewAbo,
                btnDelete,
                btnEdit,
                new VerticalSeparator(),
                hBox);
    }

    public ComboBox<String> getSenderComboBox() {
        return cbSender;
    }
}
