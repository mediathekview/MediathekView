package mediathek.gui.abo;

import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.fxml.Initializable;
import javafx.scene.control.*;
import javafx.scene.layout.HBox;
import mediathek.config.Daten;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;
import mediathek.tool.SenderList;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;
import java.net.URL;
import java.util.ResourceBundle;


public class FXAboToolBar extends ToolBar implements Initializable {

    public ComboBox<String> cbSender;
    public Button btnOn;
    public Button btnOff;
    public Button btnDelete;
    public Button btnEdit;
    public Button btnNewAbo;

    public FXAboToolBar(ManageAboPanel tabAbo) {
        super();

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        btnOn = new Button("", fontAwesome.create(FontAwesome.Glyph.CHECK).size(16d));
        btnOn.setTooltip(new Tooltip("Abos einschalten"));
        btnOn.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(true)));

        btnOff = new Button("", fontAwesome.create(FontAwesome.Glyph.REMOVE).size(16d));
        btnOff.setTooltip(new Tooltip("Abos ausschalten"));
        btnOff.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(false)));

        btnDelete = new Button("", fontAwesome.create(FontAwesome.Glyph.MINUS).size(16d));
        btnDelete.setTooltip(new Tooltip("Abos löschen"));
        btnDelete.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::loeschen));

        btnEdit = new Button("", fontAwesome.create(FontAwesome.Glyph.EDIT).size(16d));
        btnEdit.setTooltip(new Tooltip("Abo ändern"));
        btnEdit.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::editAbo));

        btnNewAbo = new Button("", fontAwesome.create(FontAwesome.Glyph.PLUS).size(16d));
        btnNewAbo.setTooltip(new Tooltip("Abo anlegen"));
        CreateNewAboAction newAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
        btnNewAbo.setOnAction(e -> SwingUtilities.invokeLater(() -> newAboAction.actionPerformed(null)));

        var senderList = new SenderList(Daten.getInstance().getListeFilme().getBaseSenderList());
        cbSender = new ComboBox<>(new EventObservableList<>(senderList));
        cbSender.getSelectionModel().select(0);

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

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {

    }
}
