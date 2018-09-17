package mediathek.gui.toolbar;

import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.image.Image;
import javafx.scene.image.ImageView;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import mSearch.tool.Listener;
import mediathek.config.MVConfig;
import mediathek.gui.GuiAbo;
import mediathek.javafx.tool.FilterButton;
import org.controlsfx.glyphfont.FontAwesome;
import org.controlsfx.glyphfont.GlyphFont;
import org.controlsfx.glyphfont.GlyphFontRegistry;

import javax.swing.*;


public class FXAboToolBar extends ToolBar {

    public FXAboToolBar(GuiAbo tabAbo) {
        super();

        Region spacer = new Region();
        HBox.setHgrow(spacer, Priority.ALWAYS);

        Button btnOn = new Button();
        btnOn.setTooltip(new Tooltip("Abos einschalten"));
        ImageView view = new ImageView(new Image("/mediathek/res/programm/toolbar-abo-ein.png", 0, 16, true, true));
        btnOn.setGraphic(view);
        btnOn.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(true)));

        Button btnOff = new Button();
        btnOff.setTooltip(new Tooltip("Abos ausschalten"));
        view = new ImageView(new Image("/mediathek/res/programm/toolbar-abo-aus.png", 0, 16, true, true));
        btnOff.setGraphic(view);
        btnOff.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(false)));

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        Button btnDelete = new Button("",fontAwesome.create(FontAwesome.Glyph.TRASH_ALT).size(16d));
        btnDelete.setTooltip(new Tooltip("Abos löschen"));
        btnDelete.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::loeschen));

        Button btnEdit = new Button("",fontAwesome.create(FontAwesome.Glyph.EDIT).size(16d));
        btnEdit.setTooltip(new Tooltip("Abo ändern"));
        btnEdit.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::aendern));

        Button btnShowFilter = new FilterButton();
        btnShowFilter.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, FXAboToolBar.class.getName());
        }));

        getItems().addAll(btnOn,
                btnOff,
                btnDelete,
                btnEdit,
                spacer,
                btnShowFilter);
    }

}
