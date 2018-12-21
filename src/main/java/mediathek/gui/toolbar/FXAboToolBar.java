package mediathek.gui.toolbar;

import javafx.scene.control.Button;
import javafx.scene.control.ToolBar;
import javafx.scene.control.Tooltip;
import javafx.scene.layout.HBox;
import javafx.scene.layout.Priority;
import javafx.scene.layout.Region;
import mSearch.tool.Listener;
import mediathek.config.Daten;
import mediathek.config.MVConfig;
import mediathek.gui.GuiAbo;
import mediathek.gui.actions.CreateNewAboAction;
import mediathek.javafx.VerticalSeparator;
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

        GlyphFont fontAwesome = GlyphFontRegistry.font("FontAwesome");

        Button btnOn = new Button("", fontAwesome.create(FontAwesome.Glyph.CHECK).size(16d));
        btnOn.setTooltip(new Tooltip("Abos einschalten"));
        btnOn.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(true)));

        Button btnOff = new Button("", fontAwesome.create(FontAwesome.Glyph.REMOVE).size(16d));
        btnOff.setTooltip(new Tooltip("Abos ausschalten"));
        btnOff.setOnAction(e -> SwingUtilities.invokeLater(() -> tabAbo.einAus(false)));

        Button btnDelete = new Button("",fontAwesome.create(FontAwesome.Glyph.MINUS).size(16d));
        btnDelete.setTooltip(new Tooltip("Abos löschen"));
        btnDelete.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::loeschen));

        Button btnEdit = new Button("",fontAwesome.create(FontAwesome.Glyph.EDIT).size(16d));
        btnEdit.setTooltip(new Tooltip("Abo ändern"));
        btnEdit.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::editAbo));

        Button btnShowFilter = new FilterButton();
        btnShowFilter.setOnAction(e -> SwingUtilities.invokeLater(() -> {
            boolean b = !Boolean.parseBoolean(MVConfig.get(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS));
            MVConfig.add(MVConfig.Configs.SYSTEM_TAB_ABO_FILTER_VIS, Boolean.toString(b));
            Listener.notify(Listener.EREIGNIS_PANEL_ABO_FILTER_ANZEIGEN, FXAboToolBar.class.getName());
        }));

        Button btnNewAbo = new Button("",fontAwesome.create(FontAwesome.Glyph.PLUS).size(16d));
        btnNewAbo.setTooltip(new Tooltip("Abo anlegen"));
        CreateNewAboAction newAboAction = new CreateNewAboAction(Daten.getInstance().getListeAbo());
        btnNewAbo.setOnAction(e -> SwingUtilities.invokeLater(() -> newAboAction.actionPerformed(null)));

        getItems().addAll(btnOn,
                btnOff,
                new VerticalSeparator(),
                btnNewAbo,
                btnDelete,
                btnEdit,
                spacer,
                btnShowFilter);
    }

}
