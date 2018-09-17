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

        Button btnDelete = new Button();
        btnDelete.setTooltip(new Tooltip("Abos löschen"));
        view = new ImageView(new Image("/mediathek/res/programm/toolbar-abo-del.png", 0, 16, true, true));
        btnDelete.setGraphic(view);
        btnDelete.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::loeschen));

        Button btnEdit = new Button();
        btnEdit.setTooltip(new Tooltip("Abo ändern"));
        view = new ImageView(new Image("/mediathek/res/programm/toolbar-abo-config.png", 0, 16, true, true));
        btnEdit.setGraphic(view);
        btnEdit.setOnAction(e -> SwingUtilities.invokeLater(tabAbo::aendern));

        Button btnShowFilter = new Button();
        btnShowFilter.setTooltip(new Tooltip("Filter anzeigen/ausblenden"));
        view = new ImageView(new Image("/mediathek/res/programm/button-filter-anzeigen.png", 0, 16, true, true));
        btnShowFilter.setGraphic(view);
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
