package mediathek;

import javafx.embed.swing.SwingNode;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.Menu;
import javafx.scene.control.MenuBar;
import javafx.scene.control.MenuItem;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import mediathek.config.Daten;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.javafx.StatusBarController;

import javax.swing.*;

public class MainWindow extends Stage {
    private static MainWindow ui = null;
    private final StatusBarController statusBarController;

    public MainWindow() {
        super();
        ui = this;

        setTitle("FX Mediathekview");
        setWidth(800d);
        setHeight(600d);

        statusBarController = new StatusBarController(Daten.getInstance());

        VBox mainLayout = new VBox();
        Node contentPane = createContentPane();
        VBox.setVgrow(contentPane, Priority.ALWAYS);
        mainLayout.getChildren().addAll(createMenuBar(),
                contentPane,
                statusBarController.createStatusBar());

        Scene scene = new Scene(mainLayout);
        setScene(scene);

        setOnCloseRequest(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().beenden(false, false)));
    }

    public static MainWindow ui() {
        return ui;
    }

    public StatusBarController getStatusBarController() {
        return statusBarController;
    }

    private MenuBar createMenuBar() {
        MenuBar menuBar = new MenuBar();

        Menu menuFile = new Menu("Datei");

        Menu menuFilme = new Menu("Filme");

        Menu menuDownloads = new Menu("Downloads");

        Menu menuAbos = new Menu("Abos");

        Menu menuAnsicht = new Menu("Ansicht");

        Menu menuHelp = new Menu("Hilfe");

        MenuItem menuItem = new MenuItem("Beenden");
        menuItem.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().beenden(false, false)));
        menuFile.getItems().add(menuItem);

        menuBar.getMenus().addAll(menuFile, menuFilme, menuDownloads, menuAbos, menuAnsicht, menuHelp);
        menuBar.useSystemMenuBarProperty().set(true);

        return menuBar;
    }

    private Node createContentPane() {

        SwingNode tabNode = new SwingNode();
        SwingUtilities.invokeLater(() -> {
            JTabbedPane tabbedPane = new JTabbedPane();
            GuiDownloads tabDownloads = new GuiDownloads(Daten.getInstance(), MediathekGui.ui());
            GuiFilme tabFilme = new GuiFilme(Daten.getInstance(), MediathekGui.ui());

            tabbedPane.addTab(GuiFilme.NAME, tabFilme);
            tabbedPane.addTab(GuiDownloads.NAME, tabDownloads);
            tabbedPane.setSelectedIndex(0);

            tabNode.setContent(tabbedPane);
        });

        return tabNode;
    }
}
