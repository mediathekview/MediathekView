package mediathek;

import javafx.embed.swing.SwingNode;
import javafx.scene.Node;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.stage.Stage;
import mediathek.config.Daten;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.javafx.StatusBarController;

import javax.swing.*;

public class MainWindow extends Stage {
    private final StatusBarController statusBarController;
    private static MainWindow ui = null;

    public StatusBarController getStatusBarController() {
        return statusBarController;
    }

    public static MainWindow ui() { return ui;}
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
        TabPane tabPane = new TabPane();
        Tab filmeTab = new Tab("Filme");
        filmeTab.setClosable(false);
        final SwingNode filmeSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> filmeSwingNode.setContent(new GuiFilme(Daten.getInstance(), MediathekGui.ui())));
        filmeTab.setContent(filmeSwingNode);

        Tab downloadsTab = new Tab("Downloads");
        final SwingNode downloadsSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> downloadsSwingNode.setContent(new GuiDownloads(Daten.getInstance(), MediathekGui.ui())));
        downloadsTab.setContent(downloadsSwingNode);
        downloadsTab.setClosable(false);

        Tab aboTab = new Tab("Abos");
        final SwingNode aboSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> aboSwingNode.setContent(new GuiAbo(Daten.getInstance(), MediathekGui.ui())));
        aboTab.setClosable(false);
        aboTab.setContent(aboSwingNode);

        tabPane.getTabs().addAll(filmeTab, downloadsTab, aboTab);

        return tabPane;
    }
}
