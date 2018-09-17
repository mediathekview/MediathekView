package mediathek;

import javafx.embed.swing.SwingNode;
import javafx.scene.Scene;
import javafx.scene.control.*;
import javafx.scene.layout.BorderPane;
import javafx.stage.Stage;
import mediathek.config.Daten;
import mediathek.gui.GuiAbo;
import mediathek.gui.GuiDownloads;
import mediathek.gui.GuiFilme;
import mediathek.javafx.StatusBarController;

import javax.swing.*;

public class MainWindow extends Stage {
    public MainWindow() {
        super();
        setTitle("Dummy JavaFX MainWindow");
        setWidth(800d);
        setHeight(600d);

        BorderPane bp = new BorderPane();
        StatusBarController cont = new StatusBarController(Daten.getInstance());
        bp.setBottom(cont.createStatusBar());

        TabPane tabPane = new TabPane();
        Tab filmeTab = new Tab("Filme");
        filmeTab.setClosable(false);
        final SwingNode filmeSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> filmeSwingNode.setContent(new GuiFilme(Daten.getInstance(),MediathekGui.ui())));
        filmeTab.setContent(filmeSwingNode);

        Tab downloadsTab = new Tab("Downloads");
        final SwingNode downloadsSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> downloadsSwingNode.setContent(new GuiDownloads(Daten.getInstance(),MediathekGui.ui())));
        downloadsTab.setContent(downloadsSwingNode);
        downloadsTab.setClosable(false);

        Tab aboTab = new Tab("Abos");
        final SwingNode aboSwingNode = new SwingNode();
        SwingUtilities.invokeLater(() -> aboSwingNode.setContent(new GuiAbo(Daten.getInstance(),MediathekGui.ui())));
        aboTab.setClosable(false);
        aboTab.setContent(aboSwingNode);

        tabPane.getTabs().addAll(filmeTab, downloadsTab,aboTab);
        bp.setCenter(tabPane);

        Scene scene = new Scene(bp);
        setScene(scene);

        MenuBar menuBar = new MenuBar();

        Menu menuFile = new Menu("Datei");

        Menu menuFilme = new Menu("Filme");

        Menu menuDownloads = new Menu("Downloads");

        Menu menuAbos = new Menu("Abos");

        Menu menuAnsicht = new Menu("Ansicht");

        Menu menuHelp = new Menu("Hilfe");

        MenuItem menuItem = new MenuItem("Beenden");
        menuItem.setOnAction(e -> SwingUtilities.invokeLater(() -> MediathekGui.ui().beenden(false,false)));
        menuFile.getItems().add(menuItem);

        menuBar.getMenus().addAll(menuFile, menuFilme, menuDownloads, menuAbos,menuAnsicht, menuHelp);
        menuBar.useSystemMenuBarProperty().set(true);
        bp.setTop(menuBar);
    }
}
