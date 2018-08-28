package mediathek.javafx.descriptionPanel;

import javafx.application.Platform;
import javafx.embed.swing.JFXPanel;
import javafx.scene.Scene;
import javafx.scene.control.Tab;
import javafx.scene.control.TabPane;
import javafx.scene.web.WebEngine;
import javafx.scene.web.WebView;
import mSearch.daten.DatenFilm;
import mediathek.daten.DatenDownload;
import mediathek.tool.table.MVDownloadsTable;
import mediathek.tool.table.MVFilmTable;

import javax.swing.*;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableModel;

public class FXDescriptionPanel extends JFXPanel implements ListSelectionListener {
    private WebEngine webEngine;
    private final JTable table;
    private DatenFilm currentFilm;

    public FXDescriptionPanel(JTable table) {
        super();
        this.table = table;

        table.getSelectionModel().addListSelectionListener(this);

        Platform.runLater(this::initFX);
    }

    private void initFX() {
        setScene(createScene());
        updateFilmData();
    }

    private Scene createScene() {
        WebView webView = new WebView();
        webView.setMaxHeight(100d);
        webView.setContextMenuEnabled(false);

        webEngine = webView.getEngine();
        webEngine.loadContent("<html></html>");

        TabPane tabPane = new TabPane();
        Tab descTab = new Tab("Beschreibung");
        tabPane.getTabs().add(descTab);

        descTab.setContent(webView);
        descTab.setOnCloseRequest(e -> {
            SwingUtilities.invokeLater(() -> getParent().setVisible(false));
            e.consume();
        });

        return new Scene(tabPane);
    }

    private void updateFilmData() {
        final int selectedTableRow = table.getSelectedRow();
        if (selectedTableRow >= 0) {
            DatenFilm film;
            final TableModel model = table.getModel();
            final int modelIndex = table.convertRowIndexToModel(selectedTableRow);

            if (table instanceof MVFilmTable) {
                film = (DatenFilm) model.getValueAt(modelIndex, DatenFilm.FILM_REF);
            } else if (table instanceof MVDownloadsTable) {
                film = ((DatenDownload) model.getValueAt(modelIndex, DatenDownload.DOWNLOAD_REF)).film;
            } else {
                System.out.println("UNHANDLED TABLE TYPE!!!");
                film = null;
            }

            displayFilmData(film);
        } else {
            displayFilmData(null);
        }
    }

    private void displayFilmData(DatenFilm aaktFilm) {
        currentFilm = aaktFilm;
        setText();
    }

    private void setText() {
        Platform.runLater(() -> {
            if (currentFilm == null) {
                webEngine.loadContent("");
            } else {
                // Beschreibung setzen
                webEngine.loadContent(
                        "<html>"
                                + "<span style=\'font-family: Helvetica; font-weight: bold;font-size: 14\'>" + (currentFilm.getSender().isEmpty() ? "" : currentFilm.getSender() + "  -  ")
                                + currentFilm.getTitle() + "</span><br/><br/>"
                                + "<span style=\'font-family: Helvetica; font-size: 14'>" + currentFilm.getDescription() + "</span>"
                                + "</html>");
            }
        });
    }

    @Override
    public void valueChanged(ListSelectionEvent e) {
        if (!e.getValueIsAdjusting()) {
            updateFilmData();
        }
    }
}
