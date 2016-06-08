/*
 * MediathekView
 * Copyright (C) 2014 W. Xaver
 * W.Xaver[at]googlemail.com
 * http://zdfmediathk.sourceforge.net/
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package mediathek.fx;

import java.net.URL;
import java.util.LinkedList;
import java.util.Optional;
import java.util.ResourceBundle;
import javafx.application.Platform;
import javafx.beans.property.ObjectProperty;
import javafx.beans.property.SimpleObjectProperty;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import javafx.embed.swing.JFXPanel;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.geometry.Insets;
import javafx.geometry.Point2D;
import javafx.geometry.Pos;
import javafx.scene.Node;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.chart.LineChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.StackedAreaChart;
import javafx.scene.chart.XYChart;
import javafx.scene.control.*;
import javafx.scene.input.MouseButton;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.HBox;
import javafx.scene.layout.StackPane;
import javafx.scene.layout.VBox;
import javafx.scene.paint.Color;
import javafx.scene.shape.Rectangle;
import javax.swing.Timer;
import mediathek.daten.Daten;
import mediathek.daten.DatenDownload;

public class DownloadInfoController implements Initializable {

    private Scene scene = null;
    private final StackPane chartContainer = new StackPane();
    private final Rectangle zoomRect = new Rectangle();

    private StackedAreaChart<Number, Number> stackedAreaChart;
    private LineChart<Number, Number> lineChart;
    private XYChart<Number, Number> chart = lineChart;

    private int count = 0;
    private int scale = 1;

    private final LinkedList<DatenDownload> startedDownloads = new LinkedList<>(); // Liste gestarteter Downloads
    XYChart.Series<Number, Number> sumSeries = new XYChart.Series<>("Summe", FXCollections.observableArrayList(new XYChart.Data<Number, Number>(0.0, 0.0)));

    private ObservableList<XYChart.Series<Number, Number>> listLineChart = FXCollections.observableArrayList();
    private ObservableList<XYChart.Series<Number, Number>> listAreaChart = FXCollections.observableArrayList();
    ObservableList<XYChart.Series<Number, Number>>[] listAr = new ObservableList[]{listLineChart, listAreaChart};
    //private ObservableList<XYChart.Series<Number, Number>> listDownloadInfos = FXCollections.synchronizedObservableList(FXCollections.observableArrayList());

    public enum KindOfChart {

        LineChart, StackedAreaChart
    }
    KindOfChart kindOfChart = KindOfChart.LineChart;

    @FXML
    private VBox vbChart;

    public void initFX(JFXPanel jfxPanel) {
        try {
            final URL fxmlUrl = getClass().getResource("/mediathek/fx/DownloadInfo.fxml");
            final FXMLLoader fXMLLoader = new FXMLLoader(fxmlUrl);
            fXMLLoader.setController(this);
            final Parent root = fXMLLoader.load();

            makeCharts();
            initList();
            initChart();
            scene = new Scene(root, 250, 150);
            jfxPanel.setScene(scene);

        } catch (Exception exc) {
            exc.printStackTrace();
        }
    }

    public void startSearch() {
        Timer timer = new Timer(1000, e -> {
            search();
        });
        timer.setInitialDelay(4000); // damit auch alles geladen ist
        timer.start();
    }

    public void clearInfos() {
        initList();
    }

    private void makeCharts() {
        ContextMenu cm = getContext();
        stackedAreaChart = new StackedAreaChart<>(createAxis(), createAxis());
        stackedAreaChart.setCreateSymbols(false);
        stackedAreaChart.setData(listAreaChart);
        setupChart(stackedAreaChart, cm);

        lineChart = new LineChart<>(createAxis(), createAxis());
        lineChart.setCreateSymbols(false);
        lineChart.setData(listLineChart);
        setupChart(lineChart, cm);
    }

    private NumberAxis createAxis() {
        final NumberAxis xAxis = new NumberAxis();
        xAxis.setAutoRanging(true);
        xAxis.setLowerBound(0.0);
        return xAxis;
    }

    private void setupChart(XYChart<Number, Number> cha, ContextMenu cm) {
        cha.setTitle("Downloads");
        cha.getXAxis().setLabel("Zeit [min]");
        cha.getYAxis().setLabel("Bandbreite");
        cha.setAnimated(false);

        cha.setOnMouseClicked(e -> {
            if (e.getButton() == MouseButton.SECONDARY) {
                cm.show(cha, e.getSceneX(), e.getSceneY());
            }
        });
    }

    private ContextMenu getContext() {
        final ContextMenu cm = new ContextMenu();
        ToggleGroup tg = new ToggleGroup();

        RadioMenuItem rmiLineChart = new RadioMenuItem("LineChart");
        rmiLineChart.setOnAction(e -> {
            changeChart(KindOfChart.LineChart);
        });
        rmiLineChart.setSelected(true);

        RadioMenuItem rmiAreaChart = new RadioMenuItem("StackedAreaChart");
        rmiAreaChart.setOnAction(e -> {
            changeChart(KindOfChart.StackedAreaChart);
        });
        rmiAreaChart.setSelected(false);

        rmiLineChart.setToggleGroup(tg);
        rmiAreaChart.setToggleGroup(tg);

        MenuItem delData = new MenuItem("Löschen");
        delData.setOnAction(e -> {
            clearInfos();
        });

        MenuItem showAll = new MenuItem("alles anzeigen");
        showAll.setOnAction(e -> {
            resetZoom();
        });

        cm.getItems().add(rmiLineChart);
        cm.getItems().add(rmiAreaChart);
        cm.getItems().add(new SeparatorMenuItem());

        cm.getItems().add(delData);
        cm.getItems().add(showAll);

        return cm;
    }

    private synchronized void initList() {
        listAreaChart.clear();
        listLineChart.clear();
        sumSeries.getData().clear();
        listLineChart.add(sumSeries);
        scale = 1;
    }

    private void initChart() {
        stackedAreaChart.setVisible(false);
        lineChart.setVisible(true);
        chart = lineChart;

        chartContainer.getChildren().add(lineChart);
        chartContainer.getChildren().add(stackedAreaChart);

        zoomRect.setManaged(false);
        zoomRect.setFill(Color.LIGHTSEAGREEN.deriveColor(0, 1, 1, 0.5));
        chartContainer.getChildren().add(zoomRect);
        setUpZooming(zoomRect, chart);

        final HBox controls = new HBox(10);
        controls.setPadding(new Insets(10));
        controls.setAlignment(Pos.CENTER);

        final Button clearButton = new Button("Löschen");
        final Button resetButton = new Button("Alles anzeigen");
        clearButton.setOnAction((ActionEvent event) -> {
            Alert alert = new Alert(Alert.AlertType.CONFIRMATION);
            alert.setTitle("Löschen");
            alert.setHeaderText("Downloadinfos löschen");
            alert.setContentText("Sollen alle Infos gelöscht werden?");

            Optional<ButtonType> bt = alert.showAndWait();
            if (bt.isPresent() && bt.get() == ButtonType.OK) {
                clearInfos();
            }
        });
        resetButton.setOnAction((ActionEvent event) -> {
            resetZoom();
        });
        controls.getChildren().addAll(clearButton, resetButton);

        vbChart.getChildren().add(chartContainer);
        vbChart.getChildren().add(controls);
    }

    private void setUpZooming(final Rectangle rect, final Node zoomingNode) {
        final ObjectProperty<Point2D> mouseAnchor = new SimpleObjectProperty<>();
        zoomingNode.setOnMousePressed((MouseEvent event) -> {
            mouseAnchor.set(new Point2D(event.getX(), event.getY()));
            rect.setWidth(0);
            rect.setHeight(0);
        });
        zoomingNode.setOnMouseDragged((MouseEvent event) -> {
            double x1 = event.getX();
            double y1 = event.getY();
            rect.setX(Math.min(x1, mouseAnchor.get().getX()));
            rect.setY(Math.min(y1, mouseAnchor.get().getY()));
            rect.setWidth(Math.abs(x1 - mouseAnchor.get().getX()));
            rect.setHeight(Math.abs(y1 - mouseAnchor.get().getY()));
            if (event.equals(MouseEvent.MOUSE_RELEASED)) {
                doZoom(rect, chart);
            }
        });
        zoomingNode.setOnMouseReleased((MouseEvent event) -> {
            if (rect.getWidth() > 5 && rect.getHeight() > 5) {
                chart.getXAxis().setAutoRanging(false);
                chart.getYAxis().setAutoRanging(false);
                doZoom(rect, chart);
            }
        });
    }

    private void resetZoom() {
        chart.getXAxis().setAutoRanging(true);
        chart.getYAxis().setAutoRanging(true);
        final NumberAxis xAxis = (NumberAxis) chart.getXAxis();
        xAxis.setLowerBound(0);
        xAxis.setUpperBound(1000);
        final NumberAxis yAxis = (NumberAxis) chart.getYAxis();
        yAxis.setLowerBound(0);
        yAxis.setUpperBound(1000);

        zoomRect.setWidth(0);
        zoomRect.setHeight(0);
    }

    private void doZoom(Rectangle zoomRect, XYChart<Number, Number> chart) {
        Point2D zoomTopLeft = new Point2D(zoomRect.getX(), zoomRect.getY());
        Point2D zoomBottomRight = new Point2D(zoomRect.getX() + zoomRect.getWidth(), zoomRect.getY() + zoomRect.getHeight());
        final NumberAxis yAxis = (NumberAxis) chart.getYAxis();
        Point2D yAxisInScene = yAxis.localToScene(0, 0);
        final NumberAxis xAxis = (NumberAxis) chart.getXAxis();
        Point2D xAxisInScene = xAxis.localToScene(0, 0);
        double xOffset = zoomTopLeft.getX() - yAxisInScene.getX();
        double yOffset = zoomBottomRight.getY() - xAxisInScene.getY();
        double xAxisScale = xAxis.getScale();
        double yAxisScale = yAxis.getScale();
        xAxis.setLowerBound(xAxis.getLowerBound() + xOffset / xAxisScale);
        xAxis.setUpperBound(xAxis.getLowerBound() + zoomRect.getWidth() / xAxisScale);
        yAxis.setLowerBound(yAxis.getLowerBound() + yOffset / yAxisScale);
        yAxis.setUpperBound(yAxis.getLowerBound() - zoomRect.getHeight() / yAxisScale);
        System.out.println(yAxis.getLowerBound() + " " + yAxis.getUpperBound());
        zoomRect.setWidth(0);
        zoomRect.setHeight(0);
    }

    private void changeChart(KindOfChart koc) {
        kindOfChart = koc;

        chart.setVisible(false);
        switch (kindOfChart) {
            case StackedAreaChart:
                chart = stackedAreaChart;
                break;
            case LineChart:
            default:
                chart = lineChart;
                break;

        }
        chart.setVisible(true);
        setUpZooming(zoomRect, chart);
    }

    private synchronized void search() {
        Platform.runLater(() -> {
            searchInfos();
            modifyChartData();
        });
    }

    private synchronized void searchInfos() {
        boolean found;
        ++count;
        final double count_ = ((double) ((int) (count / 0.6))) / 100;
        Daten.listeDownloads.getListOfStartsNotFinished(DatenDownload.QUELLE_ALLE, startedDownloads);

        //Downloads in "Diagramm" eintragen
        for (DatenDownload download : startedDownloads) {
            //jeden Download eintragen
            for (ObservableList<XYChart.Series<Number, Number>> l : listAr) {
                found = false;
                for (XYChart.Series<Number, Number> cSeries : l) {
                    if (cSeries.getName().equals(download.nr + "")) {
                        //dann gibts den schon
                        cSeries.getData().add(new XYChart.Data<>(count_, download.start.bandbreite / scale));
                        found = true;
                    }
                }
                if (!found) {
                    l.add(new XYChart.Series<Number, Number>(download.nr + "",
                            FXCollections.observableArrayList(new XYChart.Data<Number, Number>(count_, download.start.bandbreite / scale))));
                }
            }
        }
        sumSeries.getData().add(new XYChart.Data<>(count_, Daten.downloadInfos.bandwidth / scale));
    }

    private synchronized void modifyChartData() {
        double max = 0;

        for (ObservableList<XYChart.Series<Number, Number>> l : listAr) {
            for (XYChart.Series<Number, Number> cSeries : l) {
                for (XYChart.Data<Number, Number> date : cSeries.getData()) {
                    if ((long) date.getYValue() > max) {
                        max = (long) date.getYValue();
                    }
                }
            }
        }

        if (max > 5_000) {
            scale *= 1000;
            for (ObservableList<XYChart.Series<Number, Number>> l : listAr) {
                for (XYChart.Series<Number, Number> cSeries : l) {
                    for (XYChart.Data<Number, Number> date : cSeries.getData()) {
                        date.setYValue((long) date.getYValue() / 1_000);
                    }
                }
            }
        }

    }

    @Override
    public void initialize(URL url, ResourceBundle rb) {
    }

}
