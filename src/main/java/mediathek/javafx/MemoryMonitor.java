package mediathek.javafx;

import javafx.animation.Animation;
import javafx.animation.KeyFrame;
import javafx.animation.Timeline;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.geometry.Pos;
import javafx.scene.Scene;
import javafx.scene.chart.LineChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.layout.*;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.util.Duration;

import java.util.concurrent.atomic.AtomicInteger;

public class MemoryMonitor extends Stage {
    private Timeline updateMemoryTimer;
    private AtomicInteger time = new AtomicInteger();
    private LongProperty totalMemory;
    private LongProperty freeMemory;
    private LongProperty maxMemory;
    private XYChart.Series<Number, Number> series = new XYChart.Series<>();
    private NumberBinding usedMemory;

    public MemoryMonitor() {
        super();
        initComponents();

        btnPerformGc.setOnAction(e -> {
            System.gc();
        });
    }

    private void initComponents() {
        setTitle("Memory");
        setAlwaysOnTop(true);
        initStyle(StageStyle.UTILITY);

        createPropertiesAndBindings();

        Scene scene = new Scene(createMemoryMonitor());
        setScene(scene);

        setOnHiding(e -> updateMemoryTimer.stop());
        setOnShowing(e -> updateMemoryTimer.play());
    }

    private long toMegabytes(long bytes) {
        return bytes / (1024 * 1024);
    }

    private void createPropertiesAndBindings() {
        totalMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().totalMemory()));
        freeMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().freeMemory()));
        maxMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().maxMemory()));

        usedMemory = totalMemory.subtract(freeMemory);
    }

    private static final int TIMELINE_SIZE = 60;

    private void createUpdateTimer() {
        updateMemoryTimer = new Timeline(new KeyFrame(Duration.seconds(1), event -> {
            totalMemory.set(toMegabytes(Runtime.getRuntime().totalMemory()));
            freeMemory.set(toMegabytes(Runtime.getRuntime().freeMemory()));
            maxMemory.set(toMegabytes(Runtime.getRuntime().maxMemory()));

            series.getData().add(new XYChart.Data<>(time.incrementAndGet(), usedMemory.getValue()));
            if (series.getData().size() > TIMELINE_SIZE) {
                series.getData().subList(0, series.getData().size() - TIMELINE_SIZE).clear();
            }
        }));
        updateMemoryTimer.setCycleCount(Animation.INDEFINITE);
        updateMemoryTimer.play();
    }

    private Pane createMemoryMonitor() {
        //TODO implement GC button
        series.setName("Speicherverbrauch (MByte)");

        createUpdateTimer();

        return new BorderPane(createChart(), createLabels(), null, null, null);
    }

    private Button btnPerformGc = new Button("Perform GC");

    private HBox createLabels() {
        HBox hb = new HBox();
        hb.setSpacing(5d);
        hb.getChildren().add(btnPerformGc);

        Label lblUsed = new Label();
        lblUsed.textProperty().bind(usedMemory.asString("Used: %,d"));

        Label lblFree = new Label();
        lblFree.textProperty().bind(freeMemory.asString("Free: %,d"));

        Label lblTotal = new Label();
        lblTotal.textProperty().bind(totalMemory.asString("Total: %,d"));

        Label lblMax = new Label();
        lblMax.textProperty().bind(maxMemory.asString("Max: %,d"));

        VBox labels = new VBox(lblUsed, lblFree, lblTotal, lblMax);
        labels.setAlignment(Pos.CENTER);

        hb.getChildren().add(labels);
        return hb;
    }

    private Region createChart() {
        NumberAxis xAxis = new NumberAxis();
        xAxis.setLabel("Laufzeit");
        xAxis.setForceZeroInRange(false);

        NumberAxis yAxis = new NumberAxis();
        yAxis.setLabel("Speicher");

        LineChart<Number, Number> chart = new LineChart<>(xAxis, yAxis);
        chart.setAnimated(false);
        chart.getData().add(series);
        chart.createSymbolsProperty().setValue(false);

        return chart;
    }
}
