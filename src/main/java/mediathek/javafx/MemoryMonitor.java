package mediathek.javafx;

import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;
import javafx.animation.*;
import javafx.beans.binding.NumberBinding;
import javafx.beans.property.LongProperty;
import javafx.beans.property.SimpleLongProperty;
import javafx.geometry.Insets;
import javafx.scene.Scene;
import javafx.scene.chart.LineChart;
import javafx.scene.chart.NumberAxis;
import javafx.scene.chart.XYChart;
import javafx.scene.control.Label;
import javafx.scene.input.MouseEvent;
import javafx.scene.layout.*;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.stage.StageStyle;
import javafx.util.Duration;
import mediathek.javafx.tool.JFXHiddenApplication;
import org.apache.commons.io.FileUtils;
import org.apache.commons.lang3.SystemUtils;

import java.util.concurrent.atomic.AtomicInteger;

public class MemoryMonitor extends Stage {
    private static final int TIMELINE_SIZE = 60;
    private final AtomicInteger time = new AtomicInteger();
    private final XYChart.Series<Number, Number> series = new XYChart.Series<>();
    private Timeline updateMemoryTimer;
    private LongProperty totalMemory;
    private LongProperty freeMemory;
    private LongProperty maxMemory;
    private NumberBinding usedMemory;
    private LineChart<Number, Number> chart;

    public MemoryMonitor() {
        super();
        initComponents();
    }

    @SuppressFBWarnings("DM_GC")
    private void initComponents() {
        setTitle("Speicherverbrauch");
        initOwner(JFXHiddenApplication.getPrimaryStage());
        getIcons().add(JFXHiddenApplication.getApplicationImage());
        setAlwaysOnTop(true);
        if (SystemUtils.IS_OS_MAC_OSX)
            initStyle(StageStyle.UTILITY);

        createPropertiesAndBindings();

        Scene scene = new Scene(createMemoryMonitor());
        setScene(scene);

        setOnHiding(e -> updateMemoryTimer.stop());
        setOnShowing(e -> updateMemoryTimer.play());

        addEventHandler(MouseEvent.MOUSE_CLICKED, event -> {
            final Animation animation = new Transition() {
                {
                    setCycleDuration(Duration.millis(1000));
                    setInterpolator(Interpolator.EASE_OUT);
                }

                @Override
                protected void interpolate(double frac) {
                    Color vColor = new Color(1, 0, 0, 1 - frac);
                    chart.setBackground(new Background(new BackgroundFill(vColor, CornerRadii.EMPTY, Insets.EMPTY)));
                }
            };
            animation.play();
            System.gc();
        });
    }

    private long toMegabytes(long bytes) {
        return bytes / FileUtils.ONE_MB;
    }

    private void createPropertiesAndBindings() {
        totalMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().totalMemory()));
        freeMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().freeMemory()));
        maxMemory = new SimpleLongProperty(toMegabytes(Runtime.getRuntime().maxMemory()));

        usedMemory = totalMemory.subtract(freeMemory);
    }

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
        series.setName("Speicherverbrauch (MByte)");

        createUpdateTimer();
        chart = createChart();

        return new BorderPane(chart, createLabels(), null, null, null);
    }

    private Pane createLabels() {
        Label lblUsed = new Label();
        lblUsed.textProperty().bind(usedMemory.asString("Used: %,d"));

        Label lblFree = new Label();
        lblFree.textProperty().bind(freeMemory.asString("Free: %,d"));

        Label lblTotal = new Label();
        lblTotal.textProperty().bind(totalMemory.asString("Total: %,d"));

        Label lblMax = new Label();
        lblMax.textProperty().bind(maxMemory.asString("Max: %,d"));

        HBox labels = new HBox(lblUsed, lblFree, lblTotal, lblMax);
        labels.setSpacing(10d);

        return labels;
    }

    private LineChart<Number, Number> createChart() {
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
