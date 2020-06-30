package mediathek.javafx.tool;

import javafx.concurrent.Task;
import javafx.scene.control.Label;
import javafx.scene.control.ProgressBar;
import javafx.scene.control.ProgressIndicator;
import javafx.scene.layout.HBox;
import mediathek.javafx.CenteredBorderPane;
import mediathek.javafx.VerticalSeparator;

public class FXProgressPane extends HBox {
    public Label lb;
    public ProgressBar prog;

    public FXProgressPane() {
        super();
        setSpacing(4d);

        lb = new Label();
        prog = new ProgressBar();
        prog.setProgress(ProgressIndicator.INDETERMINATE_PROGRESS);

        getChildren().addAll(new VerticalSeparator(),
                new CenteredBorderPane(lb),
                new CenteredBorderPane(prog));
    }

    public void bindTask(Task<?> task) {
        lb.textProperty().bind(task.messageProperty());
        prog.progressProperty().bind(task.progressProperty());
    }
}
