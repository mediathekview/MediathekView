package mediathek.javafx.filterpanel;

import javafx.animation.PauseTransition;
import javafx.collections.ListChangeListener;
import javafx.scene.control.Label;
import javafx.scene.layout.Priority;
import javafx.scene.layout.VBox;
import javafx.util.Duration;

public class SenderBoxNode extends VBox {
    public final PauseTransition pauseTransition = new PauseTransition(Duration.millis(500d));
    public SenderListBox senderBox;

    public SenderBoxNode() {
        senderBox = new SenderListBox();
        VBox.setVgrow(senderBox, Priority.ALWAYS);
        getChildren().addAll(
                new Label("Sender:"),
                senderBox);
        senderBox.getCheckModel().getCheckedItems().
                addListener((ListChangeListener<String>) changeListener -> pauseTransition.playFromStart());
    }
}
