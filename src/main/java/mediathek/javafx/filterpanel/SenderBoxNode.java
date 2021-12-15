package mediathek.javafx.filterpanel;

import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.animation.PauseTransition;
import javafx.collections.ListChangeListener;
import javafx.util.Duration;
import org.controlsfx.control.CheckListView;

public class SenderBoxNode extends CheckListView<String> {
    public final PauseTransition pauseTransition = new PauseTransition(Duration.millis(500d));

    public SenderBoxNode() {
        setItems(new EventObservableList<>(SenderListBoxModel.getProvidedSenderList()));

        getCheckModel().getCheckedItems().
                addListener((ListChangeListener<String>) c -> pauseTransition.playFromStart());
    }
}
