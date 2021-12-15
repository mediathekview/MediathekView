package mediathek.javafx.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.animation.PauseTransition;
import javafx.collections.ListChangeListener;
import javafx.util.Duration;
import mediathek.controller.SenderFilmlistLoadApprover;
import org.controlsfx.control.CheckListView;

public class SenderBoxNode extends CheckListView<String> {
    public final PauseTransition pauseTransition = new PauseTransition(Duration.millis(500d));

    public SenderBoxNode() {
        //do not display unchecked(unloaded) senders from config...
        EventList<String> senderList = new BasicEventList<>();
        senderList.addAll(SenderListBoxModel.getReadOnlySenderList());
        senderList.removeIf(s -> !SenderFilmlistLoadApprover.isApproved(s));

        setItems(new EventObservableList<>(senderList));

        getCheckModel().getCheckedItems().
                addListener((ListChangeListener<String>) c -> pauseTransition.playFromStart());
    }
}
