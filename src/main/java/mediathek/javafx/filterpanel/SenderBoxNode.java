package mediathek.javafx.filterpanel;

import ca.odell.glazedlists.BasicEventList;
import ca.odell.glazedlists.EventList;
import ca.odell.glazedlists.javafx.EventObservableList;
import javafx.scene.control.ContextMenu;
import javafx.scene.control.MenuItem;
import mediathek.controller.SenderFilmlistLoadApprover;
import org.controlsfx.control.CheckListView;

public class SenderBoxNode extends CheckListView<String> {
    public SenderBoxNode() {
        //do not display unchecked(unloaded) senders from config...
        EventList<String> senderList = new BasicEventList<>();
        senderList.addAll(SenderListBoxModel.getReadOnlySenderList());
        senderList.removeIf(s -> !SenderFilmlistLoadApprover.isApproved(s));

        setItems(new EventObservableList<>(senderList));

        var contextMenu = new ContextMenu();
        var miClearChecks = new MenuItem("Alle Senderfilter zurücksetzen");
        miClearChecks.setOnAction(e -> getCheckModel().clearChecks());
        contextMenu.getItems().add(miClearChecks);
        setContextMenu(contextMenu);
    }
}
