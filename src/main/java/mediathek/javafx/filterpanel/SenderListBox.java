package mediathek.javafx.filterpanel;

import javafx.collections.ListChangeListener;
import mediathek.config.Daten;
import org.controlsfx.control.CheckListView;

public class SenderListBox extends CheckListView<String> {
    public SenderListBox() {
        super(Daten.getInstance().getListeFilmeNachBlackList().getSenders());
        setPrefHeight(150d);
        setMinHeight(100d);
    }
}
