package mediathek.gui.messages;

public class TableModelChangeEvent extends BaseEvent {
    public boolean active;

    public TableModelChangeEvent(boolean active) {
        this.active = active;
    }
}
