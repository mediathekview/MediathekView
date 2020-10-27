package mediathek.gui.messages;

public class ButtonsPanelVisibilityChangedEvent extends BaseEvent {
    public boolean visible;

    public ButtonsPanelVisibilityChangedEvent(boolean visible) {
        this.visible = visible;
    }
}
