package mediathek.gui.messages;

public class ButtonPanelVisibilityChangedEvent extends BaseEvent {
    public boolean visible;

    public ButtonPanelVisibilityChangedEvent(boolean visible) {
        this.visible = visible;
    }
}
