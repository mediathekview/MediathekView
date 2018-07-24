package mediathek.gui.messages;

public class InstallTabSwitchListenerEvent extends BaseEvent {
    public enum INSTALL_TYPE {INSTALL, REMOVE}

    public INSTALL_TYPE event;
}
