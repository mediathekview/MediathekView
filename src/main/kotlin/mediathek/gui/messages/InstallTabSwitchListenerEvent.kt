package mediathek.gui.messages

class InstallTabSwitchListenerEvent(val event: InstallType) : BaseEvent() {
    enum class InstallType {
        INSTALL,
        REMOVE,
    }
}
