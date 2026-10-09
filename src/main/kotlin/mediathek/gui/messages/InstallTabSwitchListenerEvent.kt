package mediathek.gui.messages

class InstallTabSwitchListenerEvent(val event: INSTALL_TYPE) : BaseEvent() {
    enum class INSTALL_TYPE {
        INSTALL,
        REMOVE,
    }
}
