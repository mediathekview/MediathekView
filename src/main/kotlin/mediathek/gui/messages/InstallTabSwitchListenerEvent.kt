package mediathek.gui.messages

open class InstallTabSwitchListenerEvent : BaseEvent() {
    enum class INSTALL_TYPE {
        INSTALL,
        REMOVE,
    }

    var event: INSTALL_TYPE? = null
}
