package mediathek.gui.messages

open class UpdateStateChangedEvent(
    val isActive: Boolean,
) : BaseEvent()
