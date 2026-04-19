package mediathek.gui.messages

data class TableModelChangeEvent(val active: Boolean, val fromSearchField: Boolean) : BaseEvent()
