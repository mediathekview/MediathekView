package mediathek.gui.messages

data class TableModelChangeEvent(@JvmField val active: Boolean, @JvmField val fromSearchField: Boolean) : BaseEvent()
