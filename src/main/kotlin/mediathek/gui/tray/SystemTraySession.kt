package mediathek.gui.tray

interface SystemTraySession : AutoCloseable {
    override fun close()
}
