package mediathek.tool.listener

import mediathek.tool.table.ColumnVisibilityStore
import mediathek.tool.table.MVTable
import java.awt.event.MouseAdapter
import java.awt.event.MouseEvent
import javax.swing.JCheckBoxMenuItem
import javax.swing.JMenuItem
import javax.swing.JPopupMenu

/**
 * Rechte Maustaste in der Tabelle (Kontextmenü)
 */
open class BeobTableHeader(
    protected val tabelle: MVTable,
    private val spaltenAnzeigen: ColumnVisibilityStore,
    /**
     * Column indices which should NOT be displayed
     */
    private val hiddenColumns: IntArray,
    /**
     * Column indices which are supposed to be buttons.
     */
    private val button: IntArray,
    private val displaySenderIconMenus: Boolean,
    private val saveLineBreak: ((Boolean) -> Unit)?,
) : MouseAdapter() {
    private val columns: Array<String>
    private lateinit var box: Array<JCheckBoxMenuItem?>

    init {
        // dynamically query column names from table
        val columnModel = tabelle.tableHeader.columnModel
        columns = Array(columnModel.columnCount) { index ->
            columnModel.getColumn(index).headerValue as String
        }
    }

    override fun mousePressed(event: MouseEvent) {
        if (event.isPopupTrigger) {
            showMenu(event)
        }
    }

    override fun mouseReleased(event: MouseEvent) {
        if (event.isPopupTrigger) {
            showMenu(event)
        }
    }

    private fun immer(index: Int): Boolean =
        hiddenColumns.any { hiddenColumn -> index == hiddenColumn }

    protected open fun toggleButtonVisibility(isSelected: Boolean) {
        for (index in button) {
            setSpalten(index, isSelected)
        }
    }

    protected open fun toggleSenderIconDisplay(isSelected: Boolean) {
        tabelle.setShowIcon(isSelected)
        setSpalten()
    }

    protected open fun prepareMenu(): JPopupMenu {
        val popupMenu = JPopupMenu()
        // Spalten ein-ausschalten
        box = arrayOfNulls(columns.size)
        for (index in columns.indices) {
            if (immer(index)) {
                continue
            }
            box[index] = JCheckBoxMenuItem(columns[index]).apply {
                isSelected = anzeigen(index)
                addActionListener { setSpalten() }
            }
            popupMenu.add(box[index])
        }
        // jetzt evtl. noch die Button
        if (button.isNotEmpty()) {
            popupMenu.addSeparator()

            val item = JCheckBoxMenuItem("Buttons anzeigen")
            item.isSelected = anzeigen(button[0]) // entweder alle oder keiner!
            item.addActionListener { toggleButtonVisibility(item.isSelected) }
            popupMenu.add(item)
        }
        if (displaySenderIconMenus) {
            popupMenu.addSeparator()

            val senderIconsItem = JCheckBoxMenuItem("Sendericons anzeigen")
            senderIconsItem.isSelected = tabelle.showSenderIcons()
            senderIconsItem.addActionListener { toggleSenderIconDisplay(senderIconsItem.isSelected) }
            popupMenu.add(senderIconsItem)

            val smallSenderIconsItem = JCheckBoxMenuItem("Kleine Sendericons anzeigen")
            smallSenderIconsItem.isSelected = tabelle.getUseSmallSenderIcons()
            if (!tabelle.showSenderIcons()) {
                smallSenderIconsItem.isEnabled = false
            } else {
                smallSenderIconsItem.addActionListener {
                    tabelle.setUseSmallSenderIcons(smallSenderIconsItem.isSelected)
                    setSpalten()
                }
            }
            popupMenu.add(smallSenderIconsItem)
        }

        popupMenu.addSeparator()
        if (saveLineBreak != null) {
            // Tabellenspalten umbrechen
            val lineBreakItem = JCheckBoxMenuItem("Zeilen umbrechen")
            lineBreakItem.isSelected = tabelle.isLineBreak()
            lineBreakItem.addActionListener {
                tabelle.setLineBreak(lineBreakItem.isSelected)
                saveLineBreak.invoke(lineBreakItem.isSelected)
                setSpalten()
            }
            popupMenu.add(lineBreakItem)

            popupMenu.addSeparator()
        }

        // Tabellenspalten zurücksetzen
        val resetColumnsItem = JMenuItem("Spalten zurücksetzen")
        resetColumnsItem.addActionListener { tabelle.resetTabelle() }
        popupMenu.add(resetColumnsItem)

        return popupMenu
    }

    private fun showMenu(event: MouseEvent) {
        val popupMenu = prepareMenu()
        popupMenu.show(event.component, event.x, event.y)
    }

    private fun anzeigen(index: Int): Boolean =
        spaltenAnzeigen.isVisible(index)

    private fun setSpalten() {
        for ((index, item) in box.withIndex()) {
            if (item != null) {
                spaltenAnzeigen.setVisible(index, item.isSelected)
            }
        }
        tabelle.spaltenEinAus()
        tabelle.calculateRowHeight()
    }

    protected open fun setSpalten(index: Int, visible: Boolean) {
        spaltenAnzeigen.setVisible(index, visible)
        tabelle.spaltenEinAus()
    }
}
