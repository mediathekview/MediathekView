package mediathek.tool.cellrenderer

import mediathek.daten.DatenPset
import mediathek.tool.SVGIconUtilities
import org.apache.logging.log4j.LogManager
import java.awt.Component
import javax.swing.JTable
import javax.swing.SwingConstants
import javax.swing.table.DefaultTableCellRenderer

class CellRendererPset : DefaultTableCellRenderer() {
    override fun getTableCellRendererComponent(
        table: JTable,
        value: Any?,
        isSelected: Boolean,
        hasFocus: Boolean,
        row: Int,
        column: Int,
    ): Component {
        background = null
        foreground = null
        font = null
        icon = null
        horizontalAlignment = SwingConstants.LEADING
        super.getTableCellRendererComponent(table, value, isSelected, hasFocus, row, column)
        try {
            val modelRow = table.convertRowIndexToModel(row)
            val modelColumn = table.convertColumnIndexToModel(column)
            val datenPset = DatenPset()
            for (i in 0 until DatenPset.MAX_ELEM) {
                datenPset[i] = table.model.getValueAt(modelRow, i).toString()
            }
            if (modelColumn == DatenPset.PROGRAMMSET_NAME) {
                foreground = datenPset.farbe
            }
            if (modelColumn == DatenPset.PROGRAMMSET_IST_ABSPIELEN) {
                drawBooleanIcon(datenPset.istAbspielen())
            }
            if (modelColumn == DatenPset.PROGRAMMSET_IST_SPEICHERN) {
                drawBooleanIcon(datenPset.istSpeichern())
            }
        } catch (ex: Exception) {
            logger.error("getTableCellRendererComponent", ex)
        }
        return this
    }

    private fun drawBooleanIcon(enabled: Boolean) {
        horizontalAlignment = SwingConstants.CENTER
        text = ""
        icon = if (enabled) checkIcon else null
    }

    private companion object {
        private val checkIcon = SVGIconUtilities.createSVGIcon("icons/fontawesome/check.svg")
        private val logger = LogManager.getLogger()
    }
}
