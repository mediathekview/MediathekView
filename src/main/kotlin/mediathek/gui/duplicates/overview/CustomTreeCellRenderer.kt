package mediathek.gui.duplicates.overview

import mediathek.daten.DatenFilm
import java.awt.Component
import javax.swing.JTree
import javax.swing.tree.DefaultMutableTreeNode
import javax.swing.tree.DefaultTreeCellRenderer

class CustomTreeCellRenderer : DefaultTreeCellRenderer() {
    override fun getTreeCellRendererComponent(
        tree: JTree,
        value: Any,
        selected: Boolean,
        expanded: Boolean,
        leaf: Boolean,
        row: Int,
        hasFocus: Boolean,
    ): Component {
        val component = super.getTreeCellRendererComponent(
            tree,
            value,
            selected,
            expanded,
            leaf,
            row,
            hasFocus,
        )
        val node = value as DefaultMutableTreeNode
        if (node.isRoot) {
            return component
        }

        when (val userObject = node.userObject) {
            is DatenFilm -> {
                text = userObject.title
                toolTipText = prepareTooltipText(userObject)
            }

            is String -> text = "%s (%d)".format(userObject, node.childCount)
            else -> text = value.toString()
        }

        return component
    }

    private fun prepareTooltipText(film: DatenFilm): String = """
        <html>
        <b>Thema:</b> ${film.thema}<br>
        <b>Titel:</b> ${film.title}<br>
        <b>gesendet:</b> ${film.sendeDatum} ${film.sendeZeit}<br>
        </html>
    """.trimIndent()
}
