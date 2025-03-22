package mediathek.javaswing.filterpanel

import javax.swing.JSpinner
import javax.swing.SpinnerListModel


class ZeitraumSpinnerSwing : JSpinner() {
    companion object {
        const val UNLIMITED_VALUE = "∞"
    }

    init {
        val days = mutableListOf(UNLIMITED_VALUE)
        for (i in 1..365) {
            days.add(i.toString())
        }

        model = SpinnerListModel(days)
        value = UNLIMITED_VALUE

        (editor as? JSpinner.DefaultEditor)?.textField?.isEditable = true

        toolTipText = """
            <html>Geben Sie Werte von 1-365 manuell ein und drücken Sie ENTER.<br>
            Für unbegrenzten Zeitraum das "∞"-Symbol eingeben.</html>
        """.trimIndent()
    }
}
