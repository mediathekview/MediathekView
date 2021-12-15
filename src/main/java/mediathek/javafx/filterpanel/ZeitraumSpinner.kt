package mediathek.javafx.filterpanel

import javafx.collections.FXCollections
import javafx.scene.control.Spinner
import javafx.scene.control.SpinnerValueFactory
import javafx.scene.control.SpinnerValueFactory.ListSpinnerValueFactory
import javafx.scene.control.Tooltip

class ZeitraumSpinner : Spinner<String?>() {
    init {
        val days = FXCollections.observableArrayList(UNLIMITED_VALUE)
        for (i in 1..365)
            days.add(i.toString())
        val valueFactory: SpinnerValueFactory<String?> = ListSpinnerValueFactory(days)
        setValueFactory(valueFactory)
        valueFactory.value = UNLIMITED_VALUE
        isEditable = true
        tooltip = Tooltip(
            """
                Geben Sie Werte von 1-365 manuell ein plus ENTER-Taste.
                Für unbegrenzten Zeitraum das "∞"-Symbol eingeben.
                """.trimIndent()
        )
    }

    companion object {
        const val UNLIMITED_VALUE = "∞"
    }
}