/*
 * Copyright (c) 2026 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.gui.tabs.tab_film.filter

import mediathek.config.application.FilterConfiguration
import mediathek.gui.tabs.tab_film.filter_selection.FilmFilterSelectionController
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBox
import mediathek.gui.tabs.tab_film.filter_selection.FilterSelectionComboBoxModel
import mediathek.tool.FilterDTO
import org.apache.commons.configuration2.XMLConfiguration
import java.awt.GraphicsEnvironment
import java.lang.reflect.Field
import java.util.*
import java.util.concurrent.atomic.AtomicReference
import javax.swing.JCheckBox
import javax.swing.JToggleButton
import javax.swing.SwingUtilities
import javax.swing.UIManager

internal object SwingFilterDialogTestFixture {
    fun assumeUiAvailable() {
        org.junit.jupiter.api.Assumptions.assumeFalse(
            GraphicsEnvironment.isHeadless(),
            "Swing dialog test requires a non-headless environment"
        )
    }

    fun createDialogSetup(
        getThemen: (Collection<String>) -> List<String> = { emptyList() },
    ): DialogSetup {
        val requestedNewFilterName = AtomicReference<String?>(null)
        val requestedRenameName = AtomicReference<String?>(null)
        val filterConfiguration = TestFilterConfiguration(XMLConfiguration())
        val firstFilter = FilterDTO(UUID.randomUUID(), "Filter 1")
        val secondFilter = FilterDTO(UUID.randomUUID(), "Filter 2")
        val zeitraumFilter = FilterDTO(UUID.randomUUID(), "Filter 3")
        filterConfiguration.addNewFilter(firstFilter)
        filterConfiguration.addNewFilter(secondFilter)
        filterConfiguration.addNewFilter(zeitraumFilter)
        filterConfiguration.setCurrentFilter(firstFilter)

        val reloadRequester = RecordingReloadRequester()
        val senderList = listOf("ARD", "3Sat")
        val controller = FilmFilterController(
            filterConfiguration,
            dataProvider = object : FilmFilterController.DataProvider {
                override fun senderList() = senderList
                override fun getThemen(senders: Collection<String>) = getThemen.invoke(senders)
            },
            reloadRequester = reloadRequester
        )

        controller.restoreCurrentFilterSelection(secondFilter)
        controller.onShowNewOnlyChanged(true)

        controller.restoreCurrentFilterSelection(zeitraumFilter)
        controller.onZeitraumChanged("7")

        controller.restoreCurrentFilterSelection(firstFilter)

        val selectionController = FilmFilterSelectionController(controller, reloadRequester)
        val model = FilterSelectionComboBoxModel(
            controller::currentFilter,
            controller::availableFilters,
            controller::isFilterLocked,
            controller.selectionObserverRegistry(),
            selectionController::select,
        )

        val dialog = withCrossPlatformLookAndFeel {
            onEdtResult {
                SwingFilterDialog(
                    owner = javax.swing.JFrame(),
                    filterSelectionComboBoxModel = model,
                    filterToggleButton = JToggleButton(),
                    filterController = controller,
                    prompts = object : SwingFilterDialog.DialogPrompts {
                        override fun confirmDeleteCurrentFilter() = true
                        override fun confirmResetCurrentFilter() = true
                        override fun requestNewFilterName(suggestedName: String) = requestedNewFilterName.get()
                        override fun requestRenameFilterName(currentFilterName: String) = requestedRenameName.get()
                    }
                )
            }
        }

        return DialogSetup(
            dialog = dialog,
            model = model,
            controller = controller,
            reloadRequester = reloadRequester,
            comboBox = comboBoxOf(dialog),
            requestedNewFilterName = requestedNewFilterName,
            requestedRenameName = requestedRenameName,
            secondFilter = secondFilter,
            zeitraumFilter = zeitraumFilter
        )
    }

    fun onEdt(action: () -> Unit) {
        SwingUtilities.invokeAndWait(action)
    }

    fun <T> onEdtResult(action: () -> T): T {
        var result: T? = null
        SwingUtilities.invokeAndWait {
            result = action()
        }
        @Suppress("UNCHECKED_CAST")
        return result as T
    }

    private fun comboBoxOf(dialog: SwingFilterDialog): FilterSelectionComboBox {
        val field: Field = dialog.javaClass.superclass.getDeclaredField("cboxFilterSelection")
        field.isAccessible = true
        @Suppress("UNCHECKED_CAST")
        return field.get(dialog) as FilterSelectionComboBox
    }

    fun triggerResetCurrentFilter(dialog: SwingFilterDialog) {
        dialog.triggerResetCurrentFilter()
    }

    fun triggerDeleteCurrentFilter(dialog: SwingFilterDialog) {
        dialog.triggerDeleteCurrentFilter()
    }

    fun triggerRenameCurrentFilter(setup: DialogSetup, newName: String?) {
        setup.requestedRenameName.set(newName)
        setup.dialog.triggerRenameCurrentFilter()
    }

    fun triggerAddNewFilter(setup: DialogSetup, newName: String?) {
        setup.requestedNewFilterName.set(newName)
        setup.dialog.triggerAddNewFilter()
    }

    fun triggerCloneCurrentFilter(dialog: SwingFilterDialog) {
        dialog.triggerCloneCurrentFilter()
    }

    fun showNewOnlyCheckBox(dialog: SwingFilterDialog): JCheckBox {
        val field = dialog.javaClass.superclass.getDeclaredField("cbShowNewOnly")
        field.isAccessible = true
        return field.get(dialog) as JCheckBox
    }

    fun lockCurrentFilterCheckBox(dialog: SwingFilterDialog): JCheckBox {
        val field = dialog.javaClass.superclass.getDeclaredField("cbLockCurrentFilter")
        field.isAccessible = true
        return field.get(dialog) as JCheckBox
    }

    fun zeitraumSpinnerValue(dialog: SwingFilterDialog): Int {
        val field = dialog.javaClass.superclass.getDeclaredField("spZeitraum")
        field.isAccessible = true
        return (field.get(dialog) as ZeitraumSpinner).value as Int
    }

    private fun <T> withCrossPlatformLookAndFeel(action: () -> T): T {
        val originalLookAndFeel = UIManager.getLookAndFeel()
        onEdt {
            UIManager.setLookAndFeel(UIManager.getCrossPlatformLookAndFeelClassName())
        }
        return try {
            action()
        } finally {
            onEdt {
                UIManager.setLookAndFeel(originalLookAndFeel)
            }
        }
    }

    data class DialogSetup(
        val dialog: SwingFilterDialog,
        val model: FilterSelectionComboBoxModel,
        val controller: FilmFilterController,
        val reloadRequester: RecordingReloadRequester,
        val comboBox: FilterSelectionComboBox,
        val requestedNewFilterName: AtomicReference<String?>,
        val requestedRenameName: AtomicReference<String?>,
        val secondFilter: FilterDTO,
        val zeitraumFilter: FilterDTO,
    )

    class RecordingReloadRequester : FilmFilterController.ReloadRequester {
        var tableReloadRequests = 0
        var zeitraumReloadRequests = 0

        override fun requestTableReload() {
            tableReloadRequests++
        }

        override fun requestZeitraumReload() {
            zeitraumReloadRequests++
        }
    }

    private class TestFilterConfiguration(configuration: XMLConfiguration) : FilterConfiguration(configuration)

}
