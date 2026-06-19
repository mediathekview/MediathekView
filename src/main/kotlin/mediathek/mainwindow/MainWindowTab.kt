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

package mediathek.mainwindow

import java.util.function.BooleanSupplier
import java.util.function.Supplier
import javax.swing.Action
import javax.swing.Icon
import javax.swing.JComponent

class MainWindowTab(
    val title: String,
    private val componentFactory: () -> JComponent,
    val visible: BooleanSupplier = BooleanSupplier { true },
    val icon: Supplier<Icon?>? = null,
    private val toggleActionFactory: (() -> Action?)? = null,
    private val onComponentCreated: (JComponent) -> Unit = {},
    private val dispose: (JComponent) -> Unit = {},
) {
    private var component: JComponent? = null

    fun component(): JComponent =
        component ?: componentFactory().also {
            onComponentCreated(it)
            component = it
        }

    fun existingComponent(): JComponent? = component

    fun toggleAction(): Action? = toggleActionFactory?.invoke()

    fun dispose() {
        component?.let(dispose)
        component = null
    }
}
