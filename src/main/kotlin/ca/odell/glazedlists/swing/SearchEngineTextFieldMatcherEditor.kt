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
 * along with this program.  If not, see <https://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.SearchEngineTextMatcherEditor
import java.awt.event.ActionListener
import javax.swing.JTextField

/**
 * Filters elements using search-engine-style input from a [JTextField].
 *
 * The field is observed for action events. Call [dispose] when this editor is
 * no longer useful but the field remains referenced.
 */
open class SearchEngineTextFieldMatcherEditor<E>(
    observedTextField: JTextField?,
    textFilterator: TextFilterator<in E>?,
) : SearchEngineTextMatcherEditor<E>(textFilterator) {
    @Suppress("JoinDeclarationAndAssignment")
    private val textField: JTextField?
    private val filterHandler = ActionListener { refilter(observedTextField!!.text) }

    init {
        this.textField = observedTextField
        this.textField!!.addActionListener(filterHandler)
        refilter(observedTextField.text)
    }

    /** Stops observing the text field. */
    open fun dispose() {
        textField!!.removeActionListener(filterHandler)
    }
}
