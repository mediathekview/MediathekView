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
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program. If not, see <http://www.gnu.org/licenses/>.
 */
package ca.odell.glazedlists.swing

import ca.odell.glazedlists.TextFilterator
import ca.odell.glazedlists.matchers.TextMatcherEditor
import java.awt.event.ActionEvent
import java.awt.event.ActionListener
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.util.regex.Pattern
import javax.swing.JTextField
import javax.swing.event.DocumentEvent
import javax.swing.event.DocumentListener
import javax.swing.text.BadLocationException
import javax.swing.text.Document
import javax.swing.text.JTextComponent

/**
 * A [TextMatcherEditor] coupled to a Swing [Document] or [JTextComponent].
 *
 * The editor can update on every document change or, for a [JTextField], only
 * when an action is fired. Call [dispose] to release the installed listeners.
 */
open class TextComponentMatcherEditor<E> private constructor(
    textComponent: JTextComponent?,
    document: Document?,
    textFilterator: TextFilterator<in E>?,
    live: Boolean,
) : TextMatcherEditor<E>(textFilterator) {
    private val filterHandler = FilterHandler()
    private var live = live

    /** Whether filtering occurs for every document change. */
    open var isLive: Boolean
        get() = live
        set(value) {
            if (value == live) return
            filterHandler.deregisterListeners(live)
            live = value
            filterHandler.registerListeners(live)
        }

    init {
        filterHandler.initialize(textComponent, document)
        filterHandler.registerListeners(live)
        filterHandler.refilter()
    }

    constructor(
        textComponent: JTextComponent,
        textFilterator: TextFilterator<in E>?,
    ) : this(textComponent, textComponent.document, textFilterator, true)

    constructor(
        textComponent: JTextComponent,
        textFilterator: TextFilterator<in E>?,
        live: Boolean,
    ) : this(textComponent, textComponent.document, textFilterator, live)

    constructor(
        document: Document?,
        textFilterator: TextFilterator<in E>?,
    ) : this(null, document, textFilterator, true)

    /** Stops this editor from listening to its document and text component. */
    open fun dispose() {
        filterHandler.deregisterListeners(live)
    }

    private open inner class FilterHandler : DocumentListener,
        ActionListener,
        PropertyChangeListener {
        private var textComponent: JTextComponent? = null
        private var document: Document? = null
        private var listenerMode = true
        private val whitespacePattern = Pattern.compile("[ \\t]")

        fun initialize(textComponent: JTextComponent?, document: Document?) {
            this.textComponent = textComponent
            this.document = document
        }

        fun registerListeners(live: Boolean) {
            if (live) {
                document!!.addDocumentListener(this)
            } else {
                val component = textComponent
                    ?: throw IllegalArgumentException(
                        "Non-live filtering supported only for JTextField (document provided)",
                    )
                if (component !is JTextField) {
                    throw IllegalArgumentException(
                        "Non-live filtering supported only for JTextField (argument class ${component.javaClass.name})",
                    )
                }
                component.addActionListener(this)
            }

            textComponent?.addPropertyChangeListener(this)
            listenerMode = live
        }

        fun deregisterListeners(live: Boolean) {
            if (live) {
                document!!.removeDocumentListener(this)
            } else {
                (textComponent as JTextField).removeActionListener(this)
            }

            textComponent?.removePropertyChangeListener(this)
        }

        fun refilter() {
            try {
                val currentMode = mode
                val currentDocument = document!!
                val text = currentDocument.getText(0, currentDocument.length)
                val filters = when (currentMode) {
                    CONTAINS -> whitespacePattern.split(text)
                    STARTS_WITH, REGULAR_EXPRESSION, EXACT -> arrayOf(text)
                    else -> throw IllegalStateException("Unknown mode: $currentMode")
                }
                setFilterText(filters)
            } catch (exception: BadLocationException) {
                throw RuntimeException(exception)
            }
        }

        override fun insertUpdate(event: DocumentEvent?) {
            refilter()
        }

        override fun removeUpdate(event: DocumentEvent?) {
            refilter()
        }

        override fun changedUpdate(event: DocumentEvent?) {
            refilter()
        }

        override fun actionPerformed(event: ActionEvent?) {
            refilter()
        }

        override fun propertyChange(event: PropertyChangeEvent?) {
            if (event!!.propertyName == "document") {
                val live = listenerMode
                deregisterListeners(live)
                document = textComponent!!.document
                registerListeners(live)
                refilter()
            }
        }
    }
}
