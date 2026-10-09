/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.matchers

import ca.odell.glazedlists.BasicEventList
import ca.odell.glazedlists.EventList
import ca.odell.glazedlists.event.ListEvent
import ca.odell.glazedlists.event.ListEventListener

/** Combines zero or more delegate matcher editors using AND or OR semantics. */
class CompositeMatcherEditor<E>(
    matcherEditors: EventList<MatcherEditor<E>> = BasicEventList(),
) : AbstractMatcherEditor<E>() {
    private val delegateEditors = matcherEditors
    private val matcherEditorListeners = mutableListOf<DelegateMatcherEditorListener>()
    private var currentMode = AND

    /** The live list of matcher editors combined by this editor. */
    @Suppress("unused")
    val matcherEditors: EventList<MatcherEditor<E>>
        get() = delegateEditors

    /** Whether delegate matchers are combined using [AND] or [OR]. */
    var mode: Int
        get() = currentMode
        set(value) {
            if (currentMode == value) return

            val oldMode = currentMode
            currentMode = value
            when (oldMode) {
                AND -> {
                    require(value == OR) { "Unsupported composite matcher mode: $value" }
                    when {
                        delegateEditors.isEmpty() -> fireMatchNone()
                        delegateEditors.size > 1 -> fireRelaxed(rebuildMatcher())
                    }
                }

                OR -> {
                    require(value == AND) { "Unsupported composite matcher mode: $value" }
                    when {
                        delegateEditors.isEmpty() -> fireMatchAll()
                        delegateEditors.size > 1 -> fireConstrained(rebuildMatcher())
                    }
                }

                else -> throw IllegalArgumentException("Unsupported composite matcher mode: $oldMode")
            }
        }

    init {
        for (matcherEditor in delegateEditors) {
            matcherEditorListeners += DelegateMatcherEditorListener(matcherEditor)
        }
        delegateEditors.addListEventListener(MatcherEditorsListListener())
        fireChanged(rebuildMatcher())
    }

    private fun rebuildMatcher(): Matcher<E> {
        val matchers = Array(delegateEditors.size) { index -> delegateEditors[index].matcher }
        return when (currentMode) {
            AND -> Matchers.and(*matchers)
            OR -> Matchers.or(*matchers)
            else -> error("Unsupported composite matcher mode: $currentMode")
        }
    }

    private inner class MatcherEditorsListListener : ListEventListener<MatcherEditor<E>> {
        override fun listChanged(listChanges: ListEvent<MatcherEditor<E>>) {
            var inserts = false
            var deletes = false
            val wasEmpty = matcherEditorListeners.isEmpty()

            while (listChanges.next()) {
                val index = listChanges.index
                when (listChanges.type) {
                    ListEvent.INSERT -> {
                        val inserted = delegateEditors[index]
                        matcherEditorListeners += DelegateMatcherEditorListener(inserted)
                        inserts = true
                    }

                    ListEvent.DELETE -> {
                        matcherEditorListeners.removeAt(index).stopListening()
                        deletes = true
                    }

                    ListEvent.UPDATE -> {
                        val updated = delegateEditors[index]
                        matcherEditorListeners[index].matcherEditor = updated
                        inserts = true
                        deletes = true
                    }
                }
            }

            val isEmpty = matcherEditorListeners.isEmpty()
            when (currentMode) {
                AND -> when {
                    inserts && deletes -> fireChanged(rebuildMatcher())
                    inserts -> fireConstrained(rebuildMatcher())
                    deletes && isEmpty -> fireMatchAll()
                    deletes -> fireRelaxed(rebuildMatcher())
                }

                OR -> when {
                    inserts && deletes -> fireChanged(rebuildMatcher())
                    inserts && wasEmpty -> fireConstrained(rebuildMatcher())
                    inserts -> fireRelaxed(rebuildMatcher())
                    deletes && isEmpty -> fireMatchAll()
                    deletes -> fireConstrained(rebuildMatcher())
                }

                else -> error("Unsupported composite matcher mode: $currentMode")
            }
        }
    }

    private inner class DelegateMatcherEditorListener(
        source: MatcherEditor<E>,
    ) : MatcherEditor.Listener<E> {
        var matcherEditor: MatcherEditor<E> = source
            set(value) {
                if (field === value) return
                stopListening()
                field = value
                value.addMatcherEditorListener(this)
            }

        init {
            source.addMatcherEditorListener(this)
        }

        override fun changedMatcher(matcherEvent: MatcherEditor.Event<E>) {
            when (matcherEvent.type) {
                MatcherEditor.Event.CONSTRAINED -> constrained()
                MatcherEditor.Event.RELAXED -> relaxed()
                MatcherEditor.Event.CHANGED -> changed()
                MatcherEditor.Event.MATCH_ALL -> matchAll()
                MatcherEditor.Event.MATCH_NONE -> matchNone()
            }
        }

        private fun matchAll() {
            if (delegateEditors.size == 1) fireMatchAll() else fireRelaxed(rebuildMatcher())
        }

        private fun matchNone() {
            if (delegateEditors.size == 1) fireMatchNone() else fireConstrained(rebuildMatcher())
        }

        private fun changed() = fireChanged(rebuildMatcher())

        private fun constrained() = fireConstrained(rebuildMatcher())

        private fun relaxed() = fireRelaxed(rebuildMatcher())

        fun stopListening() {
            matcherEditor.removeMatcherEditorListener(this)
        }
    }

    companion object {
        const val AND = 42
        const val OR = 24
    }
}
