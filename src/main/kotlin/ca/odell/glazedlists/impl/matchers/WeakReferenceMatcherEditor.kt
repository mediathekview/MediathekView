/* Glazed Lists                                                 (c) 2003-2014 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.matchers

import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.MatcherEditor
import java.lang.ref.WeakReference

/**
 * Rebroadcasts matcher events while retaining registered listeners weakly.
 * The proxy also registers itself weakly with its source editor.
 */
internal class WeakReferenceMatcherEditor<E>(
    private val source: MatcherEditor<E>,
) : MatcherEditor<E>, MatcherEditor.Listener<E> {
    private val listenerList = mutableListOf<WeakMatcherEditorListener>()

    init {
        source.addMatcherEditorListener(WeakMatcherEditorListener(source, this))
    }

    override val matcher: Matcher<E>
        get() = source.matcher

    @Synchronized
    override fun addMatcherEditorListener(listener: MatcherEditor.Listener<E>) {
        listenerList += WeakMatcherEditorListener(this, listener)
    }

    @Synchronized
    override fun removeMatcherEditorListener(listener: MatcherEditor.Listener<E>) {
        val iterator = listenerList.iterator()
        while (iterator.hasNext()) {
            val currentListener = iterator.next()
            if (currentListener === listener || currentListener.decoratedListener === listener) {
                iterator.remove()
            }
        }
    }

    @Synchronized
    override fun changedMatcher(matcherEvent: MatcherEditor.Event<E>) {
        for (index in listenerList.lastIndex downTo 0) {
            listenerList[index].changedMatcher(matcherEvent)
        }
    }

    private inner class WeakMatcherEditorListener(
        private val editor: MatcherEditor<E>,
        listener: MatcherEditor.Listener<E>,
    ) : MatcherEditor.Listener<E> {
        private val weakListener = WeakReference(listener)

        val decoratedListener: MatcherEditor.Listener<E>?
            get() = weakListener.get()

        override fun changedMatcher(matcherEvent: MatcherEditor.Event<E>) {
            val listener = decoratedListener
            if (listener == null) {
                editor.removeMatcherEditorListener(this)
                return
            }

            listener.changedMatcher(
                MatcherEditor.Event(
                    this@WeakReferenceMatcherEditor,
                    matcherEvent.type,
                    matcherEvent.matcher,
                ),
            )
        }
    }
}
