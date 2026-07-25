package ca.odell.glazedlists

import ca.odell.glazedlists.event.ListEventListener
import ca.odell.glazedlists.impl.*

/** Replaces this list's contents while preserving unchanged elements where possible. */
fun <E> EventList<E>.replaceAll(
    source: List<E>,
    updates: Boolean,
) {
    Diff.replaceAll(this, source, updates)
}

/** Replaces this list's contents using [comparator] to identify equal elements. */
fun <E> EventList<E>.replaceAll(
    source: List<E>,
    updates: Boolean,
    comparator: Comparator<E>,
) {
    Diff.replaceAll(this, source, updates, comparator)
}

/** Replaces the contents of this already-sorted list from an already-sorted [source]. */
fun <E> EventList<E>.replaceAllSorted(
    source: Collection<E>,
    updates: Boolean,
    comparator: Comparator<E>?,
) {
    GlazedListsImpl.replaceAll(this, source, updates, comparator)
}

/** Returns a live read-only view of this list. */
@Suppress("UNCHECKED_CAST")
fun <E> EventList<out E>.asReadOnly(): TransformedList<E, E> =
    ReadOnlyList(this as EventList<E>)

/** Returns a live view that maps each element through [transform]. */
fun <S, E> EventList<S>.transform(transform: (S) -> E): TransformedList<S, E> =
    SimpleFunctionList(this, transform)

/** Keeps [target] synchronized with this list until the returned listener is disposed. */
fun <E> EventList<E>.synchronizeTo(target: MutableList<E>): SyncListener<E> =
    SyncListener(this, target)

/** Installs a listener that rejects values outside [types]. */
fun <E> EventList<E>.enforceTypes(types: Set<Class<*>?>): ListEventListener<E> =
    TypeSafetyListener(this, types)

/** Returns a live map whose keys are produced by [keySelector]. */
fun <K, V> EventList<V>.synchronizeToMap(keySelector: (V) -> K): DisposableMap<K, V> =
    FunctionListMap(this, keySelector)

/** Returns a naturally ordered live multimap whose keys are produced by [keySelector]. */
fun <K, V> EventList<V>.synchronizeToMultiMap(
    keySelector: (V) -> K,
): DisposableMap<K, MutableList<V>> where K : Comparable<K> =
    synchronizeToMultiMap(GlazedLists.comparableComparator(), keySelector)

/** Returns a live multimap ordered by [keyComparator]. */
@Suppress("UNCHECKED_CAST")
fun <K, V> EventList<V>.synchronizeToMultiMap(
    keyComparator: Comparator<in K>,
    keySelector: (V) -> K,
): DisposableMap<K, MutableList<V>> =
    GroupingListMultiMap(this, keySelector, keyComparator) as DisposableMap<K, MutableList<V>>
