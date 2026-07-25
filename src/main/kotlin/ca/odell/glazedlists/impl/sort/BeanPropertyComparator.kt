/* Glazed Lists                                                 (c) 2003-2006 */
/* http://publicobject.com/glazedlists/                      publicobject.com,*/
/*                                                     O'Dell Engineering Ltd.*/
package ca.odell.glazedlists.impl.sort

import ca.odell.glazedlists.impl.beans.BeanProperty

/** Compares beans by a named property. */
internal class BeanPropertyComparator<T>(
    className: Class<T>,
    property: String,
    propertyComparator: Comparator<*>,
) : Comparator<T?> {
    @Suppress("UNCHECKED_CAST")
    private val propertyComparator = propertyComparator as Comparator<Any?>
    private val beanProperty = BeanProperty(className, property, readable = true, writable = false)

    override fun compare(alpha: T?, beta: T?): Int = propertyComparator.compare(
        alpha?.let { beanProperty[it] },
        beta?.let { beanProperty[it] },
    )

    override fun equals(other: Any?): Boolean =
        this === other ||
                other is BeanPropertyComparator<*> &&
                beanProperty == other.beanProperty &&
                propertyComparator == other.propertyComparator

    override fun hashCode(): Int = 29 * propertyComparator.hashCode() + beanProperty.hashCode()
}
