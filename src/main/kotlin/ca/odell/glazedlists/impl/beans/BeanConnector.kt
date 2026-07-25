package ca.odell.glazedlists.impl.beans

import ca.odell.glazedlists.ObservableElementChangeHandler
import ca.odell.glazedlists.ObservableElementList
import ca.odell.glazedlists.matchers.Matcher
import ca.odell.glazedlists.matchers.Matchers
import java.beans.PropertyChangeEvent
import java.beans.PropertyChangeListener
import java.lang.reflect.InvocationTargetException
import java.lang.reflect.Method
import java.util.*

internal open class BeanConnector<E> : ObservableElementList.Connector<E> {
    private val addListenerMethod: Method
    private val removeListenerMethod: Method
    private var list: ObservableElementChangeHandler<out E>? = null
    private var matcher: Matcher<PropertyChangeEvent> = Matchers.trueMatcher()

    protected val propertyChangeListener: PropertyChangeListener = createPropertyChangeListener()

    constructor(beanClass: Class<E>) {
        var addMethod: Method? = null
        var removeMethod: Method? = null
        for (method in beanClass.methods) {
            if (method.parameterCount != 1 || method.parameterTypes[0] != PropertyChangeListener::class.java) continue
            if (method.name.startsWith("add")) addMethod = method
            if (method.name.startsWith("remove")) removeMethod = method
        }

        addListenerMethod = requireNotNull(addMethod) {
            "Couldn't find listener methods for ${beanClass.name}"
        }
        removeListenerMethod = requireNotNull(removeMethod) {
            "Couldn't find listener methods for ${beanClass.name}"
        }
    }

    constructor(beanClass: Class<E>, eventMatcher: Matcher<PropertyChangeEvent>) : this(beanClass) {
        matcher = eventMatcher
    }

    constructor(beanClass: Class<E>, addListenerMethodName: String, removeListenerMethodName: String) {
        try {
            addListenerMethod = beanClass.getMethod(addListenerMethodName, PropertyChangeListener::class.java)
            removeListenerMethod = beanClass.getMethod(removeListenerMethodName, PropertyChangeListener::class.java)
        } catch (exception: NoSuchMethodException) {
            throw IllegalArgumentException("Failed to find method ${exception.message} in $beanClass", exception)
        }
    }

    constructor(
        beanClass: Class<E>,
        addListenerMethodName: String,
        removeListenerMethodName: String,
        eventMatcher: Matcher<PropertyChangeEvent>,
    ) : this(beanClass, addListenerMethodName, removeListenerMethodName) {
        matcher = eventMatcher
    }

    override fun installListener(element: E): EventListener {
        invokeListenerMethod(addListenerMethod, element)
        return propertyChangeListener
    }

    override fun uninstallListener(element: E, listener: EventListener) {
        invokeListenerMethod(removeListenerMethod, element)
    }

    override fun setObservableElementList(list: ObservableElementChangeHandler<out E>?) {
        this.list = list
    }

    val eventMatcher: Matcher<PropertyChangeEvent>
        get() = matcher

    protected open fun createPropertyChangeListener(): PropertyChangeListener = PropertyChangeHandler()

    inner class PropertyChangeHandler : PropertyChangeListener {
        override fun propertyChange(event: PropertyChangeEvent) {
            val handler = list
            if (handler != null && eventMatcher.matches(event)) {
                handler.elementChanged(event.source)
            }
        }
    }

    private fun invokeListenerMethod(method: Method, element: E) {
        try {
            method.invoke(element, propertyChangeListener)
        } catch (exception: IllegalAccessException) {
            throw RuntimeException(exception)
        } catch (exception: InvocationTargetException) {
            throw RuntimeException(exception.cause)
        }
    }
}
