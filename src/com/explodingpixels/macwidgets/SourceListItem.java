package com.explodingpixels.macwidgets;

import java.beans.PropertyChangeListener;
import java.beans.PropertyChangeSupport;
import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import javax.swing.Icon;

import com.explodingpixels.widgets.IconProvider;
import com.explodingpixels.widgets.TextProvider;

/**
 * An item in a {@link SourceList} which is contained within a {@link SourceListCategory}.
 */
public class SourceListItem implements TextProvider, IconProvider, SourceListBadgeContentProvider {

    public static final String TEXT = "text";
    public static final String ICON = "icon";
    public static final String COUNTER_VALUE = "counter";

    private List<SourceListItem> fChildItems = new ArrayList<SourceListItem>();

    private String fText;

    private Icon fIcon;

    private int fCounterValue;

    private PropertyChangeSupport fSupport = new PropertyChangeSupport(this);

    /**
     * Creates a {@code SourceListItem} with the given text.
     *
     * @param text the item text. Cannot be null.
     * @throws IllegalArgumentException if the text is null.
     */
    public SourceListItem(String text) {
        this(text, null);
    }

    /**
     * Creates a {@code SourceListItem} with the given text and icon.
     *
     * @param text the item text. Cannot be null.
     * @param icon the item icon. Can be null.
     * @throws IllegalArgumentException if the text is null.
     */
    public SourceListItem(String text, Icon icon) {
        checkText(text);
        fText = text;
        fIcon = icon;
    }

    /**
     * Gets the text to use for this item.
     *
     * @return the text to use for this item.
     */
    public String getText() {
        return fText;
    }

    /**
     * Sets the text to use for this item.
     *
     * @param text the text to use for this item. Cannot be null.
     * @throws IllegalArgumentException if the text is null.
     */
    public void setText(String text) {
        checkText(text);
        String oldText = fText;
        fText = text;
        fSupport.firePropertyChange(TEXT, oldText, fText);
    }

    /**
     * Gets the icon to use for this item.
     *
     * @return the icon to use for this item.
     */
    public Icon getIcon() {
        return fIcon;
    }

    /**
     * Sets the icon to use for this item.
     *
     * @param icon the icon to use for this item. Can be null.
     */
    public void setIcon(Icon icon) {
        Icon oldIcon = fIcon;
        fIcon = icon;
        fSupport.firePropertyChange(ICON, oldIcon, fIcon);
    }

    boolean hasCounterValue() {
        return getCounterValue() > 0;
    }

    /**
     * Gets the counter value to use for this item. The counter value will be displayed to the right
     * of the item.
     *
     * @return the counter value to use for this item.
     */
    public int getCounterValue() {
        return fCounterValue;
    }

    /**
     * Sets the counter value to use for this item. The counter value will be displayed to the right
     * of the item.
     *
     * @param counterValue the counter value to use for this item. Must be >= 0.
     * @throws IllegalArgumentException if the counter value is not >= 0.
     */
    public void setCounterValue(int counterValue) {
        checkCount(counterValue);
        int oldCounterValue = fCounterValue;
        fCounterValue = counterValue;
        fSupport.firePropertyChange(COUNTER_VALUE, oldCounterValue, fCounterValue);
    }

    /**
     * Returns {@code true} if the given {@link SourceListItem} is contained by this item, to
     * include being a sub-element of another child {@code SourceListItem}.
     *
     * @param item the {@code SourceListItem} to determine whether or not is contained by this
     *             item.
     * @return {@code true} if the given {@code SourceListItem} is contained within this item
     *         or within on of this items child {@code SourceListItem}s.
     */
    public boolean containsItem(SourceListItem item) {
        boolean contains = fChildItems.contains(item);
        if (!contains) {
            for (SourceListItem childItem : fChildItems) {
                contains = childItem.containsItem(item);
                if (contains) {
                    break;
                }
            }
        }
        return contains;
    }

    /**
     * Returns a {@link String} representation of this {@code SourceListItem}.
     *
     * @return a {@link String} representation of this {@code SourceListItem}.
     */
    @Override
    public String toString() {
        return getText();
    }

    /**
     * Gets a list of this {@code SourceListItem}'s child {@code SourceListItem}s.
     *
     * @return a list of this {@code SourceListItem}'s child {@code SourceListItem}s.
     */
    public List<SourceListItem> getChildItems() {
        return Collections.unmodifiableList(fChildItems);
    }

    int indexOfItem(SourceListItem item) {
        return fChildItems.indexOf(item);
    }

    int getChildCount() {
        return fChildItems.size();
    }

    void addItem(SourceListItem childItem) {
        fChildItems.add(childItem);
    }

    void addItem(int index, SourceListItem childItem) {
        fChildItems.add(index, childItem);
    }

    void removeItem(SourceListItem childItem) {
        fChildItems.remove(childItem);
    }

    SourceListItem removeItem(int index) {
        return fChildItems.remove(index);
    }

    // Property change support. ///////////////////////////////////////////////////////////////////

    /**
     * Adds a {@link PropertyChangeListener} on this {@code SourceListItem}.
     *
     * @param listener the listener to add.
     */
    public void addPropertyChangeListener(PropertyChangeListener listener) {
        fSupport.addPropertyChangeListener(listener);
    }

    /**
     * Removes a {@link PropertyChangeListener} from this {@code SourceListItem}.
     *
     * @param listener the listener to remove.
     */
    public void removePropertyChangeListener(PropertyChangeListener listener) {
        fSupport.removePropertyChangeListener(listener);
    }

    // Utility methods. ///////////////////////////////////////////////////////////////////////////

    private void checkCount(int count) {
        if (count < 0) {
            throw new IllegalArgumentException("Count must be zero or greater.");
        }
    }

    private void checkText(String text) {
        if (text == null) {
            throw new IllegalArgumentException("Text cannot be null.");
        }
    }

}
