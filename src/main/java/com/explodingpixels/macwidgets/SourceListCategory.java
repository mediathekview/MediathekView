package com.explodingpixels.macwidgets;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;

import com.explodingpixels.widgets.TextProvider;

/**
 * A category in a {@link SourceList}. {@code SourceListCategory}s are top level containers for
 * {@link SourceListItem}s. {@code SourceListCategory}s are text only, and rendered in full caps
 * (regardless of supplied text capitalization).
 */
public class SourceListCategory implements TextProvider {

    private List<SourceListItem> fItems = new ArrayList<SourceListItem>();

    private String fText;
    private final boolean fCollapsable;

    /**
     * Creates a {@code SourceListCategory} with the given text. The capitalization of the text will
     * be ignored, as categories are rendered in full caps.
     *
     * @param text the category text. Cannot be null.
     */
    public SourceListCategory(String text) {
        this(text, true);
    }

    /**
     * Creates a {@code SourceListCategory} with the given text. The capitalization of the text will
     * be ignored, as categories are rendered in full caps. If this {@code SourceListCategory} is marked as not
     * collapsable, then no disclosure icon will be shown, making the {@code SourceListCategory} always expanded.
     *
     * @param text        the {@code SourceListCategory} text. Cannot be null.
     * @param collapsable {@code true} if this {@code SourceListCategory} should be collapsable.
     */
    public SourceListCategory(String text, boolean collapsable) {
        checkText(text);
        fText = text;
        fCollapsable = collapsable;
    }

    private void checkText(String text) {
        if (text == null) {
            throw new IllegalArgumentException("Text cannot be null.");
        }
    }

    /**
     * Gets the category text. The returned will be the text that was set on the category - that is,
     * this method does not return an all caps version of the text.
     *
     * @return the category text.
     */
    public String getText() {
        return fText;
    }

    /**
     * Sets the text to use for this {@code SourceListCategory}. The capitalization of the text will
     * be ignored, as categories are rendered in full caps.
     *
     * @param text the category text.
     */
    public void setText(String text) {
        checkText(text);
        fText = text;
    }

    /**
     * {@code true} if this {@code SourceListCategory} is collapsable.
     *
     * @return {@code true} if this {@code SourceListCategory} is collapsable.
     */
    public boolean isCollapsable() {
        return fCollapsable;
    }

    /**
     * Gets the number of child {@link SourceListItem}s that are part of this category.
     *
     * @return the number of {@code SourceListItem}s that are part of this category.
     */
    public int getItemCount() {
        return fItems.size();
    }

    /**
     * Returns {@code true} if the given {@link SourceListItem} is contained by this category, to
     * include being a sub-element of another {@code SourceListItem} contained by this category.
     *
     * @param item the {@code SourceListItem} to determine whether or not is contained by this
     *             category.
     * @return {@code true} if the given {@code SourceListItem} is contained within this category
     *         or within on of this categories {@code SourceListItem}s.
     */
    public boolean containsItem(SourceListItem item) {
        boolean contains = false;
        for (SourceListItem childItem : fItems) {
            contains = childItem.equals(item) || childItem.containsItem(item);
            if (contains) {
                break;
            }
        }
        return contains;
    }

    /**
     * Returns a {@link String} representation of this {@code SourceListCategory}.
     *
     * @return a {@link String} representation of this {@code SourceListCategory}.
     */
    @Override
    public String toString() {
        return getText();
    }

    SourceListItem getItem(int index) {
        return fItems.get(index);
    }

    /**
     * Gets a list of this {@code SourceListCategory}'s {@link SourceListItem}s.
     *
     * @return a list of this {@code SourceListCategory}'s {@code SourceListItem}s.
     */
    public List<SourceListItem> getItems() {
        return Collections.unmodifiableList(fItems);
    }

    void addItem(SourceListItem item) {
        fItems.add(item);
    }

    void addItem(int index, SourceListItem item) {
        fItems.add(index, item);
    }

    void removeItem(SourceListItem item) {
        // TODO should throw error if item not in list?
        fItems.remove(item);
    }

    SourceListItem removeItem(int index) {
        // TODO should throw error if item not in list?
        return fItems.remove(index);
    }

}
