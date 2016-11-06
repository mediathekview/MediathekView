package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Component;

import javax.swing.Icon;

import com.explodingpixels.macwidgets.SourceList;
import com.explodingpixels.painter.MacWidgetsPainter;

/**
 * Built from MacWidgets SourceListColorScheme
 * 
 * An interface to provide a set of {@link com.explodingpixels.painter.MacWidgetsPainter}s and colors to use when painting a widget.
 */
public interface WidgetColorScheme {

    /**
     * The {@link Painter} to use for drawing the widget selection when the
     * widget is in the active window and has focus.
     *
     * @return the {@code Painter} to use for painting the selection in an active window where the
     *         widget has focus.
     */
    MacWidgetsPainter<Component> getActiveFocusedSelectedItemPainter();

    /**
     * The {@link Painter} to use for drawing the widget selection when the
     * widget is in the active window and does not have focus.
     *
     * @return the {@code Painter} to use for painting the selection in an active window where the
     *         widget does not have focus.
     */
    MacWidgetsPainter<Component> getActiveUnfocusedSelectedItemPainter();

    /**
     * The {@link Painter} to use for drawing the {@link SourceList} selection when the
     * widget is in an inactive window.
     *
     * @return the {@code Painter} to use for painting the selection in an inactive window.
     */
    MacWidgetsPainter<Component> getInactiveSelectedItemPainter();

    Color getActiveFocusedSelectedItemBackgroundColor();

    Color getActiveUnfocusedSelectedItemBackgroundColor();
    
    Color getInactiveSelectedItemBackgroundColor();
    
    /**
     * The color to draw a widget category with.
     *
     * @return the color to draw a widget category text with.
     */
    Color getCategoryTextColor();

    /**
     * The color to draw a widget category's shadow with.
     *
     * @return the color to draw a widget category text's shadow with.
     */
    Color getCategoryTextShadowColor();

    /**
     * The color to draw an unselected widget item with.
     *
     * @return the color to draw an unselected widget item with.
     */
    Color getUnselectedItemTextColor();

    /**
     * The color to draw a selected widget item with.
     *
     * @return the color to draw a selected widget item with.
     */
    Color getSelectedItemTextColor();

    /**
     * The color to draw a selected widget item's shadow with.
     *
     * @return the color to draw a selected widget item's shadow with.
     */
    Color getSelectedItemFontShadowColor();

    /**
     * The background color of the widget when it is in an active window.
     *
     * @return the background color of the widget when it is in an active window.
     */
    Color getActiveBackgroundColor();

    /**
     * The background color of the widget when it is in an inactive window.
     *
     * @return the background color of the widget when it is in an inactive window.
     */
    Color getInactiveBackgroundColor();

    /**
     * The icon to use when a widget node is collapsed and unselected.
     *
     * @return the icon to use when a widget node is collapsed and unselected.
     */
    Icon getUnselectedCollapsedIcon();

    /**
     * The icon to use when a widget node is expanded and unselected.
     *
     * @return the icon to use when a widget node is expanded and unselected.
     */
    Icon getUnselectedExpandedIcon();

    /**
     * The icon to use when a widget node is collapsed and selected.
     *
     * @return the icon to use when a widget node is collapsed and selected.
     */
    Icon getSelectedCollapsedIcon();

    /**
     * The icon to use when a widget node is expanded and selected.
     *
     * @return the icon to use when a widget node is expanded and selected.
     */
    Icon getSelectedExpandedIcon();

    /**
     * The color to draw a badge's text with.
     *
     * @return the color to draw a badge's text with.
     */
    Color getBadgeTextColor();

    /**
     * The color to draw a badge's background with when it's corresponding widget item
     * is selected. This color is used regardless of the whether the parent window is active or
     * inactive.
     *
     * @return the color to draw a badge's background with when it's corresponding
     *         widget item is selected
     */
    Color getSelectedBadgeColor();

    /**
     * The color to draw a badge's background with when it's corresponding widget item
     * is unselected and the widget is in an active window.
     *
     * @return the color to draw a badge's background with when it's corresponding
     *         widget item is unselected and the widget is in an active
     *         window.
     */
    Color getActiveUnselectedBadgeColor();

    /**
     * The color to draw a badge's background with when it's corresponding widget item
     * is unselected and the widget is in an inactive window.
     *
     * @return the color to draw a badge's background with when it's corresponding
     *         widget item is unselected and the widget is in an inactive
     *         window.
     */
    Color getInativeUnselectedBadgeColor();

}
