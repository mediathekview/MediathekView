package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Component;

import javax.swing.Icon;
import javax.swing.ImageIcon;

import com.explodingpixels.macwidgets.plaf.EmphasizedLabelUI;
import com.explodingpixels.painter.GradientWithBorderPainter;
import com.explodingpixels.painter.MacWidgetsPainter;

/**
 * A {@link WidgetColorScheme} that provides {@link Painter}s and colors
 *
 */
public class WidgetStandardColorScheme implements WidgetColorScheme {

    private static Color ACTIVE_BACKGROUND_COLOR = new Color(0xd6dde5);
    private static Color INACTIVE_BACKGROUND_COLOR = new Color(0xe8e8e8);

    private static final Color CATEGORY_FONT_COLOR = Color.BLACK;
    private static final Color CATEGORY_FONT_SHADOW_COLOR = EmphasizedLabelUI.DEFAULT_EMPHASIS_COLOR;
    private static final Color ITEM_FONT_COLOR = Color.BLACK;
    private static final Color SELECTED_ITEM_FONT_COLOR = Color.WHITE;
    private static final Color SELECTED_ITEM_FONT_SHADOW_COLOR = new Color(0, 0, 0, 110);

    private static Color BADGE_SELECTED_COLOR = Color.WHITE;
    private static Color BADGE_ACTIVE_UNSELECTED_COLOR = new Color(0x8899bc);
    private static Color BADGE_INACTIVE_UNSELECTED_COLOR = new Color(0x9a9a9a);
    private static Color BADGE_TEXT_COLOR = BADGE_SELECTED_COLOR;

    private static final Color ACTIVE_FOCUSED_SELECTION_TOPLINE_COLOR = new Color(0x4580c8);
    private static final Color ACTIVE_FOCUSED_SELECTION_TOP_COLOR = new Color(0x5d94d6);
    private static final Color ACTIVE_FOCUSED_SELECTION_BOTTOM_COLOR = new Color(0x1956ad);

    private static final Color ACTIVE_UNFOCUSED_SELECTION_TOPLINE_COLOR = new Color(0x91a0c0);
    private static final Color ACTIVE_UNFOCUSED_SELECTION_TOP_COLOR = new Color(0xa1b0cf);
    private static final Color ACTIVE_UNFOCUSED_SELECTION_BOTTOM_COLOR = new Color(0x7185ab);

    private static final Color INACTIVE_SELECTION_TOPLINE_COLOR = new Color(0x979797);
    private static final Color INACTIVE_SELECTION_TOP_COLOR = new Color(0xb4b4b4);
    private static final Color INACTIVE_SELECTION_BOTTOM_COLOR = new Color(0x8a8a8a);
    
    public static final Icon UNSELECTED_COLLAPSED_ICON = new ImageIcon(
            SourceList.class.getResource(
                    "/com/explodingpixels/macwidgets/images/source_list_right_arrow.png"));

    private static final Icon UNSELECTED_EXPANDED_ICON = new ImageIcon(
            SourceList.class.getResource(
                    "/com/explodingpixels/macwidgets/images/source_list_down_arrow.png"));

    private static final Icon SELECTED_COLLAPSED_ICON = new ImageIcon(
            SourceList.class.getResource(
                    "/com/explodingpixels/macwidgets/images/source_list_white_right_arrow.png"));

    private static final Icon SELECTED_EXPANDED_ICON = new ImageIcon(
            SourceList.class.getResource(
                    "/com/explodingpixels/macwidgets/images/source_list_white_down_arrow.png"));

    private static final MacWidgetsPainter<Component> ACTIVE_FOCUSED_SELECTION_PAINTER =
            createSourceListActiveFocusedSelectionPainter();

    private static final MacWidgetsPainter<Component> ACTIVE_UNFOCUSED_SELECTION_PAINTER =
            createSourceListActiveUnfocusedSelectionPainter();

    private static final MacWidgetsPainter<Component> INACTIVE_FOCUSED_SELECTION_PAINTER =
            createSourceListInactiveSelectionPainter();

    public MacWidgetsPainter<Component> getActiveFocusedSelectedItemPainter() {
        return ACTIVE_FOCUSED_SELECTION_PAINTER;
    }

    public MacWidgetsPainter<Component> getActiveUnfocusedSelectedItemPainter() {
        return ACTIVE_UNFOCUSED_SELECTION_PAINTER;
    }

    public MacWidgetsPainter<Component> getInactiveSelectedItemPainter() {
        return INACTIVE_FOCUSED_SELECTION_PAINTER;
    }

    public Color getCategoryTextColor() {
        return CATEGORY_FONT_COLOR;
    }

    public Color getCategoryTextShadowColor() {
        return CATEGORY_FONT_SHADOW_COLOR;
    }

    public Color getUnselectedItemTextColor() {
        return ITEM_FONT_COLOR;
    }

    public Color getSelectedItemTextColor() {
        return SELECTED_ITEM_FONT_COLOR;
    }

    public Color getSelectedItemFontShadowColor() {
        return SELECTED_ITEM_FONT_SHADOW_COLOR;
    }

    public Color getActiveBackgroundColor() {
        return ACTIVE_BACKGROUND_COLOR;
    }

    public Color getInactiveBackgroundColor() {
        return INACTIVE_BACKGROUND_COLOR;
    }

    public Icon getUnselectedCollapsedIcon() {
        return UNSELECTED_COLLAPSED_ICON;
    }

    public Icon getUnselectedExpandedIcon() {
        return UNSELECTED_EXPANDED_ICON;
    }

    public Icon getSelectedCollapsedIcon() {
        return SELECTED_COLLAPSED_ICON;
    }

    public Icon getSelectedExpandedIcon() {
        return SELECTED_EXPANDED_ICON;
    }

    public Color getBadgeTextColor() {
        return BADGE_TEXT_COLOR;
    }

    public Color getSelectedBadgeColor() {
        return BADGE_SELECTED_COLOR;
    }

    public Color getActiveUnselectedBadgeColor() {
        return BADGE_ACTIVE_UNSELECTED_COLOR;
    }

    public Color getInativeUnselectedBadgeColor() {
        return BADGE_INACTIVE_UNSELECTED_COLOR;
    }

    private static MacWidgetsPainter<Component> createSourceListActiveFocusedSelectionPainter() {
        Color topLineColor = new Color(0x4580c8);
        Color topColor = new Color(0x5d94d6);
        Color bottomColor = new Color(0x1956ad);
        return new GradientWithBorderPainter(topLineColor, bottomColor, topColor, bottomColor);
    }

    private static MacWidgetsPainter<Component> createSourceListActiveUnfocusedSelectionPainter() {
        Color topLineColor = new Color(0x91a0c0);
        Color topColor = new Color(0xa1b0cf);
        Color bottomColor = new Color(0x7185ab);
        return new GradientWithBorderPainter(topLineColor, bottomColor, topColor, bottomColor);
    }

    private static MacWidgetsPainter<Component> createSourceListInactiveSelectionPainter() {
        Color topLineColor = new Color(0x979797);
        Color topColor = new Color(0xb4b4b4);
        Color bottomColor = new Color(0x8a8a8a);
        return new GradientWithBorderPainter(topLineColor, bottomColor, topColor, bottomColor);
    }

	@Override
	public Color getActiveFocusedSelectedItemBackgroundColor() {
		return ACTIVE_FOCUSED_SELECTION_TOP_COLOR;
	}

	@Override
	public Color getActiveUnfocusedSelectedItemBackgroundColor() {
		return ACTIVE_UNFOCUSED_SELECTION_TOP_COLOR;
	}

	@Override
	public Color getInactiveSelectedItemBackgroundColor() {
		return INACTIVE_SELECTION_TOP_COLOR;
	}
}
