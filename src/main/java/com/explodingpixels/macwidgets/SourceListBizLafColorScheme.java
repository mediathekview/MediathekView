package com.explodingpixels.macwidgets;

import java.awt.Color;
import java.awt.Component;

import com.explodingpixels.painter.GradientWithBorderPainter;
import com.explodingpixels.painter.MacWidgetsPainter;

public class SourceListBizLafColorScheme extends SourceListStandardColorScheme {

	private static final MacWidgetsPainter<Component> ACTIVE_FOCUSED_SELECTION_PAINTER = createSourceListActiveFocusedSelectionPainter();

	public static final Color ACTIVE_BACKGROUND_COLOR = Color.decode("#EEF3FA");
	public static final Color INACTIVE_BACKGROUND_COLOR = Color
			.decode("#EEF3FA");

	@Override
	public MacWidgetsPainter<Component> getActiveFocusedSelectedItemPainter() {
		return ACTIVE_FOCUSED_SELECTION_PAINTER;
	}

	@Override
	public Color getActiveBackgroundColor() {
		return ACTIVE_BACKGROUND_COLOR;
	}

	@Override
	public Color getInactiveBackgroundColor() {
		return INACTIVE_BACKGROUND_COLOR;
	}

	private static MacWidgetsPainter<Component> createSourceListActiveFocusedSelectionPainter() {
		Color topLineColor = new Color(58, 93, 137);
		Color topColor = new Color(106, 144, 182);
		Color bottomColor = new Color(77, 111, 148);
		return new GradientWithBorderPainter(topLineColor, bottomColor,
				topColor, bottomColor);
	}
}
