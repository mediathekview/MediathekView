package com.explodingpixels.macwidgets;

import java.awt.Color;

import javax.swing.BorderFactory;
import javax.swing.JComponent;
import javax.swing.JSplitPane;
import javax.swing.plaf.basic.BasicSplitPaneUI;

public class WidgetFactory {

	   public static JSplitPane createHorizontalSplitPane(JComponent componentLeft, JComponent componentRight) {
	        JSplitPane splitPane = new JSplitPane(
	                JSplitPane.HORIZONTAL_SPLIT, componentLeft, componentRight);
	        splitPane.setContinuousLayout(true);
	        splitPane.setDividerSize(1);
	        ((BasicSplitPaneUI) splitPane.getUI()).getDivider().setBorder(
	                BorderFactory.createMatteBorder(0, 1, 0, 0, new Color(0xa5a5a5)));
	        splitPane.setBorder(BorderFactory.createEmptyBorder());
	        return splitPane;
	    }
	
	   public static JSplitPane createVerticalSplitPane(JComponent componentTop, JComponent componentBottom) {
	        JSplitPane splitPane = new JSplitPane(
	                JSplitPane.VERTICAL_SPLIT, componentTop, componentBottom);
	        splitPane.setContinuousLayout(true);
	        splitPane.setDividerSize(1);
	        ((BasicSplitPaneUI) splitPane.getUI()).getDivider().setBorder(
	                BorderFactory.createMatteBorder(0, 1, 0, 0, new Color(0xa5a5a5)));
	        splitPane.setBorder(BorderFactory.createEmptyBorder());
	        return splitPane;
	    }

}
