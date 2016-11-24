package com.explodingpixels.macwidgets;

import com.explodingpixels.widgets.WindowUtils;

import javax.swing.*;
import java.awt.*;

/**
 * A JPanel that listens for focus and changes color based on the active and inactive colors defined in the provided {@link com.explodingpixels.WidgetColorScheme}
 */
@SuppressWarnings("serial")
public class ActivePanel extends JPanel
{
	WidgetColorScheme colorScheme = new WidgetStandardColorScheme();

	public ActivePanel()
	{
		super();
		initialize();
	}

	public ActivePanel(LayoutManager layout)
	{
		super(layout);
		initialize();
	}

	public ActivePanel(boolean isDoubleBuffered)
	{
		super(isDoubleBuffered);
		initialize();
	}

	public ActivePanel(LayoutManager layout, boolean isDoubleBuffered)
	{
		super(layout, isDoubleBuffered);
		initialize();
	}
	
	public ActivePanel(WidgetColorScheme colorScheme)
	{
		super();
		this.colorScheme = colorScheme;
		initialize();
	}

	public ActivePanel(LayoutManager layout, WidgetColorScheme colorScheme)
	{
		super(layout);
		this.colorScheme = colorScheme;
		initialize();
	}

	public ActivePanel(boolean isDoubleBuffered, WidgetColorScheme colorScheme)
	{
		super(isDoubleBuffered);
		this.colorScheme = colorScheme;
		initialize();
	}

	public ActivePanel(LayoutManager layout, boolean isDoubleBuffered, WidgetColorScheme colorScheme)
	{
		super(layout, isDoubleBuffered);
		this.colorScheme = colorScheme;
		initialize();
	}

	protected void initialize()
	{
        WindowUtils.installJComponentRepainterOnWindowFocusChanged(this);
	}
	
	public void paint(Graphics g) 
	{
		boolean containedInActiveWindow = WindowUtils.isParentWindowFocused(this);

		Color color = containedInActiveWindow
		? colorScheme.getActiveBackgroundColor() : colorScheme.getInactiveBackgroundColor();

		this.setBackground(color);
		
		int w = getWidth();
		int h = getHeight();

		Graphics2D g2 = (Graphics2D)g;
		g2.setColor(color);
		g2.fillRect(0, 0, w, h);

		paintChildren(g2);
	}	
}
