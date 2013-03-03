package com.explodingpixels.widgets;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;
import java.beans.PropertyChangeEvent;
import java.beans.PropertyChangeListener;

import javax.swing.JTable;
import javax.swing.JViewport;
import javax.swing.ListSelectionModel;
import javax.swing.border.AbstractBorder;
import javax.swing.border.Border;
import javax.swing.event.ListSelectionEvent;
import javax.swing.event.ListSelectionListener;
import javax.swing.table.TableColumn;

import com.explodingpixels.macwidgets.plaf.ITunesTableUI;

/**
 * Creates a border for a {@link JViewport} that draws a striped background
 * corresponding to the row positions of the given {@link JTable}.
 */
public class StripedViewportBorder extends AbstractBorder implements
		ListSelectionListener, PropertyChangeListener {

	private final JViewport fViewport;
	private final JTable fTable;
	private final Color fStripeColor;

	public StripedViewportBorder(JViewport viewport, JTable table,
			Color stripeColor) {
		fViewport = viewport;
		fTable = table;
		fStripeColor = stripeColor;
		fTable.getSelectionModel().addListSelectionListener(this);
		fTable.addPropertyChangeListener(this);
		WindowUtils.installWeakWindowFocusListener(table,
				createWindowFocusListener());
	}

	public StripedViewportBorder(JViewport viewport, JTable table) {
		this(viewport, table, new Color(241, 245, 250));
	}

	@Override
	public void paintBorder(Component c, Graphics g, int x, int y, int width,
			int height) {
		paintStripedBackground(g, y);
		paintVerticalGridLines(g, y, height);
	}

	private void paintStripedBackground(Graphics g, int borderY) {
		// get the row index at the top of the clip bounds (the first row
		// to paint).
		Rectangle clip = g.getClipBounds();
		Point viewPosition = fViewport.getViewPosition();
		int rowAtPoint = fTable.rowAtPoint(viewPosition);
		// get the y coordinate of the first row to paint. if there are no
		// rows in the table, start painting at the top of the supplied
		// clipping bounds.
		int topY = rowAtPoint < 0 ? borderY : fTable.getCellRect(rowAtPoint, 0,
				true).y - viewPosition.y + borderY;
		// create a counter variable to hold the current row. if there are no
		// rows in the table, start the counter at 0.
		int currentRow = rowAtPoint < 0 ? 0 : rowAtPoint;
		int rowHeight = fTable.getRowHeight();
		Border border = null;
		if (fTable.getUI() instanceof ITunesTableUI) {
			ITunesTableUI ui = (ITunesTableUI) fTable.getUI();
			border = ui.getSelectedRowBorder();
		}
		while (topY < clip.y + clip.height) {
			int bottomY = topY + rowHeight;
			g.setColor(getRowColor(currentRow));
			g.fillRect(clip.x, topY, clip.width, rowHeight);
			if (border != null && fTable.isRowSelected(currentRow)) {
				border.paintBorder(fViewport, g, 0, topY, fViewport.getWidth(),
						fTable.getRowHeight());
			}
			topY = bottomY;
			currentRow++;
		}
	}

	private Color getRowColor(int row) {
		if (fTable.isRowSelected(row)) {
			return fTable.getSelectionBackground();
		}
		return row % 2 == 0 ? fStripeColor : fTable.getBackground();
	}

	private void paintVerticalGridLines(Graphics g, int y, int height) {
		// paint the column grid dividers for the non-existent rows.
		int x = 0 - fViewport.getViewPosition().x + fViewport.getLocation().x;
		g.setColor(fTable.getGridColor());
		for (int i = 0; i < fTable.getColumnCount(); i++) {
			TableColumn column = fTable.getColumnModel().getColumn(i);
			// increase the x position by the width of the current column.
			x += column.getWidth();
			g.setColor(fTable.getGridColor());
			// draw the grid line (not sure what the -1 is for, but BasicTableUI
			// also does it.source
			g.drawLine(x - 1, y, x - 1, y + height);
		}
	}

	public void valueChanged(ListSelectionEvent e) {
		fViewport.repaint();
	}

	public void propertyChange(PropertyChangeEvent evt) {
		if (evt.getSource().equals(fTable)) {
			if (evt.getPropertyName().equals("selectionModel")) {
				ListSelectionModel oldModel = (ListSelectionModel) evt
						.getOldValue();
				ListSelectionModel newModel = (ListSelectionModel) evt
						.getNewValue();
				oldModel.removeListSelectionListener(this);
				newModel.addListSelectionListener(this);
			} else if (evt.getPropertyName().equals("selectionBackground")) {
				fViewport.repaint();
			}
		}
	}

	private WindowFocusListener createWindowFocusListener() {
		return new WindowFocusListener() {
			public void windowGainedFocus(WindowEvent e) {
				fViewport.repaint();
			}

			public void windowLostFocus(WindowEvent e) {
				fViewport.repaint();
			}
		};
	}
}
