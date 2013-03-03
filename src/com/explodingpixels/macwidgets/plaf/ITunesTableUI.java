package com.explodingpixels.macwidgets.plaf;

import java.awt.Color;
import java.awt.Component;
import java.awt.Container;
import java.awt.Dimension;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.WindowEvent;
import java.awt.event.WindowFocusListener;

import javax.swing.BorderFactory;
import javax.swing.CellRendererPane;
import javax.swing.DefaultCellEditor;
import javax.swing.JComponent;
import javax.swing.JTable;
import javax.swing.JTextField;
import javax.swing.border.Border;
import javax.swing.plaf.basic.BasicTableUI;
import javax.swing.table.TableCellEditor;

import com.explodingpixels.data.Rating;
import com.explodingpixels.macwidgets.ITunesRatingTableCellRenderer;
import com.explodingpixels.macwidgets.ITunesTableHeaderRenderer;
import com.explodingpixels.macwidgets.MacFontUtils;
import com.explodingpixels.widgets.TableHeaderUtils;
import com.explodingpixels.widgets.TableUtils;
import com.explodingpixels.widgets.WindowUtils;

/**
 * A UI delegate that renders an iTunes style table. <br/>
 * <img src="../../../../../graphics/iTunesTable.png">
 * <p/>
 * Sorting indicators will be rendered if a
 * {@link com.explodingpixels.widgets.TableUtils.SortDelegate} is installed on
 * the associated {@link JTable} via the
 * {@link TableUtils#makeSortable(javax.swing.JTable, com.explodingpixels.widgets.TableUtils.SortDelegate)}
 * method.
 */
public class ITunesTableUI extends BasicTableUI {

	protected static final Color EVEN_ROW_COLOR = new Color(241, 245, 250);
	protected static final Color TABLE_GRID_COLOR = new Color(0xd9d9d9);
	protected static final Color SELECTION_ACTIVE_SELECTION_FOREGROUND_COLOR = Color.WHITE;
	protected static final Color SELECTION_ACTIVE_SELECTION_BACKGROUND_COLOR = new Color(
			0x3d80df);
	protected static final Color SELECTION_INACTIVE_SELECTION_FOREGROUND_COLOR = Color.BLACK;
	protected static final Color SELECTION_INACTIVE_SELECTION_BACKGROUND_COLOR = new Color(
			0xc0c0c0);
	protected static final Color SELECTION_ACTIVE_BOTTOM_BORDER_COLOR = new Color(
			125, 170, 234);
	protected static final Color SELECTION_INACTIVE_BOTTOM_BORDER_COLOR = new Color(
			224, 224, 224);
	protected static final Color TRANSPARENT_COLOR = new Color(0, 0, 0, 0);

	@Override
	public void installUI(JComponent c) {
		super.installUI(c);

		table.remove(rendererPane);
		rendererPane = createCustomCellRendererPane();
		table.add(rendererPane);

		// TODO save defaults.

		table.setOpaque(false);
		table.setFont(MacFontUtils.ITUNES_FONT);
		table.setGridColor(TABLE_GRID_COLOR);
		table.setIntercellSpacing(new Dimension(0, 0));
		table.setAutoResizeMode(JTable.AUTO_RESIZE_OFF);

		table.getTableHeader().setDefaultRenderer(
				new ITunesTableHeaderRenderer(table));

		table.setShowHorizontalLines(false);
		TableHeaderUtils.makeHeaderFillEmptySpace(table);
		TableUtils.makeStriped(table, EVEN_ROW_COLOR);

		table.setDefaultRenderer(Rating.class,
				new ITunesRatingTableCellRenderer());
		table.setDefaultEditor(Object.class, createDefaultTableCellEditor());

		makeTableActive();
		WindowUtils.installWeakWindowFocusListener(table,
				createWindowFocusListener());
	}

	@Override
	protected void installListeners() {
		super.installListeners();
	}

	private void makeTableActive() {
		table.setSelectionForeground(SELECTION_ACTIVE_SELECTION_FOREGROUND_COLOR);
		table.setSelectionBackground(SELECTION_ACTIVE_SELECTION_BACKGROUND_COLOR);
	}

	private void makeTableInactive() {
		table.setSelectionForeground(SELECTION_INACTIVE_SELECTION_FOREGROUND_COLOR);
		table.setSelectionBackground(SELECTION_INACTIVE_SELECTION_BACKGROUND_COLOR);
	}

	private TableCellEditor createDefaultTableCellEditor() {
		JTextField textField = new JTextField();
		textField.setFont(MacFontUtils.ITUNES_FONT);
		textField.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		return new DefaultCellEditor(textField);
	}

	private WindowFocusListener createWindowFocusListener() {
		return new WindowFocusListener() {
			public void windowGainedFocus(WindowEvent e) {
				makeTableActive();
			}

			public void windowLostFocus(WindowEvent e) {
				makeTableInactive();
			}
		};
	}

	public Border getRowBorder() {
		return BorderFactory.createEmptyBorder(0, 5, 0, 5);
	}

	public Border getSelectedRowBorder() {
		return BorderFactory.createCompoundBorder(
				BorderFactory.createMatteBorder(0, 0, 1, 0,
						getSelectedRowBottomHighlight()), BorderFactory
						.createEmptyBorder(1, 5, 0, 5));
	}

	private Color getSelectedRowBottomHighlight() {
		return WindowUtils.isParentWindowFocused(table) ? SELECTION_ACTIVE_BOTTOM_BORDER_COLOR
				: SELECTION_INACTIVE_BOTTOM_BORDER_COLOR;
	}

	/**
	 * Creates a custom {@link CellRendererPane} that sets the renderer
	 * component to be non-opqaque if the associated row isn't selected. This
	 * custom {@code CellRendererPane} is needed because a table UI delegate has
	 * no prepare renderer like {@link JTable} has.
	 */
	protected CellRendererPane createCustomCellRendererPane() {
		return new CellRendererPane() {
			@Override
			public void paintComponent(Graphics graphics, Component component,
					Container container, int x, int y, int w, int h,
					boolean shouldValidate) {

				int rowAtPoint = table.rowAtPoint(new Point(x, y));
				boolean isSelected = table.isRowSelected(rowAtPoint);
				if (component instanceof JComponent) {
					JComponent jComponent = (JComponent) component;
					jComponent.setOpaque(isSelected);
					jComponent.setBorder(isSelected ? getSelectedRowBorder()
							: getRowBorder());
					jComponent.setBackground(isSelected ? table
							.getSelectionBackground() : TRANSPARENT_COLOR);
				}
				super.paintComponent(graphics, component, container, x, y, w,
						h, shouldValidate);
			}
		};
	}
}
