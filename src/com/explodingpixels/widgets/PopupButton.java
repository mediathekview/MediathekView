package com.explodingpixels.widgets;

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.Point;
import java.awt.Toolkit;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.List;

import javax.swing.BorderFactory;
import javax.swing.ButtonGroup;
import javax.swing.ImageIcon;
import javax.swing.JButton;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JComponent;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.SwingConstants;
import javax.swing.SwingUtilities;
import javax.swing.UIManager;
import javax.swing.border.Border;

public class PopupButton<E> {

	private static final ImageIcon ARROWS_ICON = new ImageIcon(
			PopupButton.class
					.getResource("/com/explodingpixels/macwidgets/images/up_down_arrows_small.png"));

	private JButton fButton = new CustomJButton();

	private List<E> fPopupItemsList;

	private E fSelectedItem;

	private JPopupMenu fPopupMenu = new JPopupMenu();

	public PopupButton(E selectedItem, List<E> popupItemsList) {

		if (selectedItem == null) {
			throw new IllegalArgumentException("The selected item cannot be "
					+ "null.");
		}

		if (popupItemsList == null) {
			throw new IllegalArgumentException("The list of items to add to"
					+ "the popup menu cannot be null.");
		}

		if (!popupItemsList.contains(selectedItem)) {
			throw new IllegalArgumentException("The item to select is not in"
					+ "the given list of items.");
		}

		fSelectedItem = selectedItem;
		fPopupItemsList = popupItemsList;
		init();
	}

	private void init() {

		Font oldPopupMenuFont = fButton.getFont();
		// TODO calculate font more robustly.
		Font newPopupMenuFont = oldPopupMenuFont.deriveFont(oldPopupMenuFont
				.getSize() - 2.0f);

		ButtonGroup buttonGroup = new ButtonGroup();

		// add the given items to the popup menu.
		for (E item : fPopupItemsList) {
			// TODO should we throw IllegalArgumentException if o is null?
			JMenuItem menuItem = new JCheckBoxMenuItem(item.toString());
			menuItem.setFont(newPopupMenuFont);
			menuItem.addActionListener(createMenuItemListener(item));
			buttonGroup.add(menuItem);
			fPopupMenu.add(menuItem);
		}

		// set the selected item now that we've filled the popup menu with menu
		// items.
		setSelectedItem(fSelectedItem);

		fPopupMenu.pack();

		Font oldButtonFont = fButton.getFont();
		Font newButtonFont = oldButtonFont
				.deriveFont(oldButtonFont.getSize() - 2.0f);

		fButton.setFont(newButtonFont);
		fButton.setContentAreaFilled(false);
		fButton.setHorizontalAlignment(SwingConstants.LEFT);
		fButton.setBorder(BorderFactory.createEmptyBorder(2, 4, 2,
				ARROWS_ICON.getIconWidth() + 10));
		fButton.addActionListener(createButtonListener());

		// figure out how big the button should be. we're using the menu to help
		// us determine the width.
		Insets insets = new Insets(2, 2, 2, 2);
		Border border = UIManager.getBorder("MenuItem.border");
		if (border != null) {
			insets = border.getBorderInsets(new JMenuItem());
		}
		int width = fPopupMenu.getPreferredSize().width - insets.left
				- insets.right;
		int height = fButton.getPreferredSize().height;

		fButton.setPreferredSize(new Dimension(width, height));

	}

	public JComponent getComponent() {
		return fButton;
	}

	private void setSelectedItem(E itemToSelect) {
		fSelectedItem = itemToSelect;
		((JMenuItem) fPopupMenu.getComponent(fPopupItemsList
				.indexOf(fSelectedItem))).setSelected(true);
		fButton.setText(fSelectedItem.toString());
	}

	private ActionListener createButtonListener() {
		return new ActionListener() {
			public void actionPerformed(ActionEvent e) {

				// grab the right most location of the button.
				int buttonRightX = fButton.getWidth();

				// figure out how the height of a menu item.
				Insets insets = fPopupMenu.getInsets();
				int itemHeight_px = (fPopupMenu.getPreferredSize().height
						- insets.top - insets.bottom)
						/ fPopupItemsList.size();

				// calculate the x and y value at which to place the popup menu.
				// by default, this will place the selected menu item in the
				// popup item directly over the button.
				int x = buttonRightX - fPopupMenu.getPreferredSize().width;
				int y = fButton.getY()
						- insets.top
						- (fPopupItemsList.indexOf(fSelectedItem) * itemHeight_px);

				// do a cursory check to make sure we're not placing the popup
				// off the bottom of the screen. note that Java on Mac won't
				// let the popup show up off screen no matter where you place
				// it.
				Dimension size = Toolkit.getDefaultToolkit().getScreenSize();
				Point bottomOfMenuOnScreen = new Point(0, y
						+ fPopupMenu.getPreferredSize().height);
				SwingUtilities.convertPointToScreen(bottomOfMenuOnScreen,
						fButton);
				if (bottomOfMenuOnScreen.y > size.height) {
					y = fButton.getHeight()
							- fPopupMenu.getPreferredSize().height;
				}

				// set the selected item in the popup menu.
				fPopupMenu.setSelected(fPopupMenu.getComponent(fPopupItemsList
						.indexOf(fSelectedItem)));

				fPopupMenu.show(fButton, x, y);

				// force the correct item to be shown as selected. this is a
				// work around for Java bug 4740942, which has been fixed by
				// Sun, but not by Apple.
				int index = fPopupMenu.getSelectionModel().getSelectedIndex();
				MenuElement[] menuPath = new MenuElement[2];
				menuPath[0] = fPopupMenu;
				menuPath[1] = fPopupMenu.getSubElements()[index];
				MenuSelectionManager.defaultManager().setSelectedPath(menuPath);

			}
		};
	}

	private ActionListener createMenuItemListener(final E item) {
		return new ActionListener() {
			public void actionPerformed(ActionEvent e) {
				setSelectedItem(item);
			}
		};
	}

	// Custom JButton.
	// ////////////////////////////////////////////////////////////////////////////

	private static class CustomJButton extends JButton {

		@Override
		protected void paintComponent(Graphics g) {
			super.paintComponent(g);

			int x = getWidth() - ARROWS_ICON.getIconWidth() - 4;
			int y = getHeight() / 2 - ARROWS_ICON.getIconHeight() / 2;

			g.drawImage(ARROWS_ICON.getImage(), x, y, null);

		}

	}

}
