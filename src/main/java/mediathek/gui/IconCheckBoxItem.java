package mediathek.gui;

import java.awt.event.ActionListener;
import javax.swing.*;
import java.awt.*;
import javax.swing.table.TableColumn;
import javax.swing.table.TableColumnModel;
import mediathek.gui.dialog.bookmark.BookmarkModel;
import mediathek.tool.ApplicationConfiguration;

/**
 * A custom menu item that combines a {@link JCheckBox} with an {@link Icon} or text label.
 * This component extends {@link JMenuItem} but visually includes a checkbox and an image label.
 *
 * Useful for menus that require both selection state and a visual label or icon.
 */
public class IconCheckBoxItem extends JMenuItem {

  /** The checkbox component representing the selection state. */
  private JCheckBox checkBox;

  /** The label component displaying either text or an icon. */
  private JLabel imageLabel;

  public IconCheckBoxItem() {
    super();
    initComponents();
    buildUI();
  }

  /**
   * Constructs an {@code IconCheckBoxItem} with the given icon.
   *
   * @param icon the icon to display next to the checkbox
   */
  public IconCheckBoxItem(Icon icon) {
    super();
    initComponents();
    imageLabel.setIcon(icon);
    buildUI();
  }

  /**
   * Constructs an {@code IconCheckBoxItem} with the given text.
   *
   * @param text the text to display next to the checkbox
   */
  public IconCheckBoxItem(String text) {
    super();
    initComponents();
    imageLabel.setText(text);
    buildUI();
  }

  /**
   * Initializes internal components.
   */
  private void initComponents() {
    checkBox = new JCheckBox();
    imageLabel = new JLabel();
  }

  /**
   * Builds the visual layout by combining the checkbox and image label.
   */
  private void buildUI() {
    checkBox.setOpaque(false);
    imageLabel.setOpaque(false);
    imageLabel.setBorder(BorderFactory.createEmptyBorder(0, 5, 0, 0));

    JPanel panel = new JPanel();
    panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
    panel.setOpaque(false);
    panel.add(checkBox);
    panel.add(imageLabel);
    panel.setBorder(BorderFactory.createEmptyBorder(2, 5, 2, 5));
    setLayout(new BorderLayout());
    add(panel, BorderLayout.CENTER);
    setPreferredSize(new Dimension(180, 24)); // Optional
  }


  /**
   * Returns whether the checkbox is selected.
   *
   * @return {@code true} if the checkbox is selected, {@code false} otherwise
   */
  @Override
  public boolean isSelected() {
    return checkBox.isSelected();
  }

  /**
   * Sets the selected state of the checkbox.
   *
   * @param selected {@code true} to select the checkbox, {@code false} to deselect
   */
  @Override
  public void setSelected(boolean selected) {
    checkBox.setSelected(selected);
  }

  /**
   * Returns the internal checkbox component.
   *
   * @return the {@link JCheckBox} used in this menu item
   */
  public JCheckBox getCheckBox() {
    return checkBox;
  }

  /**
   * Returns the internal image or text label component.
   *
   * @return the {@link JLabel} used in this menu item
   */
  public JLabel getImageLabel() {
    return imageLabel;
  }

  @Override
  public void addActionListener(ActionListener l){
    checkBox.addActionListener(l);
  }

}
