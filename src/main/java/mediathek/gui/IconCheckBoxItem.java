package mediathek.gui;

import javax.swing.*;
import java.awt.*;

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
    JPanel panel = new JPanel(new FlowLayout(FlowLayout.LEFT, 5, 0));
    panel.setOpaque(false);
    panel.add(checkBox);
    panel.add(imageLabel);
    setLayout(new BorderLayout());
    add(panel, BorderLayout.CENTER);
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

  /**
   * Sets the icon displayed in the label.
   *
   * @param icon the {@link Icon} to display
   */
  @Override
  public void setIcon(Icon icon) {
    imageLabel.setIcon(icon);
  }

  /**
   * Returns the icon currently displayed in the label.
   *
   * @return the currently displayed {@link Icon}, or {@code null} if none
   */
  @Override
  public Icon getIcon() {
    return imageLabel.getIcon();
  }

  /**
   * Sets the text displayed in the label.
   *
   * @param text the text to display
   */
  @Override
  public void setText(String text) {
    imageLabel.setText(text);
  }

  /**
   * Returns the text currently displayed in the label.
   *
   * @return the label text, or {@code null} if none
   */
  @Override
  public String getText() {
    return imageLabel.getText();
  }
}
