package mediathek.gui;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import java.beans.ConstructorProperties;
import javax.swing.Action;
import javax.swing.DefaultButtonModel;
import javax.swing.Icon;
import javax.swing.JButton;
import mediathek.tool.SVGIconUtilities;

/*
    Created by: Markus
    Created at: 06.04.2025
*/
public class SVGButton extends JButton {

  private FlatSVGIcon icon2;


  public SVGButton() {
    this(null, null);
  }

  /**
   * Creates a button with an icon.
   *
   * @param icon  the Icon image to display on the button
   */
  public SVGButton(Icon icon) {
    this(null, icon);
  }

  /**
   * Creates a button with text.
   *
   * @param text  the text of the button
   */
  @ConstructorProperties({"text"})
  public SVGButton(String text) {
    this(text, null);
  }

  /**
   * Creates a button where properties are taken from the
   * <code>Action</code> supplied.
   *
   * @param a the <code>Action</code> used to specify the new button
   *
   * @since 1.3
   */
  public SVGButton(Action a) {
    this();
    setAction(a);
  }

  /**
   * Creates a button with initial text and an icon.
   *
   * @param text  the text of the button
   * @param icon  the Icon image to display on the button
   */
  public SVGButton(String text, Icon icon) {
    // Create the model
    setModel(new DefaultButtonModel());

    // initialize
    init(text, icon);
  }


  public void setIcon2(String ressource) {
    this.icon2 = SVGIconUtilities.createSVGIcon(ressource);
  super.setIcon(SVGIconUtilities.createSVGIcon(ressource));
  }


}
