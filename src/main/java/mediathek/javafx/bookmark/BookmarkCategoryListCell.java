package mediathek.javafx.bookmark;

import javafx.geometry.Insets;
import javafx.geometry.Pos;
import javafx.scene.control.ListCell;
import javafx.scene.layout.Background;
import javafx.scene.layout.BackgroundFill;
import javafx.scene.layout.CornerRadii;
import javafx.scene.paint.Color;

/**
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkCategoryListCell extends ListCell<BookmarkCategory> {
  
  private boolean useSelected = true;
  
  public BookmarkCategoryListCell(boolean useSelected) {
    super();
    this.useSelected = useSelected;
  }
   
  @Override
  public void updateItem(BookmarkCategory category, boolean empty) {
     super.updateItem(category, empty);
     if (!empty && (category != null)) {
        setText(category.getName());
        setTextFill(category.getColor());
        setBackground(new Background(new BackgroundFill(useSelected && this.isSelected() ? Color.DARKBLUE : category.getBackgroundColor(),
                                                        new CornerRadii(8.0), new Insets(2,10,2,10))));
        this.setAlignment(Pos.CENTER);
     }
     else {
        setText(null);
        setBackground(Background.EMPTY);
     }
  }

  public void ignoreSelected() {
    useSelected = false;
  }
}