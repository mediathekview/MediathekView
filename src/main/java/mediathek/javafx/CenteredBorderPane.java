package mediathek.javafx;

import javafx.scene.Node;
import javafx.scene.layout.BorderPane;

/**
 * A BorderPane subclass which automatically centers nodes
 */
public class CenteredBorderPane extends BorderPane {
    public CenteredBorderPane(Node node) {
        super();
        setCenter(node);
        setCenterShape(true);
    }
}
