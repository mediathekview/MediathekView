package mediathek.javafx.tool;

import javafx.event.Event;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.skin.TableHeaderRow;
import javafx.scene.control.skin.TableViewSkin;

public class TableViewColumnContextMenuHelper {

    private final TableView<?> tableView;
    private ContextMenu tableContextMenu;

    public TableViewColumnContextMenuHelper(final TableView<?> tableView) {
        super();
        this.tableView = tableView;
        tableView.setTableMenuButtonVisible(true);

        tableView.skinProperty().addListener(event -> {
            tableView.tableMenuButtonVisibleProperty()
                    .addListener((observable, oldValue, newValue) -> {
                        if (newValue) {
                            registerListeners();
                        }
                    });
            if (tableView.isTableMenuButtonVisible()) {
                registerListeners();
            }
        });
    }

    private void registerListeners() {
        final Node buttonNode = getMenuButton();
        // replace mouse listener on "+" node
        assert buttonNode != null;
        buttonNode.setOnMousePressed(event -> {
            showContextMenu();
            event.consume();
        });
    }

    private Node getMenuButton() {
        final TableHeaderRow tableHeaderRow = getTableHeaderRow();
        if (tableHeaderRow == null) {
            return null;
        }
        // child identified as cornerRegion in TableHeaderRow.java
        return tableHeaderRow.getChildren().stream().filter(child -> child
                .getStyleClass().contains("show-hide-columns-button")).findAny()
                .get();
    }

    private TableHeaderRow getTableHeaderRow() {
        final TableViewSkin<?> tableSkin = (TableViewSkin<?>) tableView
                .getSkin();
        if (tableSkin == null) {
            return null;
        }
        // find the TableHeaderRow child
        return (TableHeaderRow) tableSkin.getChildren().stream()
                .filter(child -> child instanceof TableHeaderRow).findAny()
                .get();
    }

    protected void showContextMenu() {
        final Node buttonNode = getMenuButton();
        // When the menu is already shown clicking the + button hides it.
        if (tableContextMenu != null) {
            tableContextMenu.hide();
        } else {
            // Show the menu
            // rebuilds the menu each time it is opened
            tableContextMenu = createContextMenu();
            tableContextMenu.setOnHidden(event -> tableContextMenu = null);
            tableContextMenu.show(buttonNode, Side.BOTTOM, 0, 0);
            // Repositioning the menu to be aligned by its right side (keeping
            // inside the table view)
            assert buttonNode != null;
            tableContextMenu.setX(
                    buttonNode.localToScreen(buttonNode.getBoundsInLocal())
                            .getMaxX() - tableContextMenu.getWidth());
        }
    }

    // adds custom menu items to the context menu which allow us to control
    // the on hide property
    private ContextMenu createContextMenu() {
        final ContextMenu contextMenu = new ContextMenu();
        contextMenu.getItems().add(createSelectAllMenuItem(contextMenu));
        contextMenu.getItems().add(createDeselectAllMenuItem(contextMenu));
        contextMenu.getItems().add(new SeparatorMenuItem());
        addColumnCustomMenuItems(contextMenu);
        return contextMenu;
    }

    private CustomMenuItem createSelectAllMenuItem(
            final ContextMenu contextMenu) {
        final Label selectAllLabel = new Label("Alle auswählen");
        // adds listener to the label to change the size so the user
        // can click anywhere in the menu items area and not just on the
        // text to activate its onAction
        contextMenu.focusedProperty().addListener(event -> selectAllLabel
                .setPrefWidth(contextMenu.getWidth() * 0.75));

        final CustomMenuItem selectAllMenuItem = new CustomMenuItem(
                selectAllLabel);
        selectAllMenuItem.setOnAction(this::selectAll);
        // set to false so the context menu stays visible after click
        selectAllMenuItem.setHideOnClick(false);
        return selectAllMenuItem;
    }

    private void selectAll(final Event event) {
        tableView.getColumns().forEach(column -> column.setVisible(true));
        event.consume();
    }

    private CustomMenuItem createDeselectAllMenuItem(
            final ContextMenu contextMenu) {
        final Label deselectAllLabel = new Label("Alle abwählen");
        // adds listener to the label to change the size so the user
        // can click anywhere in the menu items area and not just on the
        // text to activate its onAction
        contextMenu.focusedProperty().addListener(event -> deselectAllLabel
                .setPrefWidth(contextMenu.getWidth() * 0.75));

        final CustomMenuItem deselectAllMenuItem = new CustomMenuItem(
                deselectAllLabel);
        deselectAllMenuItem.setOnAction(this::deselectAll);
        // set to false so the context menu stays visible after click
        deselectAllMenuItem.setHideOnClick(false);
        return deselectAllMenuItem;
    }

    private void deselectAll(final Event event) {
        tableView.getColumns().forEach(column -> column.setVisible(false));
        event.consume();
    }

    private void addColumnCustomMenuItems(final ContextMenu contextMenu) {
        // menu item for each of the available columns
        tableView.getColumns().forEach(column -> contextMenu.getItems().add(createColumnCustomMenuItem(contextMenu, column)));
    }

    protected CustomMenuItem createColumnCustomMenuItem(
            final ContextMenu contextMenu, final TableColumn<?, ?> column) {
        final CheckBox checkBox = new CheckBox(column.getText());
        // adds listener to the check box to change the size so the user
        // can click anywhere in the menu items area and not just on the
        // text to activate its onAction
        contextMenu.focusedProperty().addListener(
                event -> checkBox.setPrefWidth(contextMenu.getWidth() * 0.75));
        // the context menu item's state controls its bound column's visibility
        checkBox.selectedProperty().bindBidirectional(column.visibleProperty());

        final CustomMenuItem customMenuItem = new CustomMenuItem(checkBox);
        customMenuItem.getStyleClass().set(1, "check-menu-item");
        customMenuItem.setOnAction(event -> {
            checkBox.setSelected(!checkBox.isSelected());
            event.consume();
        });
        // set to false so the context menu stays visible after click
        customMenuItem.setHideOnClick(false);
        return customMenuItem;
    }
}