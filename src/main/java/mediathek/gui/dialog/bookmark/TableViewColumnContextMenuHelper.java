/*
 * Copyright (c) 2025 derreisende77.
 * This code was developed as part of the MediathekView project https://github.com/mediathekview/MediathekView
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 */

package mediathek.javafx.bookmark;

import javafx.event.Event;
import javafx.geometry.Side;
import javafx.scene.Node;
import javafx.scene.control.*;
import javafx.scene.control.skin.TableHeaderRow;
import javafx.scene.control.skin.TableViewSkin;

import java.util.Optional;

class TableViewColumnContextMenuHelper {

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
        // replace mouse listener on "+" node
        getMenuButton().ifPresent(node -> node.setOnMousePressed(event -> {
            showContextMenu();
            event.consume();
        }));
    }

    private Optional<Node> getMenuButton() {
        return getTableHeaderRow().flatMap(headerRow -> headerRow.getChildren().stream()
                .filter(child -> child.getStyleClass().contains("show-hide-columns-button"))
                .findAny());
    }

    private Optional<TableHeaderRow> getTableHeaderRow() {
        final TableViewSkin<?> tableSkin = (TableViewSkin<?>) tableView.getSkin();
        if (tableSkin == null) {
            return Optional.empty();
        }
        // find the TableHeaderRow child
        return tableSkin.getChildren().stream()
                .filter(child -> child instanceof TableHeaderRow)
                .map(node -> (TableHeaderRow) node)
                .findAny();
    }

    protected void showContextMenu() {
        // When the menu is already shown clicking the + button hides it.
        if (tableContextMenu != null) {
            tableContextMenu.hide();
        } else {
            getMenuButton().ifPresent(buttonNode -> {
                // Show the menu
                // rebuilds the menu each time it is opened
                tableContextMenu = createContextMenu();
                tableContextMenu.setOnHidden(event -> tableContextMenu = null);
                tableContextMenu.show(buttonNode, Side.BOTTOM, 0, 0);
                // Repositioning the menu to be aligned by its right side (keeping
                // inside the table view)
                tableContextMenu.setX(
                        buttonNode.localToScreen(buttonNode.getBoundsInLocal())
                                .getMaxX() - tableContextMenu.getWidth());
            });
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