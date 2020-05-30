/*
 * To change this license header, choose License Headers in Project Properties.
 * To change this template file, choose Tools | Templates
 * and open the template in the editor.
 */
package mediathek.javafx.bookmark;

import com.fasterxml.jackson.annotation.JsonIgnore;
import com.fasterxml.jackson.annotation.JsonIgnoreProperties;
import javafx.beans.property.SimpleStringProperty;
import javafx.beans.property.StringProperty;
import javafx.scene.paint.Color;

/**
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
@JsonIgnoreProperties(ignoreUnknown=true)
public class BookmarkCategory {
  private final StringProperty name;
  private Color color;
  private Color backgroundColor;
  
  BookmarkCategory() {
    this("");
  }
  
   BookmarkCategory(String name) {
    this(name, Color.BLACK, Color.TRANSPARENT);
  }
    
  BookmarkCategory(String name, Color color, Color backgroundColor) {
    this.name = new SimpleStringProperty(name);
    this.color = color;
    this.backgroundColor = backgroundColor;
  }
  
  public String getName() {
    return name.getValue();
  }
  
  public void setName(String name) {
    this.name.setValue(name);
  }
  
  @JsonIgnore
  public StringProperty getNameProperty() {
    return name;
  }
  
  public String getColorValue() {
    return color.toString();
  }
  
  public void setColorValue(String value) {
    this.color = Color.valueOf(value);
  }
  
  public String getBackgroundColorValue() {
    return backgroundColor.toString();
  }
  
  public void setBackgroundColorValue(String value) {
    this.backgroundColor = Color.valueOf(value);
  }

  @JsonIgnore
  public Color getColor() {
    return color;
  }
  
  @JsonIgnore
  public void setColor(Color color) {
    this.color = color;
  }
  
  @JsonIgnore
  public Color getBackgroundColor() {
    return backgroundColor;
  }
  
  @JsonIgnore
  public void setBackgroundColor(Color color) {
    this.backgroundColor = color;
  }
}
