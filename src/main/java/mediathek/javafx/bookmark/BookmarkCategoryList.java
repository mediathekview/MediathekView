
package mediathek.javafx.bookmark;

import com.fasterxml.jackson.core.JsonEncoding;
import com.fasterxml.jackson.core.JsonGenerator;
import com.fasterxml.jackson.core.JsonParser;
import com.fasterxml.jackson.core.JsonToken;
import com.fasterxml.jackson.databind.MappingJsonFactory;
import java.io.IOException;
import java.nio.file.Path;
import java.util.ArrayList;
import javafx.beans.Observable;
import javafx.collections.FXCollections;
import javafx.collections.ObservableList;
import mediathek.config.Daten;
import org.apache.logging.log4j.LogManager;

/**
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */


public class BookmarkCategoryList {
  
  public static final String NOCATEGORY = "Keine Kategorie";  // Strings used for selection defaults
  public static final String WITHOUTCATEGORY ="Ohne Kategorie";
  public static final String ALLCATEGORY ="Alle Kategorien";
  
  private static BookmarkCategoryList instance;
  private final ObservableList<BookmarkCategory> olist;
  
  private BookmarkCategoryList() {
    olist = FXCollections.observableArrayList((BookmarkCategory data) -> new Observable[]{
      data.getNameProperty()
    });
  }

  /**
   * Return singleton
   * @return instance
   */
  public static BookmarkCategoryList getInstance() {
    return instance == null ? instance = new BookmarkCategoryList() : instance;
  }

  /**
   * Return bookmark category list
   * @return observable List
   */
  public ObservableList<BookmarkCategory> getObervableList() {
    return olist;
  }

  /**
   * Get size of list
   * @return number of stored movies
   */
  public int getNbOfEntries() {
    return olist.size();
  }

  /**
   * Delete list
   */
  public void clear() {
    olist.clear();
  }
  
  public BookmarkCategory getItemByName(String Name) {
    BookmarkCategory category = null;
    for (BookmarkCategory cat: olist) {
      if (cat.getName().equals(Name)) {
        category = cat;
        break;
      }
    }
    return category;
  }
  
  /**
   * Load list from backup medium
   * @param filePath: File to read from
   */
  public void loadFromFile(Path filePath) {
    try (JsonParser parser = new MappingJsonFactory().createParser(filePath.toFile())) {
      JsonToken jToken;
      while((jToken = parser.nextToken()) != null){
        if (jToken == JsonToken.START_ARRAY && parser.getCurrentName().equals("bookmarkCategories")) {
          while (parser.nextToken() != JsonToken.END_ARRAY) {
            BookmarkCategory obj = parser.readValueAs(BookmarkCategory.class);
            olist.add(obj);
          }
        }
      }
    }
    catch (Exception e)  {
      LogManager.getLogger(Daten.class).warn("Could not read categories from file {}, error {} => file ignored", filePath.toString(), e.getMessage());
    }
  }

  /**
   * Save list to backup medium
   * @param filePath: File to save to
   */
  public void saveToFile(Path filePath) {
    try (JsonGenerator jGenerator = new MappingJsonFactory().createGenerator(filePath.toFile(), JsonEncoding.UTF8).useDefaultPrettyPrinter()) {
      jGenerator.writeStartObject();
      jGenerator.writeFieldName("bookmarkCategories");
      jGenerator.writeStartArray();
      for (BookmarkCategory data : olist) {
        jGenerator.writeObject(data);
      }
      jGenerator.writeEndArray();
      jGenerator.writeEndObject();
    }
    catch (IOException e) {
      LogManager.getLogger(Daten.class).warn("Could not save categories to file {}, error {}", filePath.toString(), e.toString());
    }
  } 
  
  /**
   * Provide a string list of configuered categories 
   * @return 
   */
  public ArrayList<String> getSelectionList() {
    ArrayList<String> al = new ArrayList<>();
    olist.forEach((category) -> {
      al.add(category.getName());
    });
    return al;
  }
  
  /**
   * Find matching category
   * @param name: Category name
   * @return Category or null
   */
  public BookmarkCategory findCategory(String name) {
    BookmarkCategory category = null;
    if (name != null) {
      for (BookmarkCategory c: olist) {
        if (c.getName().equals(name)) {
          category = c;
          break;
        }
      }
    }
    return category;
  }
}
