
package mediathek.javafx.bookmark;

import mediathek.daten.DatenFilm;

/**
 * Data container used for add lists 
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkCollectionData {
  public DatenFilm movie;
  public String category;

  public BookmarkCollectionData(DatenFilm movie, String category) {
    this.movie =  movie;
    this.category = category;
  }
}
