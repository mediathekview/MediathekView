package mediathek.daten.bookmark;

import java.util.List;

public class ListeBookmark {
  public List<DatenBookmark> bookmarks;

  public ListeBookmark() {
  }

  public ListeBookmark(List<DatenBookmark> bookmarks) {
    this.bookmarks = bookmarks;
  }

  public List<DatenBookmark> getBookmarks() {
    return bookmarks;
  }

  public void setBookmarks(List<DatenBookmark> bookmarks) {
    this.bookmarks = bookmarks;
  }
}

