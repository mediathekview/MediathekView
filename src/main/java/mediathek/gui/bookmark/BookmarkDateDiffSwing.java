package mediathek.gui.bookmark;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.temporal.ChronoUnit;

/**
 * Date comparator class, used in bookmark table
 * specific for string format dd.mm.yyyy
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkDateDiffSwing {

  private  LocalDate today;
  private static BookmarkDateDiffSwing bookmarkDateComparator;
  DateTimeFormatter dateformatter;

  private BookmarkDateDiffSwing() {
    today = LocalDate.now(); 
    dateformatter = DateTimeFormatter.ofPattern("dd.MM.yyyy");
  }
  
  public long diff2Today(String str) {
    if (today != null && str != null) {
      LocalDate ld = LocalDate.parse(str, dateformatter);
      return ChronoUnit.DAYS.between(today,ld);
    }
    return 0;
  }

  public void setToday(LocalDate d) { 
    today = d;
  }
  
  public static synchronized BookmarkDateDiffSwing getInstance( ) {
      if (bookmarkDateComparator == null) {
          bookmarkDateComparator = new BookmarkDateDiffSwing();
      }
      return bookmarkDateComparator;
  }
  

}

