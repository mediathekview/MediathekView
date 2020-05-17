package mediathek.javafx.bookmark;

import java.util.Comparator;
import java.util.function.BiFunction;

/**
 * Date comparator class, used in bookmark table
 * specific for string format dd.mm.yyyy
 *
 * @author Klaus Wich <klaus.wich@aim.com>
 */
public class BookmarkDateComparator implements Comparator<String> {

  @Override
  public int compare(String str1, String str2) {
    BiFunction<Integer, Integer, Integer> cmprange = (i1, i2) -> {
      for ( int i = i1; i < i2; i++) {
        int s1 = (int)str1.charAt(i);
        int s2 = (int)str2.charAt(i);
        if (s1 != s2) {
          return s1 - s2;
        }
      }
      return 0;
    };

    int s1 = str1 != null ? str1.length() : -1;
    int s2 = str2 != null ? str2.length() : -1;
    int result = s1 - s2;
    if (result == 0 && s1 > 9 && s2 > 9) {
      result = cmprange.apply(6,10);
      if (result == 0) {
        result = cmprange.apply(3,5);
        if (result == 0) {
          result = cmprange.apply(0,2);
        }
      }
    }
    return result;
  }
}

