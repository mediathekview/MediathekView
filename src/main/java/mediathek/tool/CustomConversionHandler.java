package mediathek.tool;

import org.apache.commons.configuration2.convert.DefaultConversionHandler;
import org.apache.commons.configuration2.interpol.ConfigurationInterpolator;

import java.util.UUID;

public class CustomConversionHandler extends DefaultConversionHandler {



  @Override
  @SuppressWarnings("unchecked")
  protected <T> T convertValue(Object src, Class<T> targetCls, ConfigurationInterpolator ci) {
    if (src == null) {
      return null;
    } else if (UUID.class.equals(targetCls)) {
      String uuidAsString = super.convertValue(src, String.class, ci);
      return (T) UUID.fromString(uuidAsString);
    } else {
      return super.convertValue(src, targetCls, ci);
    }
  }
}
