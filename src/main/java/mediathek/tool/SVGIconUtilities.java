package mediathek.tool;

import com.formdev.flatlaf.extras.FlatSVGIcon;
import org.jspecify.annotations.NonNull;

public class SVGIconUtilities {
    public static FlatSVGIcon createSVGIcon(@NonNull String resource) {
        return createSVGIcon(resource, 16f);
    }

    public static FlatSVGIcon createSVGIcon(@NonNull String resource, float height) {
        FlatSVGIcon icon = new FlatSVGIcon(resource);
        float scaleFactor = (1f / icon.getIconHeight()) * height;
        return icon.derive(scaleFactor);
    }
}
