package mediathek.daten;

//FIXME convert indices to int!!
public class FilmResolution {
    public enum Enum {
        LOW,
        NORMAL,
        HIGH_QUALITY;

        @Override
        public String toString() {
            return switch (super.toString()) {
                case "LOW" -> FilmResolution.LOW;
                case "NORMAL" -> FilmResolution.NORMAL;
                case "HIGH_QUALITY" -> FilmResolution.HIGH_QUALITY;
                default -> throw new IndexOutOfBoundsException("Illegal enum to string!");
            };
        }

        public static Enum fromLegacyString(String in) {
            return switch (in) {
                case FilmResolution.LOW -> LOW;
                case FilmResolution.NORMAL -> NORMAL;
                case FilmResolution.HIGH_QUALITY -> HIGH_QUALITY;
                default -> throw new IndexOutOfBoundsException("Illegal string to enum op!");
            };
        }
    }

    public static final String NORMAL = "normal";
    public static final String HIGH_QUALITY = "hd";
    public static final String LOW = "klein";
}
