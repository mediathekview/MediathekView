package mediathek.tool;

public enum OperatingSystemType {

    UNKNOWN(""), WIN32("Windows"), WIN64("Windows"), LINUX("Linux"), MAC("Mac");
    private final String name;

    OperatingSystemType(String name) {
        this.name = name;
    }

    @Override
    public String toString() {
        return name;
    }
}
