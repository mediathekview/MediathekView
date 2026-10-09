package mediathek.controller.starter

enum class StartStatus {
    INITIALIZED,
    RUNNING,
    FINISHED,
    ERROR,
    ;

    fun isBefore(other: StartStatus): Boolean = compareTo(other) < 0

    fun isAfter(other: StartStatus): Boolean = compareTo(other) > 0

    fun isAtLeast(other: StartStatus): Boolean = compareTo(other) >= 0
}
