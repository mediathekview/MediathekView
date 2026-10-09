package mediathek.mac

fun escapeAppleScriptString(value: String): String =
    value.replace("\\", "\\\\").replace("\"", "\\\"")
