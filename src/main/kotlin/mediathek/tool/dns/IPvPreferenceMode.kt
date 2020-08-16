package mediathek.tool.dns

/**
 * Specifies, which IP addresses shall be used in DNS resolution.
 */
enum class IPvPreferenceMode(val code: String) {
    SYSTEM("system"),
    IPV6_FIRST("ip_v6"),
    IPV4_FIRST("ip_v4"),
    IPV6_ONLY("ip_v6_only"),
    IPV4_ONLY("ip_v4_only");

    override fun toString(): String {
        return code
    }

    companion object {
        @JvmStatic
        fun fromString(ipMode: String): IPvPreferenceMode =
                values().find { it.code == ipMode } ?: throw IllegalArgumentException("Unknown value $ipMode")
    }
}