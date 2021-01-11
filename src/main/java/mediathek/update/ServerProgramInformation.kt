package mediathek.update

import mediathek.tool.Version

data class ServerProgramInformation(val version: Version) {
    class ParserTags {
        companion object {
            const val INFO = "Info"
            const val VERSION = "Program_Version"
            const val INFO_NO = "number"
        }
    }
}