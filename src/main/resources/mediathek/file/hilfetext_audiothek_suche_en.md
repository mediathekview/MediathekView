# Searching the Audiothek

The **Filter** field in the **Audiothek** tab filters the displayed entries. Press Enter to search.

Use the field's clear button or press `Esc` to remove the search text and show the full list again.

## Local search

Without a field key, the search covers all currently visible searchable columns:

- Channel
- Genre
- Topic
- Title
- Date
- Time

Matching is case-insensitive and finds partial matches.

```text
tatort
hörspiel
ard
```

Each example finds entries containing that term in the visible search area.

## Searching specific fields

Use `key:value` to restrict the search to a field. Available keys remain unchanged in either interface language:

- `sender`
- `genre`
- `thema`, `theme`
- `titel`, `title`
- `datum`, `date`
- `zeit`, `time`
- `dauer`, `duration`
- `größe`, `groesse`, `size`

Examples:

```text
sender:ard
thema:krimi
titel:"die drei ???"
datum:12.03.2026
größe:120
```

Quoted values match the exact expression. Unquoted values use partial matching.

Combine search terms to require every part to match:

```text
sender:ard thema:hörspiel
titel:welk datum:11.03.2026
```

## Online search

Enable **Online search** to retrieve matching results from **Podcastindex** and add them to the results list.

It runs alongside the local search. An activity indicator appears beside Online search while it runs.

Online search accepts free text and the `titel:`, `title:`, `thema:` and `theme:` keys. For these keys, only the value is sent to Podcastindex. Returned results are then restricted to the specified field.

The `genre:` filter applies only to returned online results and is not sent to Podcastindex. A search containing only `genre:` therefore stays local.

Other keys, such as `sender:`, `datum:` or `größe:`, search only the local Audiothek.

Disable Online search to show only local Audiothek entries.

## Notes

The status bar shows the number of matches.

Visible table columns affect the results: free-text searches cover only visible searchable columns. Duration and Size are excluded from free-text searches.
