# FOSSILPOL Commit Message Fallback

Use this file only when the sibling `../VegVault/.ai/commit-messages.md` is unavailable.

When asked only for a commit message, return exactly one plain-text line in the form `<subject>: <short summary>`, at most 72 characters, with no body, period, quotes, code fence, or explanation.

Use the narrowest durable function, script, or domain subject. Start the summary with a specific verb such as `add`, `adjust`, `correct`, `document`, `preserve`, `remove`, `replace`, `update`, or `validate`. Do not include issue/PR numbers, phase labels, or vague words such as `feat`, `feature`, `fix`, or `enhance`.

Examples:

- `FOSSILPOL: preserve reviewed assembly partitions`
- `chronology: validate age-uncertainty dimensions`
