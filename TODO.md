# TODO

## Compaction

The built-in model has a small context window, and a conversation outgrows it quickly. What the
app does for now (`compact` in `src/Main.elm`):

- After each reply the conversation's share of the window is measured. From 80% on (`compactAt`),
  all but the last 10 messages (`keepInContext`) leave the model's context.
- If the Prompt API refuses a turn as too large (`QuotaExceededError`), the conversation is
  compacted the same way and the turn is asked again, once.
- Compacted messages stay in the transcript, under a line that says the model no longer sees them,
  and are saved in localStorage as `archive:<conversation id>`. Attachments are saved by name and
  type only: their data would soon fill localStorage.

This forgets everything older than the last 10 messages. What we want instead:

- [ ] **Summarize instead of dropping.** Before messages leave the context, have the model write a
      summary of them and keep it at the start of the context, updated as more messages leave.
- [ ] **A fixed context budget.** Split the window into parts with their own budgets, for example
      instructions, memory, summary and recent turns, and compact each part to fit its budget instead
      of reacting at 80% of the whole.
- [ ] **Lossless memory we can zoom into.** Keep every message, and a hierarchy of summaries over
      them (turns → episodes → the whole conversation). The context holds the coarse levels, and the
      model can expand a summary back into the messages under it with a tool when it needs the
      detail. That allows very long conversations without losing what was said.
- [ ] **Search the archive.** A tool that finds earlier messages by meaning, with the embeddings
      browser-ai offers, so the model can pull back what is relevant to the current turn.
- [ ] **Storage that fits.** Move the archive from localStorage (about 5 MB, strings only) to
      IndexedDB, keep the attachments, and load past conversations into Recents on start.
- [ ] **Count before sending.** Measure the next turn with `measureContextUsage` before prompting,
      so a turn that won't fit is compacted first instead of failing and being asked again.
