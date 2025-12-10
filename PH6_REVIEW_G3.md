# Phase 6 Review

## Summary
Phase 6 (Containers & Access) is **INCOMPLETE**. While `ToxicParser.Grammar.Containers` has been introduced, it only implements a skeletal version of list and tuple parsing. Critical features like maps, structs, bitstrings, and access syntax are missing or explicitly deferred.

## Status: INCOMPLETE

### Delivered
*   **Module Structure:** `ToxicParser.Grammar.Containers` exists and is integrated into `ToxicParser.Grammar.Expressions`.
*   **Basic List/Tuple Parsing:** Scaffolding for `[...]` and `{...}` exists.

### Gaps (Critical)
1.  **Map & Struct Parsing:** The code explicitly notes `# Map parsing is deferred to a later phase`. Structs are not handled at all.
2.  **Bitstring Parsing:** No handling for `<<...>>` binaries/bitstrings.
3.  **Access Syntax:** `expr[...]` support is missing from the Pratt parser (`led` function).
4.  **List/Tuple Logic Flaws:**
    *   The `parse_elements` loop does not enforce comma separation correctly (it allows `[1 2]`).
    *   No support for trailing commas.
    *   No support for keyword list tails (e.g., `[1, a: 2]`).
5.  **EOE & Recovery:** No EOE normalization inside containers or terminator-based synchronization.

## Recommendations
To mark Phase 6 as done, the following must be implemented:
1.  **Complete Container Parsers:** Implement full parsing for maps (`%{...}`), structs (`%Name{...}`), and bitstrings (`<<...>>`).
2.  **Access Syntax:** Add `[` handling to `ToxicParser.Pratt.led` to support `expr[...]`.
3.  **Correct Element Parsing:** Fix the loop to enforce commas, handle trailing commas, and support keyword list tails.
4.  **EOE Handling:** Ensure newlines are treated correctly inside containers (ignored or treated as separators depending on context).
