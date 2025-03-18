// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License. See the LICENSE file in the project root for full license information.

#include <tree_sitter/parser.h>
#include <wctype.h>

enum TOKEN_TYPE {
  STATEMENT_TERMINATOR
};

// Helper function to consume whitespace.
static inline void skip(TSLexer *lexer) {
  lexer->advance(lexer, true);
}

static bool scan_statement_terminator(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  // Only scan if the parser expects a STATEMENT_TERMINATOR.
  if (!valid_symbols[STATEMENT_TERMINATOR])
    return false;

  lexer->result_symbol = STATEMENT_TERMINATOR;
  lexer->mark_end(lexer);

  // Consume all whitespace, handling CR and CR+LF sequences.
  while (iswspace(lexer->lookahead)) {
    // If a carriage return, check if it's part of a CR+LF sequence.
    if (lexer->lookahead == '\r') {
      skip(lexer);
      if (lexer->lookahead == '\n') {
        skip(lexer);
        return true;
      }
      return true;
    }
    skip(lexer);
  }

  // Check for expected terminator characters.
  if (lexer->lookahead == 0 ||   // End-of-file
      lexer->lookahead == '}' ||
      lexer->lookahead == ';' ||
      lexer->lookahead == ')' ||
      lexer->lookahead == '\n') {
    return true;
  }

  // If none of the expected characters are found, fail to match.
  return false;
}

// --- API Implementation ---

void *tree_sitter_powershell_external_scanner_create() {
  return NULL;
}

void tree_sitter_powershell_external_scanner_destroy(void *p) {
  // No state to clean up.
}

unsigned tree_sitter_powershell_external_scanner_serialize(void *payload, char *buffer) {
  // No state to serialize.
  return 0;
}

void tree_sitter_powershell_external_scanner_deserialize(void *payload, const char *buffer, unsigned length) {
  // No state to deserialize.
}

bool tree_sitter_powershell_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols) {
  return scan_statement_terminator(payload, lexer, valid_symbols);
}
