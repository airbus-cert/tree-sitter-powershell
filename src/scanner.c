// Copyright (c) Microsoft Corporation. All rights reserved.
// Licensed under the MIT License. See the LICENSE file in the project root for full license information.

#include "tree_sitter/parser.h"
#include <wctype.h>

enum TOKEN_TYPE {
    STATEMENT_TERMINATOR,
    CONCAT,
    CONCAT2,
    IS_NOT_COMMAND_PARAMETER,
};

/* --- API --- */

void *tree_sitter_powershell_external_scanner_create();

void tree_sitter_powershell_external_scanner_destroy(void *p);

unsigned tree_sitter_powershell_external_scanner_serialize(void *payload, char *buffer);

void tree_sitter_powershell_external_scanner_deserialize(void *payload, const char *buffer, unsigned length);

bool tree_sitter_powershell_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols);

/* --- Internal Functions --- */

static void skip(TSLexer *lexer) { lexer->advance(lexer, true); }

static bool scan(void *payload, TSLexer *lexer, const bool *valid_symbols)
{
    if (valid_symbols[CONCAT]) {
        if (!(lexer->lookahead == 0 || iswspace(lexer->lookahead) || lexer->lookahead == ')' ||
              lexer->lookahead == ';' || lexer->lookahead == '&' || lexer->lookahead == '|' ||
              lexer->lookahead == '}' || lexer->lookahead == '(' ||lexer->lookahead == '{')) {
            //lexer->lookahead == '>' || lexer->lookahead == '<')) {
            lexer->result_symbol = CONCAT;

            // Ensure that $ or @ is followed by [a-zA-Z_:] to validate concat
            // Handle $$, $?, $_, $^
            // Handle ${ which is the start of a braced variable (but do not validate if it's closed)
            if (lexer->lookahead == '$' || lexer->lookahead == '@') {
                lexer->mark_end(lexer);
                lexer->advance(lexer, false);
                return (lexer->lookahead >= 65 && lexer->lookahead <= 90)  ||  // A-Z
                    (lexer->lookahead >= 97 && lexer->lookahead <= 122) ||  // a-z
                    (lexer->lookahead >= 48 && lexer->lookahead <= 57)  ||  // 0-9
                    lexer->lookahead == '_' || lexer->lookahead == '$' || lexer->lookahead == ':' ||
                    lexer->lookahead == '?' || lexer->lookahead == '^' || lexer->lookahead == '{';
            }

            return true;
        }
    }

    if (valid_symbols[CONCAT2]) {
        if (!(lexer->lookahead == 0 || iswspace(lexer->lookahead) || lexer->lookahead == ')' ||
              lexer->lookahead == ';' || lexer->lookahead == '&' || lexer->lookahead == '|' ||
              lexer->lookahead == '}' || lexer->lookahead == '>' || lexer->lookahead == '<')) {
            lexer->result_symbol = CONCAT2;
            lexer->mark_end(lexer);
            return true;
        }
    }

    if (valid_symbols[IS_NOT_COMMAND_PARAMETER]) {
        if (lexer->lookahead != '-') {
            lexer->result_symbol = IS_NOT_COMMAND_PARAMETER;
            lexer->mark_end(lexer);
            return true;
        }
    }

    if (valid_symbols[STATEMENT_TERMINATOR]) {
        lexer->result_symbol = STATEMENT_TERMINATOR;
        // This token has no characters -- everything is lookahead to determine its existence
        lexer->mark_end(lexer);

        for (;;) {
            if (lexer->lookahead == 0) return true;
            if (lexer->lookahead == '}') return true;
            if (lexer->lookahead == ';') return true;
            if (lexer->lookahead == ')') return true;
            if (lexer->lookahead == '\n') return true;
            if (!iswspace(lexer->lookahead)) return false;
            skip(lexer);
        }
    }

    return false;
}

/* --- API Implementation --- */

bool tree_sitter_powershell_external_scanner_scan(void *payload, TSLexer *lexer, const bool *valid_symbols)
{
    return scan(payload, lexer, valid_symbols);
}

void *tree_sitter_powershell_external_scanner_create()
{
    return NULL;
}

void tree_sitter_powershell_external_scanner_destroy(void *p)
{
}

unsigned tree_sitter_powershell_external_scanner_serialize(void *payload, char *buffer)
{
    return 0;
}

void tree_sitter_powershell_external_scanner_deserialize(void *payload, const char *buffer, unsigned length)
{
}
