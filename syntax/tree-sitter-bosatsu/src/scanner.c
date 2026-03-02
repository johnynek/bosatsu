#include <stdbool.h>
#include <stdint.h>
#include <string.h>
#include <tree_sitter/parser.h>

enum TokenType {
  NEWLINE,
  INDENT,
  DEDENT,
};

#define MAX_INDENTS 256

typedef struct {
  uint16_t indents[MAX_INDENTS];
  uint16_t indent_count;
  uint16_t pending_dedents;
  bool at_line_start;
} Scanner;

static inline void scanner_init(Scanner *scanner) {
  scanner->indent_count = 1;
  scanner->indents[0] = 0;
  scanner->pending_dedents = 0;
  scanner->at_line_start = true;
}

void *tree_sitter_bosatsu_external_scanner_create(void) {
  Scanner *scanner = (Scanner *)malloc(sizeof(Scanner));
  scanner_init(scanner);
  return scanner;
}

void tree_sitter_bosatsu_external_scanner_destroy(void *payload) {
  free(payload);
}

unsigned tree_sitter_bosatsu_external_scanner_serialize(
    void *payload,
    char *buffer
) {
  Scanner *scanner = (Scanner *)payload;
  unsigned size = 0;

  if (size + sizeof(scanner->indent_count) > TREE_SITTER_SERIALIZATION_BUFFER_SIZE)
    return 0;
  memcpy(buffer + size, &scanner->indent_count, sizeof(scanner->indent_count));
  size += sizeof(scanner->indent_count);

  unsigned indents_size = scanner->indent_count * sizeof(uint16_t);
  if (size + indents_size > TREE_SITTER_SERIALIZATION_BUFFER_SIZE)
    return 0;
  memcpy(buffer + size, scanner->indents, indents_size);
  size += indents_size;

  if (size + sizeof(scanner->pending_dedents) > TREE_SITTER_SERIALIZATION_BUFFER_SIZE)
    return 0;
  memcpy(buffer + size, &scanner->pending_dedents, sizeof(scanner->pending_dedents));
  size += sizeof(scanner->pending_dedents);

  if (size + 1 > TREE_SITTER_SERIALIZATION_BUFFER_SIZE)
    return 0;
  buffer[size++] = scanner->at_line_start ? 1 : 0;

  return size;
}

void tree_sitter_bosatsu_external_scanner_deserialize(
    void *payload,
    const char *buffer,
    unsigned length
) {
  Scanner *scanner = (Scanner *)payload;
  scanner_init(scanner);

  if (length == 0)
    return;

  unsigned size = 0;

  if (size + sizeof(scanner->indent_count) > length)
    return;
  memcpy(&scanner->indent_count, buffer + size, sizeof(scanner->indent_count));
  size += sizeof(scanner->indent_count);

  if (scanner->indent_count == 0 || scanner->indent_count > MAX_INDENTS) {
    scanner_init(scanner);
    return;
  }

  unsigned indents_size = scanner->indent_count * sizeof(uint16_t);
  if (size + indents_size > length) {
    scanner_init(scanner);
    return;
  }
  memcpy(scanner->indents, buffer + size, indents_size);
  size += indents_size;

  if (size + sizeof(scanner->pending_dedents) <= length) {
    memcpy(&scanner->pending_dedents, buffer + size, sizeof(scanner->pending_dedents));
    size += sizeof(scanner->pending_dedents);
  }

  if (size < length) {
    scanner->at_line_start = buffer[size] != 0;
  }
}

static inline uint16_t current_indent(const Scanner *scanner) {
  return scanner->indents[scanner->indent_count - 1];
}

static inline uint16_t count_indentation(TSLexer *lexer) {
  uint16_t indent = 0;
  for (;;) {
    if (lexer->lookahead == ' ') {
      indent += 1;
      lexer->advance(lexer, true);
    } else if (lexer->lookahead == '\t') {
      indent += 8;
      lexer->advance(lexer, true);
    } else {
      break;
    }
  }
  return indent;
}

static inline void skip_comment(TSLexer *lexer) {
  while (!lexer->eof(lexer) && lexer->lookahead != '\n' && lexer->lookahead != '\r') {
    lexer->advance(lexer, true);
  }
}

bool tree_sitter_bosatsu_external_scanner_scan(
    void *payload,
    TSLexer *lexer,
    const bool *valid_symbols
) {
  Scanner *scanner = (Scanner *)payload;

  if (valid_symbols[DEDENT] && scanner->pending_dedents > 0) {
    scanner->pending_dedents -= 1;
    lexer->result_symbol = DEDENT;
    return true;
  }

  if (lexer->eof(lexer)) {
    if (valid_symbols[DEDENT] && scanner->indent_count > 1) {
      scanner->indent_count -= 1;
      lexer->result_symbol = DEDENT;
      return true;
    }
    if (valid_symbols[NEWLINE] && !scanner->at_line_start) {
      scanner->at_line_start = true;
      lexer->result_symbol = NEWLINE;
      return true;
    }
    return false;
  }

  if (scanner->at_line_start && (valid_symbols[INDENT] || valid_symbols[DEDENT])) {
    uint16_t indent = count_indentation(lexer);

    if (lexer->lookahead == '#') {
      skip_comment(lexer);
    }

    if (!(lexer->lookahead == '\n' || lexer->lookahead == '\r' || lexer->eof(lexer))) {
      uint16_t current = current_indent(scanner);
      if (indent > current && valid_symbols[INDENT]) {
        if (scanner->indent_count < MAX_INDENTS) {
          scanner->indents[scanner->indent_count++] = indent;
        }
        scanner->at_line_start = false;
        lexer->result_symbol = INDENT;
        return true;
      }

      if (indent < current && valid_symbols[DEDENT]) {
        while (scanner->indent_count > 1 && indent < current_indent(scanner)) {
          scanner->indent_count -= 1;
          scanner->pending_dedents += 1;
        }
        if (scanner->pending_dedents > 0) {
          scanner->pending_dedents -= 1;
          lexer->result_symbol = DEDENT;
          return true;
        }
      }

      scanner->at_line_start = false;
    }
  }

  if (valid_symbols[NEWLINE]) {
    while (lexer->lookahead == ' ' || lexer->lookahead == '\t' || lexer->lookahead == '\f') {
      lexer->advance(lexer, true);
    }

    if (lexer->lookahead == '\r') {
      lexer->advance(lexer, true);
      if (lexer->lookahead == '\n') {
        lexer->advance(lexer, true);
      }
      scanner->at_line_start = true;
      lexer->result_symbol = NEWLINE;
      return true;
    }
    if (lexer->lookahead == '\n') {
      lexer->advance(lexer, true);
      scanner->at_line_start = true;
      lexer->result_symbol = NEWLINE;
      return true;
    }
  }

  scanner->at_line_start = false;
  return false;
}
