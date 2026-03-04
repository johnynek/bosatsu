#include "tree_sitter/parser.h"

#if defined(__GNUC__) || defined(__clang__)
#pragma GCC diagnostic ignored "-Wmissing-field-initializers"
#endif

#define LANGUAGE_VERSION 14
#define STATE_COUNT 35
#define LARGE_STATE_COUNT 26
#define SYMBOL_COUNT 60
#define ALIAS_COUNT 0
#define TOKEN_COUNT 43
#define EXTERNAL_TOKEN_COUNT 3
#define FIELD_COUNT 1
#define MAX_ALIAS_SEQUENCE_LENGTH 3
#define PRODUCTION_ID_COUNT 2

enum ts_symbol_identifiers {
  anon_sym_package = 1,
  anon_sym_SLASH = 2,
  sym_package_segment = 3,
  anon_sym_LPAREN = 4,
  anon_sym_RPAREN = 5,
  anon_sym_LBRACK = 6,
  anon_sym_RBRACK = 7,
  anon_sym_LBRACE = 8,
  anon_sym_RBRACE = 9,
  anon_sym_def = 10,
  anon_sym_enum = 11,
  anon_sym_struct = 12,
  anon_sym_external = 13,
  anon_sym_if = 14,
  anon_sym_elif = 15,
  anon_sym_else = 16,
  anon_sym_match = 17,
  anon_sym_matches = 18,
  anon_sym_case = 19,
  anon_sym_recur = 20,
  anon_sym_loop = 21,
  anon_sym_from = 22,
  anon_sym_import = 23,
  anon_sym_export = 24,
  anon_sym_operator = 25,
  sym_identifier = 26,
  sym_type_identifier = 27,
  sym_backticked_identifier = 28,
  sym_number = 29,
  sym_string = 30,
  sym_character = 31,
  sym_operator = 32,
  sym_unknown = 33,
  anon_sym_COLON = 34,
  anon_sym_COMMA = 35,
  anon_sym_DOT = 36,
  anon_sym_SEMI = 37,
  anon_sym_EQ = 38,
  sym_comment = 39,
  sym__newline = 40,
  sym__indent = 41,
  sym__dedent = 42,
  sym_source_file = 43,
  sym_package_declaration = 44,
  sym_package_name = 45,
  sym_statement = 46,
  sym_simple_statement = 47,
  sym_line_items = 48,
  sym_line_item = 49,
  sym_atom = 50,
  sym_tuple = 51,
  sym_list = 52,
  sym_record = 53,
  sym_keyword = 54,
  sym_punctuation = 55,
  aux_sym_source_file_repeat1 = 56,
  aux_sym_package_name_repeat1 = 57,
  aux_sym_line_items_repeat1 = 58,
  aux_sym_tuple_repeat1 = 59,
};

static const char * const ts_symbol_names[] = {
  [ts_builtin_sym_end] = "end",
  [anon_sym_package] = "package",
  [anon_sym_SLASH] = "/",
  [sym_package_segment] = "package_segment",
  [anon_sym_LPAREN] = "(",
  [anon_sym_RPAREN] = ")",
  [anon_sym_LBRACK] = "[",
  [anon_sym_RBRACK] = "]",
  [anon_sym_LBRACE] = "{",
  [anon_sym_RBRACE] = "}",
  [anon_sym_def] = "def",
  [anon_sym_enum] = "enum",
  [anon_sym_struct] = "struct",
  [anon_sym_external] = "external",
  [anon_sym_if] = "if",
  [anon_sym_elif] = "elif",
  [anon_sym_else] = "else",
  [anon_sym_match] = "match",
  [anon_sym_matches] = "matches",
  [anon_sym_case] = "case",
  [anon_sym_recur] = "recur",
  [anon_sym_loop] = "loop",
  [anon_sym_from] = "from",
  [anon_sym_import] = "import",
  [anon_sym_export] = "export",
  [anon_sym_operator] = "operator",
  [sym_identifier] = "identifier",
  [sym_type_identifier] = "type_identifier",
  [sym_backticked_identifier] = "backticked_identifier",
  [sym_number] = "number",
  [sym_string] = "string",
  [sym_character] = "character",
  [sym_operator] = "operator",
  [sym_unknown] = "unknown",
  [anon_sym_COLON] = ":",
  [anon_sym_COMMA] = ",",
  [anon_sym_DOT] = ".",
  [anon_sym_SEMI] = ";",
  [anon_sym_EQ] = "=",
  [sym_comment] = "comment",
  [sym__newline] = "_newline",
  [sym__indent] = "_indent",
  [sym__dedent] = "_dedent",
  [sym_source_file] = "source_file",
  [sym_package_declaration] = "package_declaration",
  [sym_package_name] = "package_name",
  [sym_statement] = "statement",
  [sym_simple_statement] = "simple_statement",
  [sym_line_items] = "line_items",
  [sym_line_item] = "line_item",
  [sym_atom] = "atom",
  [sym_tuple] = "tuple",
  [sym_list] = "list",
  [sym_record] = "record",
  [sym_keyword] = "keyword",
  [sym_punctuation] = "punctuation",
  [aux_sym_source_file_repeat1] = "source_file_repeat1",
  [aux_sym_package_name_repeat1] = "package_name_repeat1",
  [aux_sym_line_items_repeat1] = "line_items_repeat1",
  [aux_sym_tuple_repeat1] = "tuple_repeat1",
};

static const TSSymbol ts_symbol_map[] = {
  [ts_builtin_sym_end] = ts_builtin_sym_end,
  [anon_sym_package] = anon_sym_package,
  [anon_sym_SLASH] = anon_sym_SLASH,
  [sym_package_segment] = sym_package_segment,
  [anon_sym_LPAREN] = anon_sym_LPAREN,
  [anon_sym_RPAREN] = anon_sym_RPAREN,
  [anon_sym_LBRACK] = anon_sym_LBRACK,
  [anon_sym_RBRACK] = anon_sym_RBRACK,
  [anon_sym_LBRACE] = anon_sym_LBRACE,
  [anon_sym_RBRACE] = anon_sym_RBRACE,
  [anon_sym_def] = anon_sym_def,
  [anon_sym_enum] = anon_sym_enum,
  [anon_sym_struct] = anon_sym_struct,
  [anon_sym_external] = anon_sym_external,
  [anon_sym_if] = anon_sym_if,
  [anon_sym_elif] = anon_sym_elif,
  [anon_sym_else] = anon_sym_else,
  [anon_sym_match] = anon_sym_match,
  [anon_sym_matches] = anon_sym_matches,
  [anon_sym_case] = anon_sym_case,
  [anon_sym_recur] = anon_sym_recur,
  [anon_sym_loop] = anon_sym_loop,
  [anon_sym_from] = anon_sym_from,
  [anon_sym_import] = anon_sym_import,
  [anon_sym_export] = anon_sym_export,
  [anon_sym_operator] = anon_sym_operator,
  [sym_identifier] = sym_identifier,
  [sym_type_identifier] = sym_type_identifier,
  [sym_backticked_identifier] = sym_backticked_identifier,
  [sym_number] = sym_number,
  [sym_string] = sym_string,
  [sym_character] = sym_character,
  [sym_operator] = sym_operator,
  [sym_unknown] = sym_unknown,
  [anon_sym_COLON] = anon_sym_COLON,
  [anon_sym_COMMA] = anon_sym_COMMA,
  [anon_sym_DOT] = anon_sym_DOT,
  [anon_sym_SEMI] = anon_sym_SEMI,
  [anon_sym_EQ] = anon_sym_EQ,
  [sym_comment] = sym_comment,
  [sym__newline] = sym__newline,
  [sym__indent] = sym__indent,
  [sym__dedent] = sym__dedent,
  [sym_source_file] = sym_source_file,
  [sym_package_declaration] = sym_package_declaration,
  [sym_package_name] = sym_package_name,
  [sym_statement] = sym_statement,
  [sym_simple_statement] = sym_simple_statement,
  [sym_line_items] = sym_line_items,
  [sym_line_item] = sym_line_item,
  [sym_atom] = sym_atom,
  [sym_tuple] = sym_tuple,
  [sym_list] = sym_list,
  [sym_record] = sym_record,
  [sym_keyword] = sym_keyword,
  [sym_punctuation] = sym_punctuation,
  [aux_sym_source_file_repeat1] = aux_sym_source_file_repeat1,
  [aux_sym_package_name_repeat1] = aux_sym_package_name_repeat1,
  [aux_sym_line_items_repeat1] = aux_sym_line_items_repeat1,
  [aux_sym_tuple_repeat1] = aux_sym_tuple_repeat1,
};

static const TSSymbolMetadata ts_symbol_metadata[] = {
  [ts_builtin_sym_end] = {
    .visible = false,
    .named = true,
  },
  [anon_sym_package] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SLASH] = {
    .visible = true,
    .named = false,
  },
  [sym_package_segment] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_LPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RPAREN] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACK] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_LBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_RBRACE] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_def] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_enum] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_struct] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_external] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_if] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_elif] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_else] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_match] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_matches] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_case] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_recur] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_loop] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_from] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_import] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_export] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_operator] = {
    .visible = true,
    .named = false,
  },
  [sym_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_type_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_backticked_identifier] = {
    .visible = true,
    .named = true,
  },
  [sym_number] = {
    .visible = true,
    .named = true,
  },
  [sym_string] = {
    .visible = true,
    .named = true,
  },
  [sym_character] = {
    .visible = true,
    .named = true,
  },
  [sym_operator] = {
    .visible = true,
    .named = true,
  },
  [sym_unknown] = {
    .visible = true,
    .named = true,
  },
  [anon_sym_COLON] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_COMMA] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_DOT] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_SEMI] = {
    .visible = true,
    .named = false,
  },
  [anon_sym_EQ] = {
    .visible = true,
    .named = false,
  },
  [sym_comment] = {
    .visible = true,
    .named = true,
  },
  [sym__newline] = {
    .visible = false,
    .named = true,
  },
  [sym__indent] = {
    .visible = false,
    .named = true,
  },
  [sym__dedent] = {
    .visible = false,
    .named = true,
  },
  [sym_source_file] = {
    .visible = true,
    .named = true,
  },
  [sym_package_declaration] = {
    .visible = true,
    .named = true,
  },
  [sym_package_name] = {
    .visible = true,
    .named = true,
  },
  [sym_statement] = {
    .visible = false,
    .named = true,
    .supertype = true,
  },
  [sym_simple_statement] = {
    .visible = true,
    .named = true,
  },
  [sym_line_items] = {
    .visible = true,
    .named = true,
  },
  [sym_line_item] = {
    .visible = true,
    .named = true,
  },
  [sym_atom] = {
    .visible = false,
    .named = true,
    .supertype = true,
  },
  [sym_tuple] = {
    .visible = true,
    .named = true,
  },
  [sym_list] = {
    .visible = true,
    .named = true,
  },
  [sym_record] = {
    .visible = true,
    .named = true,
  },
  [sym_keyword] = {
    .visible = true,
    .named = true,
  },
  [sym_punctuation] = {
    .visible = true,
    .named = true,
  },
  [aux_sym_source_file_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_package_name_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_line_items_repeat1] = {
    .visible = false,
    .named = false,
  },
  [aux_sym_tuple_repeat1] = {
    .visible = false,
    .named = false,
  },
};

enum ts_field_identifiers {
  field_name = 1,
};

static const char * const ts_field_names[] = {
  [0] = NULL,
  [field_name] = "name",
};

static const TSFieldMapSlice ts_field_map_slices[PRODUCTION_ID_COUNT] = {
  [1] = {.index = 0, .length = 1},
};

static const TSFieldMapEntry ts_field_map_entries[] = {
  [0] =
    {field_name, 1},
};

static const TSSymbol ts_alias_sequences[PRODUCTION_ID_COUNT][MAX_ALIAS_SEQUENCE_LENGTH] = {
  [0] = {0},
};

static const uint16_t ts_non_terminal_alias_map[] = {
  0,
};

static const TSStateId ts_primary_state_ids[STATE_COUNT] = {
  [0] = 0,
  [1] = 1,
  [2] = 2,
  [3] = 3,
  [4] = 4,
  [5] = 5,
  [6] = 6,
  [7] = 7,
  [8] = 8,
  [9] = 9,
  [10] = 10,
  [11] = 11,
  [12] = 12,
  [13] = 13,
  [14] = 14,
  [15] = 15,
  [16] = 16,
  [17] = 17,
  [18] = 18,
  [19] = 19,
  [20] = 20,
  [21] = 21,
  [22] = 22,
  [23] = 23,
  [24] = 24,
  [25] = 25,
  [26] = 26,
  [27] = 27,
  [28] = 28,
  [29] = 29,
  [30] = 30,
  [31] = 31,
  [32] = 32,
  [33] = 33,
  [34] = 34,
};

static TSCharacterRange sym_operator_character_set_1[] = {
  {'!', '!'}, {'$', '&'}, {'*', '+'}, {'-', '/'}, {'<', '@'}, {'\\', '\\'}, {'^', '^'}, {'|', '|'},
  {'~', '~'},
};

static TSCharacterRange sym_unknown_character_set_1[] = {
  {0, 0x08}, {0x0e, 0x1f}, {'!', '\''}, {'*', '9'}, {';', 'Z'}, {'\\', '\\'}, {'^', 'z'}, {'|', '|'},
  {'~', 0x10ffff},
};

static bool ts_lex(TSLexer *lexer, TSStateId state) {
  START_LEXER();
  eof = lexer->eof(lexer);
  switch (state) {
    case 0:
      if (eof) ADVANCE(31);
      ADVANCE_MAP(
        '"', 11,
        '#', 246,
        '\'', 15,
        '(', 37,
        ')', 38,
        ',', 238,
        '-', 216,
        '.', 240,
        '/', 35,
        '0', 197,
        ':', 237,
        ';', 242,
        '=', 244,
        '[', 39,
        ']', 40,
        '`', 25,
        'c', 75,
        'd', 95,
        'e', 123,
        'f', 159,
        'i', 109,
        'l', 135,
        'm', 83,
        'o', 147,
        'p', 76,
        'r', 103,
        's', 179,
        '{', 41,
        '}', 42,
      );
      if (lookahead == '\t' ||
          lookahead == '\f' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(0);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(198);
      if (('!' <= lookahead && lookahead <= '@') ||
          ('\\' <= lookahead && lookahead <= '^') ||
          ('|' <= lookahead && lookahead <= '~')) ADVANCE(218);
      if (('_' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(193);
      END_STATE();
    case 1:
      if (lookahead == '\n') ADVANCE(18);
      if (lookahead == '"') ADVANCE(210);
      if (lookahead == '$') ADVANCE(2);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == '{') ADVANCE(4);
      if (lookahead == '}') ADVANCE(11);
      if (lookahead != 0) ADVANCE(1);
      END_STATE();
    case 2:
      if (lookahead == '\n') ADVANCE(18);
      if (lookahead == '"') ADVANCE(210);
      if (lookahead == '$') ADVANCE(2);
      if (lookahead == '\\') ADVANCE(3);
      if (lookahead == '{') ADVANCE(7);
      if (lookahead == '}') ADVANCE(11);
      if (lookahead != 0) ADVANCE(1);
      END_STATE();
    case 3:
      if (lookahead == '\n') ADVANCE(18);
      if (lookahead == '{') ADVANCE(4);
      if (lookahead == '}') ADVANCE(11);
      if (lookahead != 0) ADVANCE(1);
      END_STATE();
    case 4:
      if (lookahead == '\n') ADVANCE(20);
      if (lookahead == '"') ADVANCE(212);
      if (lookahead == '$') ADVANCE(5);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == '{') ADVANCE(11);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 5:
      if (lookahead == '\n') ADVANCE(20);
      if (lookahead == '"') ADVANCE(212);
      if (lookahead == '$') ADVANCE(5);
      if (lookahead == '\\') ADVANCE(6);
      if (lookahead == '{') ADVANCE(1);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 6:
      if (lookahead == '\n') ADVANCE(20);
      if (lookahead == '{') ADVANCE(11);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(4);
      END_STATE();
    case 7:
      if (lookahead == '\n') ADVANCE(19);
      if (lookahead == '"') ADVANCE(211);
      if (lookahead == '$') ADVANCE(8);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead == '{') ADVANCE(4);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(7);
      END_STATE();
    case 8:
      if (lookahead == '\n') ADVANCE(19);
      if (lookahead == '"') ADVANCE(211);
      if (lookahead == '$') ADVANCE(8);
      if (lookahead == '\\') ADVANCE(9);
      if (lookahead == '{') ADVANCE(7);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(7);
      END_STATE();
    case 9:
      if (lookahead == '\n') ADVANCE(19);
      if (lookahead == '{') ADVANCE(4);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(7);
      END_STATE();
    case 10:
      if (lookahead == '"') ADVANCE(209);
      if (lookahead == '$') ADVANCE(10);
      if (lookahead == '\\') ADVANCE(26);
      if (lookahead == '{') ADVANCE(1);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(11);
      END_STATE();
    case 11:
      if (lookahead == '"') ADVANCE(209);
      if (lookahead == '$') ADVANCE(10);
      if (lookahead == '\\') ADVANCE(26);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(11);
      END_STATE();
    case 12:
      if (lookahead == '"') ADVANCE(214);
      if (lookahead == '\\') ADVANCE(28);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(12);
      END_STATE();
    case 13:
      ADVANCE_MAP(
        '"', 221,
        '#', 231,
        '\'', 223,
        '(', 37,
        ')', 38,
        ',', 239,
        '-', 217,
        '.', 241,
        '0', 199,
        ':', 237,
        ';', 243,
        '=', 245,
        '[', 39,
        ']', 40,
        '`', 225,
        'c', 79,
        'd', 99,
        'e', 125,
        'f', 160,
        'i', 112,
        'l', 137,
        'm', 84,
        'o', 148,
        'r', 104,
        's', 180,
        '{', 41,
        '}', 42,
      );
      if (lookahead == '\t' ||
          lookahead == '\f' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(13);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(200);
      if (('!' <= lookahead && lookahead <= '@') ||
          ('\\' <= lookahead && lookahead <= '^') ||
          ('|' <= lookahead && lookahead <= '~')) ADVANCE(219);
      if (('_' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(194);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(236);
      END_STATE();
    case 14:
      if (lookahead == '#') ADVANCE(246);
      if (lookahead == '/') ADVANCE(34);
      if (lookahead == '\t' ||
          lookahead == '\f' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(14);
      if (('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 15:
      if (lookahead == '\'') ADVANCE(209);
      if (lookahead == '\\') ADVANCE(27);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(15);
      END_STATE();
    case 16:
      if (lookahead == '\'') ADVANCE(214);
      if (lookahead == '\\') ADVANCE(29);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(16);
      END_STATE();
    case 17:
      if (lookahead == '`') ADVANCE(195);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(17);
      END_STATE();
    case 18:
      if (lookahead == '{') ADVANCE(20);
      if (lookahead == '}') ADVANCE(11);
      if (lookahead != 0) ADVANCE(18);
      END_STATE();
    case 19:
      if (lookahead == '{') ADVANCE(20);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(19);
      END_STATE();
    case 20:
      if (lookahead == '}') ADVANCE(18);
      if (lookahead != 0 &&
          lookahead != '{') ADVANCE(20);
      END_STATE();
    case 21:
      if (lookahead == '0' ||
          lookahead == '1' ||
          lookahead == '_') ADVANCE(201);
      END_STATE();
    case 22:
      if (('0' <= lookahead && lookahead <= '7') ||
          lookahead == '_') ADVANCE(203);
      END_STATE();
    case 23:
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(205);
      END_STATE();
    case 24:
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(207);
      END_STATE();
    case 25:
      if (lookahead != 0 &&
          lookahead != '\n' &&
          lookahead != '`') ADVANCE(17);
      END_STATE();
    case 26:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(11);
      END_STATE();
    case 27:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(15);
      END_STATE();
    case 28:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(12);
      END_STATE();
    case 29:
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(16);
      END_STATE();
    case 30:
      if (eof) ADVANCE(31);
      ADVANCE_MAP(
        '"', 221,
        '#', 231,
        '\'', 223,
        '(', 37,
        ',', 239,
        '-', 217,
        '.', 241,
        '0', 199,
        ':', 237,
        ';', 243,
        '=', 245,
        '[', 39,
        '`', 225,
        'c', 79,
        'd', 99,
        'e', 125,
        'f', 160,
        'i', 112,
        'l', 137,
        'm', 84,
        'o', 148,
        'p', 80,
        'r', 104,
        's', 180,
        '{', 41,
      );
      if (lookahead == '\t' ||
          lookahead == '\f' ||
          lookahead == '\r' ||
          lookahead == ' ') SKIP(30);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(200);
      if (('!' <= lookahead && lookahead <= '&') ||
          ('*' <= lookahead && lookahead <= '@') ||
          lookahead == '\\' ||
          lookahead == '^' ||
          lookahead == '|' ||
          lookahead == '~') ADVANCE(219);
      if (('_' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if (('A' <= lookahead && lookahead <= 'Z')) ADVANCE(194);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          (lookahead < ' ' || '~' < lookahead)) ADVANCE(236);
      END_STATE();
    case 31:
      ACCEPT_TOKEN(ts_builtin_sym_end);
      END_STATE();
    case 32:
      ACCEPT_TOKEN(anon_sym_package);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 33:
      ACCEPT_TOKEN(anon_sym_package);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 34:
      ACCEPT_TOKEN(anon_sym_SLASH);
      END_STATE();
    case 35:
      ACCEPT_TOKEN(anon_sym_SLASH);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(218);
      END_STATE();
    case 36:
      ACCEPT_TOKEN(sym_package_segment);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(36);
      END_STATE();
    case 37:
      ACCEPT_TOKEN(anon_sym_LPAREN);
      END_STATE();
    case 38:
      ACCEPT_TOKEN(anon_sym_RPAREN);
      END_STATE();
    case 39:
      ACCEPT_TOKEN(anon_sym_LBRACK);
      END_STATE();
    case 40:
      ACCEPT_TOKEN(anon_sym_RBRACK);
      END_STATE();
    case 41:
      ACCEPT_TOKEN(anon_sym_LBRACE);
      END_STATE();
    case 42:
      ACCEPT_TOKEN(anon_sym_RBRACE);
      END_STATE();
    case 43:
      ACCEPT_TOKEN(anon_sym_def);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 44:
      ACCEPT_TOKEN(anon_sym_def);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 45:
      ACCEPT_TOKEN(anon_sym_enum);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 46:
      ACCEPT_TOKEN(anon_sym_enum);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 47:
      ACCEPT_TOKEN(anon_sym_struct);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 48:
      ACCEPT_TOKEN(anon_sym_struct);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 49:
      ACCEPT_TOKEN(anon_sym_external);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 50:
      ACCEPT_TOKEN(anon_sym_external);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 51:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 52:
      ACCEPT_TOKEN(anon_sym_if);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 53:
      ACCEPT_TOKEN(anon_sym_elif);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 54:
      ACCEPT_TOKEN(anon_sym_elif);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 55:
      ACCEPT_TOKEN(anon_sym_else);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 56:
      ACCEPT_TOKEN(anon_sym_else);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 57:
      ACCEPT_TOKEN(anon_sym_match);
      if (lookahead == 'e') ADVANCE(169);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 58:
      ACCEPT_TOKEN(anon_sym_match);
      if (lookahead == 'e') ADVANCE(170);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 59:
      ACCEPT_TOKEN(anon_sym_matches);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 60:
      ACCEPT_TOKEN(anon_sym_matches);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 61:
      ACCEPT_TOKEN(anon_sym_case);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 62:
      ACCEPT_TOKEN(anon_sym_case);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 63:
      ACCEPT_TOKEN(anon_sym_recur);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 64:
      ACCEPT_TOKEN(anon_sym_recur);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 65:
      ACCEPT_TOKEN(anon_sym_loop);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 66:
      ACCEPT_TOKEN(anon_sym_loop);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 67:
      ACCEPT_TOKEN(anon_sym_from);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 68:
      ACCEPT_TOKEN(anon_sym_from);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 69:
      ACCEPT_TOKEN(anon_sym_import);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 70:
      ACCEPT_TOKEN(anon_sym_import);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 71:
      ACCEPT_TOKEN(anon_sym_export);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 72:
      ACCEPT_TOKEN(anon_sym_export);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 73:
      ACCEPT_TOKEN(anon_sym_operator);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 74:
      ACCEPT_TOKEN(anon_sym_operator);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 75:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(171);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 76:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(87);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 77:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(115);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 78:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(124);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 79:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(172);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 80:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(89);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 81:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(116);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 82:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(126);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 83:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(181);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 84:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(182);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 85:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(183);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 86:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'a') ADVANCE(184);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('b' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 87:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(121);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 88:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(117);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 89:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(122);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 90:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(118);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 91:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(189);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 92:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(190);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 93:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(175);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 94:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'c') ADVANCE(178);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 95:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(110);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 96:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(61);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 97:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(55);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 98:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(32);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 99:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(113);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 100:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(62);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 101:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(56);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 102:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(33);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 103:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(91);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 104:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(92);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 105:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(167);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 106:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(168);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 107:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(153);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 108:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'e') ADVANCE(156);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 109:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(51);
      if (lookahead == 'm') ADVANCE(151);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 110:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(43);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 111:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(53);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 112:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(52);
      if (lookahead == 'm') ADVANCE(152);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 113:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(44);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 114:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'f') ADVANCE(54);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 115:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'g') ADVANCE(98);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 116:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'g') ADVANCE(102);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 117:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(57);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 118:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'h') ADVANCE(58);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 119:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(111);
      if (lookahead == 's') ADVANCE(97);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 120:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'i') ADVANCE(114);
      if (lookahead == 's') ADVANCE(101);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 121:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'k') ADVANCE(77);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 122:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'k') ADVANCE(81);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 123:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(119);
      if (lookahead == 'n') ADVANCE(185);
      if (lookahead == 'x') ADVANCE(149);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 124:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(49);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 125:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(120);
      if (lookahead == 'n') ADVANCE(186);
      if (lookahead == 'x') ADVANCE(150);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 126:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'l') ADVANCE(50);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 127:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(45);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 128:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(67);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 129:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(46);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 130:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'm') ADVANCE(68);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 131:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(78);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 132:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'n') ADVANCE(82);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 133:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(128);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 134:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(130);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 135:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(136);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 136:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(145);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 137:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(138);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 138:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(146);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 139:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(161);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 140:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(163);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 141:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(165);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 142:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(166);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 143:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(155);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 144:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'o') ADVANCE(158);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 145:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(65);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 146:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(66);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 147:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(105);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 148:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(106);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 149:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(139);
      if (lookahead == 't') ADVANCE(107);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 150:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(140);
      if (lookahead == 't') ADVANCE(108);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 151:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(141);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 152:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'p') ADVANCE(142);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 153:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(131);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 154:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(63);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 155:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(73);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 156:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(132);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 157:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(64);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 158:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(74);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 159:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(133);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 160:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(134);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 161:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(173);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 162:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(187);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 163:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(176);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 164:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(188);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 165:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(174);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 166:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(177);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 167:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(85);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 168:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'r') ADVANCE(86);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 169:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(59);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 170:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(60);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 171:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(96);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 172:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 's') ADVANCE(100);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 173:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(71);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 174:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(69);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 175:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(47);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 176:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(72);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 177:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(70);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 178:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(48);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 179:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(162);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 180:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(164);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 181:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(88);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 182:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(90);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 183:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(143);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 184:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 't') ADVANCE(144);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 185:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(127);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 186:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(129);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 187:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(93);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 188:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(94);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 189:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(154);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 190:
      ACCEPT_TOKEN(sym_identifier);
      if (lookahead == 'u') ADVANCE(157);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 191:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(191);
      END_STATE();
    case 192:
      ACCEPT_TOKEN(sym_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(192);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 193:
      ACCEPT_TOKEN(sym_type_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(193);
      END_STATE();
    case 194:
      ACCEPT_TOKEN(sym_type_identifier);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'Z') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'z')) ADVANCE(194);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 195:
      ACCEPT_TOKEN(sym_backticked_identifier);
      END_STATE();
    case 196:
      ACCEPT_TOKEN(sym_backticked_identifier);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 197:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(23);
      if (lookahead == 'B' ||
          lookahead == 'b') ADVANCE(21);
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(22);
      if (lookahead == 'X' ||
          lookahead == 'x') ADVANCE(24);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(198);
      END_STATE();
    case 198:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(23);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(198);
      END_STATE();
    case 199:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(229);
      if (lookahead == 'B' ||
          lookahead == 'b') ADVANCE(227);
      if (lookahead == 'O' ||
          lookahead == 'o') ADVANCE(228);
      if (lookahead == 'X' ||
          lookahead == 'x') ADVANCE(235);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(200);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 200:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '.') ADVANCE(229);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(200);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 201:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '0' ||
          lookahead == '1' ||
          lookahead == '_') ADVANCE(201);
      END_STATE();
    case 202:
      ACCEPT_TOKEN(sym_number);
      if (lookahead == '0' ||
          lookahead == '1' ||
          lookahead == '_') ADVANCE(202);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 203:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '7') ||
          lookahead == '_') ADVANCE(203);
      END_STATE();
    case 204:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '7') ||
          lookahead == '_') ADVANCE(204);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 205:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(205);
      END_STATE();
    case 206:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(206);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 207:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(207);
      END_STATE();
    case 208:
      ACCEPT_TOKEN(sym_number);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(208);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 209:
      ACCEPT_TOKEN(sym_string);
      END_STATE();
    case 210:
      ACCEPT_TOKEN(sym_string);
      if (lookahead == '{') ADVANCE(20);
      if (lookahead == '}') ADVANCE(11);
      if (lookahead != 0) ADVANCE(18);
      END_STATE();
    case 211:
      ACCEPT_TOKEN(sym_string);
      if (lookahead == '{') ADVANCE(20);
      if (lookahead == '}') ADVANCE(1);
      if (lookahead != 0) ADVANCE(19);
      END_STATE();
    case 212:
      ACCEPT_TOKEN(sym_string);
      if (lookahead == '}') ADVANCE(18);
      if (lookahead != 0 &&
          lookahead != '{') ADVANCE(20);
      END_STATE();
    case 213:
      ACCEPT_TOKEN(sym_string);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 214:
      ACCEPT_TOKEN(sym_character);
      END_STATE();
    case 215:
      ACCEPT_TOKEN(sym_character);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 216:
      ACCEPT_TOKEN(sym_operator);
      if (lookahead == '0') ADVANCE(197);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(198);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(218);
      END_STATE();
    case 217:
      ACCEPT_TOKEN(sym_operator);
      if (lookahead == '0') ADVANCE(199);
      if (('1' <= lookahead && lookahead <= '9')) ADVANCE(200);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(219);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '!' &&
          (lookahead < '(' || '+' < lookahead) &&
          (lookahead < '-' || ':' < lookahead) &&
          (lookahead < '[' || '^' < lookahead) &&
          (lookahead < '{' || '~' < lookahead)) ADVANCE(236);
      END_STATE();
    case 218:
      ACCEPT_TOKEN(sym_operator);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(218);
      END_STATE();
    case 219:
      ACCEPT_TOKEN(sym_operator);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(219);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '!' &&
          (lookahead < '(' || '+' < lookahead) &&
          lookahead != ':' &&
          (lookahead < '[' || '^' < lookahead) &&
          (lookahead < '{' || '~' < lookahead)) ADVANCE(236);
      END_STATE();
    case 220:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '"') ADVANCE(213);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(230);
      if (lookahead == '{') ADVANCE(1);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == '}') ADVANCE(11);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(221);
      END_STATE();
    case 221:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '"') ADVANCE(213);
      if (lookahead == '$') ADVANCE(220);
      if (lookahead == '\\') ADVANCE(230);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(11);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(221);
      END_STATE();
    case 222:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '"') ADVANCE(215);
      if (lookahead == '\\') ADVANCE(233);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(12);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(222);
      END_STATE();
    case 223:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\'') ADVANCE(213);
      if (lookahead == '\\') ADVANCE(232);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(15);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(223);
      END_STATE();
    case 224:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\'') ADVANCE(215);
      if (lookahead == '\\') ADVANCE(234);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          ('[' <= lookahead && lookahead <= ']') ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(16);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(224);
      END_STATE();
    case 225:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '`') ADVANCE(236);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(17);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(226);
      END_STATE();
    case 226:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '`') ADVANCE(196);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(17);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(226);
      END_STATE();
    case 227:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '0' ||
          lookahead == '1' ||
          lookahead == '_') ADVANCE(202);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 228:
      ACCEPT_TOKEN(sym_unknown);
      if (('0' <= lookahead && lookahead <= '7') ||
          lookahead == '_') ADVANCE(204);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 229:
      ACCEPT_TOKEN(sym_unknown);
      if (('0' <= lookahead && lookahead <= '9') ||
          lookahead == '_') ADVANCE(206);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 230:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(11);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(221);
      END_STATE();
    case 231:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(246);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(231);
      END_STATE();
    case 232:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(15);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(223);
      END_STATE();
    case 233:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(12);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(222);
      END_STATE();
    case 234:
      ACCEPT_TOKEN(sym_unknown);
      if (lookahead == '\t' ||
          (0x0b <= lookahead && lookahead <= '\r') ||
          lookahead == ' ' ||
          lookahead == '(' ||
          lookahead == ')' ||
          lookahead == ':' ||
          lookahead == '[' ||
          lookahead == ']' ||
          lookahead == '{' ||
          lookahead == '}') ADVANCE(16);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead)) ADVANCE(224);
      END_STATE();
    case 235:
      ACCEPT_TOKEN(sym_unknown);
      if (('0' <= lookahead && lookahead <= '9') ||
          ('A' <= lookahead && lookahead <= 'F') ||
          lookahead == '_' ||
          ('a' <= lookahead && lookahead <= 'f')) ADVANCE(208);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 236:
      ACCEPT_TOKEN(sym_unknown);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 237:
      ACCEPT_TOKEN(anon_sym_COLON);
      END_STATE();
    case 238:
      ACCEPT_TOKEN(anon_sym_COMMA);
      END_STATE();
    case 239:
      ACCEPT_TOKEN(anon_sym_COMMA);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 240:
      ACCEPT_TOKEN(anon_sym_DOT);
      if (lookahead == '"') ADVANCE(12);
      if (lookahead == '\'') ADVANCE(16);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(218);
      END_STATE();
    case 241:
      ACCEPT_TOKEN(anon_sym_DOT);
      if (lookahead == '"') ADVANCE(222);
      if (lookahead == '\'') ADVANCE(224);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(219);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          (lookahead < ' ' || '"' < lookahead) &&
          (lookahead < '$' || '+' < lookahead) &&
          lookahead != ':' &&
          (lookahead < '[' || '^' < lookahead) &&
          (lookahead < '{' || '~' < lookahead)) ADVANCE(236);
      END_STATE();
    case 242:
      ACCEPT_TOKEN(anon_sym_SEMI);
      END_STATE();
    case 243:
      ACCEPT_TOKEN(anon_sym_SEMI);
      if ((!eof && set_contains(sym_unknown_character_set_1, 9, lookahead))) ADVANCE(236);
      END_STATE();
    case 244:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(218);
      END_STATE();
    case 245:
      ACCEPT_TOKEN(anon_sym_EQ);
      if (set_contains(sym_operator_character_set_1, 9, lookahead)) ADVANCE(219);
      if (lookahead != 0 &&
          (lookahead < '\t' || '\r' < lookahead) &&
          lookahead != ' ' &&
          lookahead != '!' &&
          (lookahead < '(' || '+' < lookahead) &&
          lookahead != ':' &&
          (lookahead < '[' || '^' < lookahead) &&
          (lookahead < '{' || '~' < lookahead)) ADVANCE(236);
      END_STATE();
    case 246:
      ACCEPT_TOKEN(sym_comment);
      if (lookahead != 0 &&
          lookahead != '\n') ADVANCE(246);
      END_STATE();
    default:
      return false;
  }
}

static const TSLexMode ts_lex_modes[STATE_COUNT] = {
  [0] = {.lex_state = 0, .external_lex_state = 1},
  [1] = {.lex_state = 30, .external_lex_state = 1},
  [2] = {.lex_state = 30, .external_lex_state = 1},
  [3] = {.lex_state = 30, .external_lex_state = 1},
  [4] = {.lex_state = 13, .external_lex_state = 2},
  [5] = {.lex_state = 13, .external_lex_state = 2},
  [6] = {.lex_state = 13, .external_lex_state = 2},
  [7] = {.lex_state = 13, .external_lex_state = 2},
  [8] = {.lex_state = 13, .external_lex_state = 2},
  [9] = {.lex_state = 13, .external_lex_state = 2},
  [10] = {.lex_state = 13, .external_lex_state = 2},
  [11] = {.lex_state = 13, .external_lex_state = 2},
  [12] = {.lex_state = 13, .external_lex_state = 2},
  [13] = {.lex_state = 30, .external_lex_state = 1},
  [14] = {.lex_state = 30, .external_lex_state = 1},
  [15] = {.lex_state = 30, .external_lex_state = 1},
  [16] = {.lex_state = 13, .external_lex_state = 2},
  [17] = {.lex_state = 13, .external_lex_state = 2},
  [18] = {.lex_state = 13, .external_lex_state = 2},
  [19] = {.lex_state = 13, .external_lex_state = 2},
  [20] = {.lex_state = 13, .external_lex_state = 2},
  [21] = {.lex_state = 13, .external_lex_state = 2},
  [22] = {.lex_state = 13, .external_lex_state = 2},
  [23] = {.lex_state = 13, .external_lex_state = 2},
  [24] = {.lex_state = 13, .external_lex_state = 2},
  [25] = {.lex_state = 13, .external_lex_state = 2},
  [26] = {.lex_state = 14, .external_lex_state = 2},
  [27] = {.lex_state = 14, .external_lex_state = 2},
  [28] = {.lex_state = 14, .external_lex_state = 2},
  [29] = {.lex_state = 14},
  [30] = {.lex_state = 14, .external_lex_state = 2},
  [31] = {.lex_state = 0, .external_lex_state = 2},
  [32] = {.lex_state = 0},
  [33] = {.lex_state = 14},
  [34] = {.lex_state = 0, .external_lex_state = 2},
};

static const uint16_t ts_parse_table[LARGE_STATE_COUNT][SYMBOL_COUNT] = {
  [0] = {
    [ts_builtin_sym_end] = ACTIONS(1),
    [anon_sym_package] = ACTIONS(1),
    [anon_sym_SLASH] = ACTIONS(1),
    [anon_sym_LPAREN] = ACTIONS(1),
    [anon_sym_RPAREN] = ACTIONS(1),
    [anon_sym_LBRACK] = ACTIONS(1),
    [anon_sym_RBRACK] = ACTIONS(1),
    [anon_sym_LBRACE] = ACTIONS(1),
    [anon_sym_RBRACE] = ACTIONS(1),
    [anon_sym_def] = ACTIONS(1),
    [anon_sym_enum] = ACTIONS(1),
    [anon_sym_struct] = ACTIONS(1),
    [anon_sym_external] = ACTIONS(1),
    [anon_sym_if] = ACTIONS(1),
    [anon_sym_elif] = ACTIONS(1),
    [anon_sym_else] = ACTIONS(1),
    [anon_sym_match] = ACTIONS(1),
    [anon_sym_matches] = ACTIONS(1),
    [anon_sym_case] = ACTIONS(1),
    [anon_sym_recur] = ACTIONS(1),
    [anon_sym_loop] = ACTIONS(1),
    [anon_sym_from] = ACTIONS(1),
    [anon_sym_import] = ACTIONS(1),
    [anon_sym_export] = ACTIONS(1),
    [anon_sym_operator] = ACTIONS(1),
    [sym_identifier] = ACTIONS(1),
    [sym_type_identifier] = ACTIONS(1),
    [sym_backticked_identifier] = ACTIONS(1),
    [sym_number] = ACTIONS(1),
    [sym_string] = ACTIONS(1),
    [sym_character] = ACTIONS(1),
    [sym_operator] = ACTIONS(1),
    [anon_sym_COLON] = ACTIONS(1),
    [anon_sym_COMMA] = ACTIONS(1),
    [anon_sym_DOT] = ACTIONS(1),
    [anon_sym_SEMI] = ACTIONS(1),
    [anon_sym_EQ] = ACTIONS(1),
    [sym_comment] = ACTIONS(3),
    [sym__newline] = ACTIONS(1),
    [sym__indent] = ACTIONS(1),
    [sym__dedent] = ACTIONS(1),
  },
  [1] = {
    [sym_source_file] = STATE(32),
    [sym_package_declaration] = STATE(3),
    [sym_statement] = STATE(3),
    [sym_simple_statement] = STATE(15),
    [sym_line_items] = STATE(34),
    [sym_line_item] = STATE(12),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_source_file_repeat1] = STATE(3),
    [aux_sym_line_items_repeat1] = STATE(12),
    [ts_builtin_sym_end] = ACTIONS(5),
    [anon_sym_package] = ACTIONS(7),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(27),
    [sym__indent] = ACTIONS(27),
    [sym__dedent] = ACTIONS(27),
  },
  [2] = {
    [sym_package_declaration] = STATE(2),
    [sym_statement] = STATE(2),
    [sym_simple_statement] = STATE(15),
    [sym_line_items] = STATE(34),
    [sym_line_item] = STATE(12),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_source_file_repeat1] = STATE(2),
    [aux_sym_line_items_repeat1] = STATE(12),
    [ts_builtin_sym_end] = ACTIONS(29),
    [anon_sym_package] = ACTIONS(31),
    [anon_sym_LPAREN] = ACTIONS(34),
    [anon_sym_LBRACK] = ACTIONS(37),
    [anon_sym_LBRACE] = ACTIONS(40),
    [anon_sym_def] = ACTIONS(43),
    [anon_sym_enum] = ACTIONS(43),
    [anon_sym_struct] = ACTIONS(43),
    [anon_sym_external] = ACTIONS(43),
    [anon_sym_if] = ACTIONS(43),
    [anon_sym_elif] = ACTIONS(43),
    [anon_sym_else] = ACTIONS(43),
    [anon_sym_match] = ACTIONS(43),
    [anon_sym_matches] = ACTIONS(43),
    [anon_sym_case] = ACTIONS(43),
    [anon_sym_recur] = ACTIONS(43),
    [anon_sym_loop] = ACTIONS(43),
    [anon_sym_from] = ACTIONS(43),
    [anon_sym_import] = ACTIONS(43),
    [anon_sym_export] = ACTIONS(43),
    [anon_sym_operator] = ACTIONS(43),
    [sym_identifier] = ACTIONS(46),
    [sym_type_identifier] = ACTIONS(46),
    [sym_backticked_identifier] = ACTIONS(49),
    [sym_number] = ACTIONS(49),
    [sym_string] = ACTIONS(49),
    [sym_character] = ACTIONS(49),
    [sym_operator] = ACTIONS(46),
    [sym_unknown] = ACTIONS(46),
    [anon_sym_COLON] = ACTIONS(52),
    [anon_sym_COMMA] = ACTIONS(55),
    [anon_sym_DOT] = ACTIONS(55),
    [anon_sym_SEMI] = ACTIONS(55),
    [anon_sym_EQ] = ACTIONS(55),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(58),
    [sym__indent] = ACTIONS(58),
    [sym__dedent] = ACTIONS(58),
  },
  [3] = {
    [sym_package_declaration] = STATE(2),
    [sym_statement] = STATE(2),
    [sym_simple_statement] = STATE(15),
    [sym_line_items] = STATE(34),
    [sym_line_item] = STATE(12),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_source_file_repeat1] = STATE(2),
    [aux_sym_line_items_repeat1] = STATE(12),
    [ts_builtin_sym_end] = ACTIONS(61),
    [anon_sym_package] = ACTIONS(7),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(63),
    [sym__indent] = ACTIONS(63),
    [sym__dedent] = ACTIONS(63),
  },
  [4] = {
    [sym_line_item] = STATE(4),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(4),
    [anon_sym_LPAREN] = ACTIONS(65),
    [anon_sym_RPAREN] = ACTIONS(68),
    [anon_sym_LBRACK] = ACTIONS(70),
    [anon_sym_RBRACK] = ACTIONS(68),
    [anon_sym_LBRACE] = ACTIONS(73),
    [anon_sym_RBRACE] = ACTIONS(68),
    [anon_sym_def] = ACTIONS(76),
    [anon_sym_enum] = ACTIONS(76),
    [anon_sym_struct] = ACTIONS(76),
    [anon_sym_external] = ACTIONS(76),
    [anon_sym_if] = ACTIONS(76),
    [anon_sym_elif] = ACTIONS(76),
    [anon_sym_else] = ACTIONS(76),
    [anon_sym_match] = ACTIONS(76),
    [anon_sym_matches] = ACTIONS(76),
    [anon_sym_case] = ACTIONS(76),
    [anon_sym_recur] = ACTIONS(76),
    [anon_sym_loop] = ACTIONS(76),
    [anon_sym_from] = ACTIONS(76),
    [anon_sym_import] = ACTIONS(76),
    [anon_sym_export] = ACTIONS(76),
    [anon_sym_operator] = ACTIONS(76),
    [sym_identifier] = ACTIONS(79),
    [sym_type_identifier] = ACTIONS(79),
    [sym_backticked_identifier] = ACTIONS(82),
    [sym_number] = ACTIONS(82),
    [sym_string] = ACTIONS(82),
    [sym_character] = ACTIONS(82),
    [sym_operator] = ACTIONS(79),
    [sym_unknown] = ACTIONS(79),
    [anon_sym_COLON] = ACTIONS(85),
    [anon_sym_COMMA] = ACTIONS(88),
    [anon_sym_DOT] = ACTIONS(88),
    [anon_sym_SEMI] = ACTIONS(88),
    [anon_sym_EQ] = ACTIONS(88),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(91),
  },
  [5] = {
    [sym_line_item] = STATE(6),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(6),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_RPAREN] = ACTIONS(94),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(96),
  },
  [6] = {
    [sym_line_item] = STATE(4),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(4),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_RPAREN] = ACTIONS(98),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(100),
  },
  [7] = {
    [sym_line_item] = STATE(4),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(4),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_RBRACK] = ACTIONS(102),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(100),
  },
  [8] = {
    [sym_line_item] = STATE(10),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(10),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_RBRACE] = ACTIONS(104),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(106),
  },
  [9] = {
    [sym_line_item] = STATE(7),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(7),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_RBRACK] = ACTIONS(108),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(110),
  },
  [10] = {
    [sym_line_item] = STATE(4),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_tuple_repeat1] = STATE(4),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_RBRACE] = ACTIONS(112),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(100),
  },
  [11] = {
    [sym_line_item] = STATE(11),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_line_items_repeat1] = STATE(11),
    [anon_sym_LPAREN] = ACTIONS(114),
    [anon_sym_LBRACK] = ACTIONS(117),
    [anon_sym_LBRACE] = ACTIONS(120),
    [anon_sym_def] = ACTIONS(123),
    [anon_sym_enum] = ACTIONS(123),
    [anon_sym_struct] = ACTIONS(123),
    [anon_sym_external] = ACTIONS(123),
    [anon_sym_if] = ACTIONS(123),
    [anon_sym_elif] = ACTIONS(123),
    [anon_sym_else] = ACTIONS(123),
    [anon_sym_match] = ACTIONS(123),
    [anon_sym_matches] = ACTIONS(123),
    [anon_sym_case] = ACTIONS(123),
    [anon_sym_recur] = ACTIONS(123),
    [anon_sym_loop] = ACTIONS(123),
    [anon_sym_from] = ACTIONS(123),
    [anon_sym_import] = ACTIONS(123),
    [anon_sym_export] = ACTIONS(123),
    [anon_sym_operator] = ACTIONS(123),
    [sym_identifier] = ACTIONS(126),
    [sym_type_identifier] = ACTIONS(126),
    [sym_backticked_identifier] = ACTIONS(129),
    [sym_number] = ACTIONS(129),
    [sym_string] = ACTIONS(129),
    [sym_character] = ACTIONS(129),
    [sym_operator] = ACTIONS(126),
    [sym_unknown] = ACTIONS(126),
    [anon_sym_COLON] = ACTIONS(132),
    [anon_sym_COMMA] = ACTIONS(135),
    [anon_sym_DOT] = ACTIONS(135),
    [anon_sym_SEMI] = ACTIONS(135),
    [anon_sym_EQ] = ACTIONS(135),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(138),
  },
  [12] = {
    [sym_line_item] = STATE(11),
    [sym_atom] = STATE(20),
    [sym_tuple] = STATE(21),
    [sym_list] = STATE(21),
    [sym_record] = STATE(21),
    [sym_keyword] = STATE(20),
    [sym_punctuation] = STATE(20),
    [aux_sym_line_items_repeat1] = STATE(11),
    [anon_sym_LPAREN] = ACTIONS(9),
    [anon_sym_LBRACK] = ACTIONS(11),
    [anon_sym_LBRACE] = ACTIONS(13),
    [anon_sym_def] = ACTIONS(15),
    [anon_sym_enum] = ACTIONS(15),
    [anon_sym_struct] = ACTIONS(15),
    [anon_sym_external] = ACTIONS(15),
    [anon_sym_if] = ACTIONS(15),
    [anon_sym_elif] = ACTIONS(15),
    [anon_sym_else] = ACTIONS(15),
    [anon_sym_match] = ACTIONS(15),
    [anon_sym_matches] = ACTIONS(15),
    [anon_sym_case] = ACTIONS(15),
    [anon_sym_recur] = ACTIONS(15),
    [anon_sym_loop] = ACTIONS(15),
    [anon_sym_from] = ACTIONS(15),
    [anon_sym_import] = ACTIONS(15),
    [anon_sym_export] = ACTIONS(15),
    [anon_sym_operator] = ACTIONS(15),
    [sym_identifier] = ACTIONS(17),
    [sym_type_identifier] = ACTIONS(17),
    [sym_backticked_identifier] = ACTIONS(19),
    [sym_number] = ACTIONS(19),
    [sym_string] = ACTIONS(19),
    [sym_character] = ACTIONS(19),
    [sym_operator] = ACTIONS(17),
    [sym_unknown] = ACTIONS(17),
    [anon_sym_COLON] = ACTIONS(21),
    [anon_sym_COMMA] = ACTIONS(23),
    [anon_sym_DOT] = ACTIONS(23),
    [anon_sym_SEMI] = ACTIONS(23),
    [anon_sym_EQ] = ACTIONS(23),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(140),
  },
  [13] = {
    [ts_builtin_sym_end] = ACTIONS(142),
    [anon_sym_package] = ACTIONS(144),
    [anon_sym_LPAREN] = ACTIONS(142),
    [anon_sym_LBRACK] = ACTIONS(142),
    [anon_sym_LBRACE] = ACTIONS(142),
    [anon_sym_def] = ACTIONS(144),
    [anon_sym_enum] = ACTIONS(144),
    [anon_sym_struct] = ACTIONS(144),
    [anon_sym_external] = ACTIONS(144),
    [anon_sym_if] = ACTIONS(144),
    [anon_sym_elif] = ACTIONS(144),
    [anon_sym_else] = ACTIONS(144),
    [anon_sym_match] = ACTIONS(144),
    [anon_sym_matches] = ACTIONS(144),
    [anon_sym_case] = ACTIONS(144),
    [anon_sym_recur] = ACTIONS(144),
    [anon_sym_loop] = ACTIONS(144),
    [anon_sym_from] = ACTIONS(144),
    [anon_sym_import] = ACTIONS(144),
    [anon_sym_export] = ACTIONS(144),
    [anon_sym_operator] = ACTIONS(144),
    [sym_identifier] = ACTIONS(144),
    [sym_type_identifier] = ACTIONS(144),
    [sym_backticked_identifier] = ACTIONS(144),
    [sym_number] = ACTIONS(144),
    [sym_string] = ACTIONS(144),
    [sym_character] = ACTIONS(144),
    [sym_operator] = ACTIONS(144),
    [sym_unknown] = ACTIONS(144),
    [anon_sym_COLON] = ACTIONS(142),
    [anon_sym_COMMA] = ACTIONS(144),
    [anon_sym_DOT] = ACTIONS(144),
    [anon_sym_SEMI] = ACTIONS(144),
    [anon_sym_EQ] = ACTIONS(144),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(142),
    [sym__indent] = ACTIONS(142),
    [sym__dedent] = ACTIONS(142),
  },
  [14] = {
    [ts_builtin_sym_end] = ACTIONS(146),
    [anon_sym_package] = ACTIONS(148),
    [anon_sym_LPAREN] = ACTIONS(146),
    [anon_sym_LBRACK] = ACTIONS(146),
    [anon_sym_LBRACE] = ACTIONS(146),
    [anon_sym_def] = ACTIONS(148),
    [anon_sym_enum] = ACTIONS(148),
    [anon_sym_struct] = ACTIONS(148),
    [anon_sym_external] = ACTIONS(148),
    [anon_sym_if] = ACTIONS(148),
    [anon_sym_elif] = ACTIONS(148),
    [anon_sym_else] = ACTIONS(148),
    [anon_sym_match] = ACTIONS(148),
    [anon_sym_matches] = ACTIONS(148),
    [anon_sym_case] = ACTIONS(148),
    [anon_sym_recur] = ACTIONS(148),
    [anon_sym_loop] = ACTIONS(148),
    [anon_sym_from] = ACTIONS(148),
    [anon_sym_import] = ACTIONS(148),
    [anon_sym_export] = ACTIONS(148),
    [anon_sym_operator] = ACTIONS(148),
    [sym_identifier] = ACTIONS(148),
    [sym_type_identifier] = ACTIONS(148),
    [sym_backticked_identifier] = ACTIONS(148),
    [sym_number] = ACTIONS(148),
    [sym_string] = ACTIONS(148),
    [sym_character] = ACTIONS(148),
    [sym_operator] = ACTIONS(148),
    [sym_unknown] = ACTIONS(148),
    [anon_sym_COLON] = ACTIONS(146),
    [anon_sym_COMMA] = ACTIONS(148),
    [anon_sym_DOT] = ACTIONS(148),
    [anon_sym_SEMI] = ACTIONS(148),
    [anon_sym_EQ] = ACTIONS(148),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(146),
    [sym__indent] = ACTIONS(146),
    [sym__dedent] = ACTIONS(146),
  },
  [15] = {
    [ts_builtin_sym_end] = ACTIONS(150),
    [anon_sym_package] = ACTIONS(152),
    [anon_sym_LPAREN] = ACTIONS(150),
    [anon_sym_LBRACK] = ACTIONS(150),
    [anon_sym_LBRACE] = ACTIONS(150),
    [anon_sym_def] = ACTIONS(152),
    [anon_sym_enum] = ACTIONS(152),
    [anon_sym_struct] = ACTIONS(152),
    [anon_sym_external] = ACTIONS(152),
    [anon_sym_if] = ACTIONS(152),
    [anon_sym_elif] = ACTIONS(152),
    [anon_sym_else] = ACTIONS(152),
    [anon_sym_match] = ACTIONS(152),
    [anon_sym_matches] = ACTIONS(152),
    [anon_sym_case] = ACTIONS(152),
    [anon_sym_recur] = ACTIONS(152),
    [anon_sym_loop] = ACTIONS(152),
    [anon_sym_from] = ACTIONS(152),
    [anon_sym_import] = ACTIONS(152),
    [anon_sym_export] = ACTIONS(152),
    [anon_sym_operator] = ACTIONS(152),
    [sym_identifier] = ACTIONS(152),
    [sym_type_identifier] = ACTIONS(152),
    [sym_backticked_identifier] = ACTIONS(152),
    [sym_number] = ACTIONS(152),
    [sym_string] = ACTIONS(152),
    [sym_character] = ACTIONS(152),
    [sym_operator] = ACTIONS(152),
    [sym_unknown] = ACTIONS(152),
    [anon_sym_COLON] = ACTIONS(150),
    [anon_sym_COMMA] = ACTIONS(152),
    [anon_sym_DOT] = ACTIONS(152),
    [anon_sym_SEMI] = ACTIONS(152),
    [anon_sym_EQ] = ACTIONS(152),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(150),
    [sym__indent] = ACTIONS(150),
    [sym__dedent] = ACTIONS(150),
  },
  [16] = {
    [anon_sym_LPAREN] = ACTIONS(154),
    [anon_sym_RPAREN] = ACTIONS(154),
    [anon_sym_LBRACK] = ACTIONS(154),
    [anon_sym_RBRACK] = ACTIONS(154),
    [anon_sym_LBRACE] = ACTIONS(154),
    [anon_sym_RBRACE] = ACTIONS(154),
    [anon_sym_def] = ACTIONS(156),
    [anon_sym_enum] = ACTIONS(156),
    [anon_sym_struct] = ACTIONS(156),
    [anon_sym_external] = ACTIONS(156),
    [anon_sym_if] = ACTIONS(156),
    [anon_sym_elif] = ACTIONS(156),
    [anon_sym_else] = ACTIONS(156),
    [anon_sym_match] = ACTIONS(156),
    [anon_sym_matches] = ACTIONS(156),
    [anon_sym_case] = ACTIONS(156),
    [anon_sym_recur] = ACTIONS(156),
    [anon_sym_loop] = ACTIONS(156),
    [anon_sym_from] = ACTIONS(156),
    [anon_sym_import] = ACTIONS(156),
    [anon_sym_export] = ACTIONS(156),
    [anon_sym_operator] = ACTIONS(156),
    [sym_identifier] = ACTIONS(156),
    [sym_type_identifier] = ACTIONS(156),
    [sym_backticked_identifier] = ACTIONS(156),
    [sym_number] = ACTIONS(156),
    [sym_string] = ACTIONS(156),
    [sym_character] = ACTIONS(156),
    [sym_operator] = ACTIONS(156),
    [sym_unknown] = ACTIONS(156),
    [anon_sym_COLON] = ACTIONS(154),
    [anon_sym_COMMA] = ACTIONS(156),
    [anon_sym_DOT] = ACTIONS(156),
    [anon_sym_SEMI] = ACTIONS(156),
    [anon_sym_EQ] = ACTIONS(156),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(154),
  },
  [17] = {
    [anon_sym_LPAREN] = ACTIONS(158),
    [anon_sym_RPAREN] = ACTIONS(158),
    [anon_sym_LBRACK] = ACTIONS(158),
    [anon_sym_RBRACK] = ACTIONS(158),
    [anon_sym_LBRACE] = ACTIONS(158),
    [anon_sym_RBRACE] = ACTIONS(158),
    [anon_sym_def] = ACTIONS(160),
    [anon_sym_enum] = ACTIONS(160),
    [anon_sym_struct] = ACTIONS(160),
    [anon_sym_external] = ACTIONS(160),
    [anon_sym_if] = ACTIONS(160),
    [anon_sym_elif] = ACTIONS(160),
    [anon_sym_else] = ACTIONS(160),
    [anon_sym_match] = ACTIONS(160),
    [anon_sym_matches] = ACTIONS(160),
    [anon_sym_case] = ACTIONS(160),
    [anon_sym_recur] = ACTIONS(160),
    [anon_sym_loop] = ACTIONS(160),
    [anon_sym_from] = ACTIONS(160),
    [anon_sym_import] = ACTIONS(160),
    [anon_sym_export] = ACTIONS(160),
    [anon_sym_operator] = ACTIONS(160),
    [sym_identifier] = ACTIONS(160),
    [sym_type_identifier] = ACTIONS(160),
    [sym_backticked_identifier] = ACTIONS(160),
    [sym_number] = ACTIONS(160),
    [sym_string] = ACTIONS(160),
    [sym_character] = ACTIONS(160),
    [sym_operator] = ACTIONS(160),
    [sym_unknown] = ACTIONS(160),
    [anon_sym_COLON] = ACTIONS(158),
    [anon_sym_COMMA] = ACTIONS(160),
    [anon_sym_DOT] = ACTIONS(160),
    [anon_sym_SEMI] = ACTIONS(160),
    [anon_sym_EQ] = ACTIONS(160),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(158),
  },
  [18] = {
    [anon_sym_LPAREN] = ACTIONS(162),
    [anon_sym_RPAREN] = ACTIONS(162),
    [anon_sym_LBRACK] = ACTIONS(162),
    [anon_sym_RBRACK] = ACTIONS(162),
    [anon_sym_LBRACE] = ACTIONS(162),
    [anon_sym_RBRACE] = ACTIONS(162),
    [anon_sym_def] = ACTIONS(164),
    [anon_sym_enum] = ACTIONS(164),
    [anon_sym_struct] = ACTIONS(164),
    [anon_sym_external] = ACTIONS(164),
    [anon_sym_if] = ACTIONS(164),
    [anon_sym_elif] = ACTIONS(164),
    [anon_sym_else] = ACTIONS(164),
    [anon_sym_match] = ACTIONS(164),
    [anon_sym_matches] = ACTIONS(164),
    [anon_sym_case] = ACTIONS(164),
    [anon_sym_recur] = ACTIONS(164),
    [anon_sym_loop] = ACTIONS(164),
    [anon_sym_from] = ACTIONS(164),
    [anon_sym_import] = ACTIONS(164),
    [anon_sym_export] = ACTIONS(164),
    [anon_sym_operator] = ACTIONS(164),
    [sym_identifier] = ACTIONS(164),
    [sym_type_identifier] = ACTIONS(164),
    [sym_backticked_identifier] = ACTIONS(164),
    [sym_number] = ACTIONS(164),
    [sym_string] = ACTIONS(164),
    [sym_character] = ACTIONS(164),
    [sym_operator] = ACTIONS(164),
    [sym_unknown] = ACTIONS(164),
    [anon_sym_COLON] = ACTIONS(162),
    [anon_sym_COMMA] = ACTIONS(164),
    [anon_sym_DOT] = ACTIONS(164),
    [anon_sym_SEMI] = ACTIONS(164),
    [anon_sym_EQ] = ACTIONS(164),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(162),
  },
  [19] = {
    [anon_sym_LPAREN] = ACTIONS(166),
    [anon_sym_RPAREN] = ACTIONS(166),
    [anon_sym_LBRACK] = ACTIONS(166),
    [anon_sym_RBRACK] = ACTIONS(166),
    [anon_sym_LBRACE] = ACTIONS(166),
    [anon_sym_RBRACE] = ACTIONS(166),
    [anon_sym_def] = ACTIONS(168),
    [anon_sym_enum] = ACTIONS(168),
    [anon_sym_struct] = ACTIONS(168),
    [anon_sym_external] = ACTIONS(168),
    [anon_sym_if] = ACTIONS(168),
    [anon_sym_elif] = ACTIONS(168),
    [anon_sym_else] = ACTIONS(168),
    [anon_sym_match] = ACTIONS(168),
    [anon_sym_matches] = ACTIONS(168),
    [anon_sym_case] = ACTIONS(168),
    [anon_sym_recur] = ACTIONS(168),
    [anon_sym_loop] = ACTIONS(168),
    [anon_sym_from] = ACTIONS(168),
    [anon_sym_import] = ACTIONS(168),
    [anon_sym_export] = ACTIONS(168),
    [anon_sym_operator] = ACTIONS(168),
    [sym_identifier] = ACTIONS(168),
    [sym_type_identifier] = ACTIONS(168),
    [sym_backticked_identifier] = ACTIONS(168),
    [sym_number] = ACTIONS(168),
    [sym_string] = ACTIONS(168),
    [sym_character] = ACTIONS(168),
    [sym_operator] = ACTIONS(168),
    [sym_unknown] = ACTIONS(168),
    [anon_sym_COLON] = ACTIONS(166),
    [anon_sym_COMMA] = ACTIONS(168),
    [anon_sym_DOT] = ACTIONS(168),
    [anon_sym_SEMI] = ACTIONS(168),
    [anon_sym_EQ] = ACTIONS(168),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(166),
  },
  [20] = {
    [anon_sym_LPAREN] = ACTIONS(170),
    [anon_sym_RPAREN] = ACTIONS(170),
    [anon_sym_LBRACK] = ACTIONS(170),
    [anon_sym_RBRACK] = ACTIONS(170),
    [anon_sym_LBRACE] = ACTIONS(170),
    [anon_sym_RBRACE] = ACTIONS(170),
    [anon_sym_def] = ACTIONS(172),
    [anon_sym_enum] = ACTIONS(172),
    [anon_sym_struct] = ACTIONS(172),
    [anon_sym_external] = ACTIONS(172),
    [anon_sym_if] = ACTIONS(172),
    [anon_sym_elif] = ACTIONS(172),
    [anon_sym_else] = ACTIONS(172),
    [anon_sym_match] = ACTIONS(172),
    [anon_sym_matches] = ACTIONS(172),
    [anon_sym_case] = ACTIONS(172),
    [anon_sym_recur] = ACTIONS(172),
    [anon_sym_loop] = ACTIONS(172),
    [anon_sym_from] = ACTIONS(172),
    [anon_sym_import] = ACTIONS(172),
    [anon_sym_export] = ACTIONS(172),
    [anon_sym_operator] = ACTIONS(172),
    [sym_identifier] = ACTIONS(172),
    [sym_type_identifier] = ACTIONS(172),
    [sym_backticked_identifier] = ACTIONS(172),
    [sym_number] = ACTIONS(172),
    [sym_string] = ACTIONS(172),
    [sym_character] = ACTIONS(172),
    [sym_operator] = ACTIONS(172),
    [sym_unknown] = ACTIONS(172),
    [anon_sym_COLON] = ACTIONS(170),
    [anon_sym_COMMA] = ACTIONS(172),
    [anon_sym_DOT] = ACTIONS(172),
    [anon_sym_SEMI] = ACTIONS(172),
    [anon_sym_EQ] = ACTIONS(172),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(170),
  },
  [21] = {
    [anon_sym_LPAREN] = ACTIONS(174),
    [anon_sym_RPAREN] = ACTIONS(174),
    [anon_sym_LBRACK] = ACTIONS(174),
    [anon_sym_RBRACK] = ACTIONS(174),
    [anon_sym_LBRACE] = ACTIONS(174),
    [anon_sym_RBRACE] = ACTIONS(174),
    [anon_sym_def] = ACTIONS(176),
    [anon_sym_enum] = ACTIONS(176),
    [anon_sym_struct] = ACTIONS(176),
    [anon_sym_external] = ACTIONS(176),
    [anon_sym_if] = ACTIONS(176),
    [anon_sym_elif] = ACTIONS(176),
    [anon_sym_else] = ACTIONS(176),
    [anon_sym_match] = ACTIONS(176),
    [anon_sym_matches] = ACTIONS(176),
    [anon_sym_case] = ACTIONS(176),
    [anon_sym_recur] = ACTIONS(176),
    [anon_sym_loop] = ACTIONS(176),
    [anon_sym_from] = ACTIONS(176),
    [anon_sym_import] = ACTIONS(176),
    [anon_sym_export] = ACTIONS(176),
    [anon_sym_operator] = ACTIONS(176),
    [sym_identifier] = ACTIONS(176),
    [sym_type_identifier] = ACTIONS(176),
    [sym_backticked_identifier] = ACTIONS(176),
    [sym_number] = ACTIONS(176),
    [sym_string] = ACTIONS(176),
    [sym_character] = ACTIONS(176),
    [sym_operator] = ACTIONS(176),
    [sym_unknown] = ACTIONS(176),
    [anon_sym_COLON] = ACTIONS(174),
    [anon_sym_COMMA] = ACTIONS(176),
    [anon_sym_DOT] = ACTIONS(176),
    [anon_sym_SEMI] = ACTIONS(176),
    [anon_sym_EQ] = ACTIONS(176),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(174),
  },
  [22] = {
    [anon_sym_LPAREN] = ACTIONS(178),
    [anon_sym_RPAREN] = ACTIONS(178),
    [anon_sym_LBRACK] = ACTIONS(178),
    [anon_sym_RBRACK] = ACTIONS(178),
    [anon_sym_LBRACE] = ACTIONS(178),
    [anon_sym_RBRACE] = ACTIONS(178),
    [anon_sym_def] = ACTIONS(180),
    [anon_sym_enum] = ACTIONS(180),
    [anon_sym_struct] = ACTIONS(180),
    [anon_sym_external] = ACTIONS(180),
    [anon_sym_if] = ACTIONS(180),
    [anon_sym_elif] = ACTIONS(180),
    [anon_sym_else] = ACTIONS(180),
    [anon_sym_match] = ACTIONS(180),
    [anon_sym_matches] = ACTIONS(180),
    [anon_sym_case] = ACTIONS(180),
    [anon_sym_recur] = ACTIONS(180),
    [anon_sym_loop] = ACTIONS(180),
    [anon_sym_from] = ACTIONS(180),
    [anon_sym_import] = ACTIONS(180),
    [anon_sym_export] = ACTIONS(180),
    [anon_sym_operator] = ACTIONS(180),
    [sym_identifier] = ACTIONS(180),
    [sym_type_identifier] = ACTIONS(180),
    [sym_backticked_identifier] = ACTIONS(180),
    [sym_number] = ACTIONS(180),
    [sym_string] = ACTIONS(180),
    [sym_character] = ACTIONS(180),
    [sym_operator] = ACTIONS(180),
    [sym_unknown] = ACTIONS(180),
    [anon_sym_COLON] = ACTIONS(178),
    [anon_sym_COMMA] = ACTIONS(180),
    [anon_sym_DOT] = ACTIONS(180),
    [anon_sym_SEMI] = ACTIONS(180),
    [anon_sym_EQ] = ACTIONS(180),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(178),
  },
  [23] = {
    [anon_sym_LPAREN] = ACTIONS(182),
    [anon_sym_RPAREN] = ACTIONS(182),
    [anon_sym_LBRACK] = ACTIONS(182),
    [anon_sym_RBRACK] = ACTIONS(182),
    [anon_sym_LBRACE] = ACTIONS(182),
    [anon_sym_RBRACE] = ACTIONS(182),
    [anon_sym_def] = ACTIONS(184),
    [anon_sym_enum] = ACTIONS(184),
    [anon_sym_struct] = ACTIONS(184),
    [anon_sym_external] = ACTIONS(184),
    [anon_sym_if] = ACTIONS(184),
    [anon_sym_elif] = ACTIONS(184),
    [anon_sym_else] = ACTIONS(184),
    [anon_sym_match] = ACTIONS(184),
    [anon_sym_matches] = ACTIONS(184),
    [anon_sym_case] = ACTIONS(184),
    [anon_sym_recur] = ACTIONS(184),
    [anon_sym_loop] = ACTIONS(184),
    [anon_sym_from] = ACTIONS(184),
    [anon_sym_import] = ACTIONS(184),
    [anon_sym_export] = ACTIONS(184),
    [anon_sym_operator] = ACTIONS(184),
    [sym_identifier] = ACTIONS(184),
    [sym_type_identifier] = ACTIONS(184),
    [sym_backticked_identifier] = ACTIONS(184),
    [sym_number] = ACTIONS(184),
    [sym_string] = ACTIONS(184),
    [sym_character] = ACTIONS(184),
    [sym_operator] = ACTIONS(184),
    [sym_unknown] = ACTIONS(184),
    [anon_sym_COLON] = ACTIONS(182),
    [anon_sym_COMMA] = ACTIONS(184),
    [anon_sym_DOT] = ACTIONS(184),
    [anon_sym_SEMI] = ACTIONS(184),
    [anon_sym_EQ] = ACTIONS(184),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(182),
  },
  [24] = {
    [anon_sym_LPAREN] = ACTIONS(186),
    [anon_sym_RPAREN] = ACTIONS(186),
    [anon_sym_LBRACK] = ACTIONS(186),
    [anon_sym_RBRACK] = ACTIONS(186),
    [anon_sym_LBRACE] = ACTIONS(186),
    [anon_sym_RBRACE] = ACTIONS(186),
    [anon_sym_def] = ACTIONS(188),
    [anon_sym_enum] = ACTIONS(188),
    [anon_sym_struct] = ACTIONS(188),
    [anon_sym_external] = ACTIONS(188),
    [anon_sym_if] = ACTIONS(188),
    [anon_sym_elif] = ACTIONS(188),
    [anon_sym_else] = ACTIONS(188),
    [anon_sym_match] = ACTIONS(188),
    [anon_sym_matches] = ACTIONS(188),
    [anon_sym_case] = ACTIONS(188),
    [anon_sym_recur] = ACTIONS(188),
    [anon_sym_loop] = ACTIONS(188),
    [anon_sym_from] = ACTIONS(188),
    [anon_sym_import] = ACTIONS(188),
    [anon_sym_export] = ACTIONS(188),
    [anon_sym_operator] = ACTIONS(188),
    [sym_identifier] = ACTIONS(188),
    [sym_type_identifier] = ACTIONS(188),
    [sym_backticked_identifier] = ACTIONS(188),
    [sym_number] = ACTIONS(188),
    [sym_string] = ACTIONS(188),
    [sym_character] = ACTIONS(188),
    [sym_operator] = ACTIONS(188),
    [sym_unknown] = ACTIONS(188),
    [anon_sym_COLON] = ACTIONS(186),
    [anon_sym_COMMA] = ACTIONS(188),
    [anon_sym_DOT] = ACTIONS(188),
    [anon_sym_SEMI] = ACTIONS(188),
    [anon_sym_EQ] = ACTIONS(188),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(186),
  },
  [25] = {
    [anon_sym_LPAREN] = ACTIONS(190),
    [anon_sym_RPAREN] = ACTIONS(190),
    [anon_sym_LBRACK] = ACTIONS(190),
    [anon_sym_RBRACK] = ACTIONS(190),
    [anon_sym_LBRACE] = ACTIONS(190),
    [anon_sym_RBRACE] = ACTIONS(190),
    [anon_sym_def] = ACTIONS(192),
    [anon_sym_enum] = ACTIONS(192),
    [anon_sym_struct] = ACTIONS(192),
    [anon_sym_external] = ACTIONS(192),
    [anon_sym_if] = ACTIONS(192),
    [anon_sym_elif] = ACTIONS(192),
    [anon_sym_else] = ACTIONS(192),
    [anon_sym_match] = ACTIONS(192),
    [anon_sym_matches] = ACTIONS(192),
    [anon_sym_case] = ACTIONS(192),
    [anon_sym_recur] = ACTIONS(192),
    [anon_sym_loop] = ACTIONS(192),
    [anon_sym_from] = ACTIONS(192),
    [anon_sym_import] = ACTIONS(192),
    [anon_sym_export] = ACTIONS(192),
    [anon_sym_operator] = ACTIONS(192),
    [sym_identifier] = ACTIONS(192),
    [sym_type_identifier] = ACTIONS(192),
    [sym_backticked_identifier] = ACTIONS(192),
    [sym_number] = ACTIONS(192),
    [sym_string] = ACTIONS(192),
    [sym_character] = ACTIONS(192),
    [sym_operator] = ACTIONS(192),
    [sym_unknown] = ACTIONS(192),
    [anon_sym_COLON] = ACTIONS(190),
    [anon_sym_COMMA] = ACTIONS(192),
    [anon_sym_DOT] = ACTIONS(192),
    [anon_sym_SEMI] = ACTIONS(192),
    [anon_sym_EQ] = ACTIONS(192),
    [sym_comment] = ACTIONS(25),
    [sym__newline] = ACTIONS(190),
  },
};

static const uint16_t ts_small_parse_table[] = {
  [0] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(194), 1,
      anon_sym_SLASH,
    ACTIONS(196), 1,
      sym__newline,
    STATE(27), 1,
      aux_sym_package_name_repeat1,
  [13] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(194), 1,
      anon_sym_SLASH,
    ACTIONS(198), 1,
      sym__newline,
    STATE(28), 1,
      aux_sym_package_name_repeat1,
  [26] = 4,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(200), 1,
      anon_sym_SLASH,
    ACTIONS(203), 1,
      sym__newline,
    STATE(28), 1,
      aux_sym_package_name_repeat1,
  [39] = 3,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(205), 1,
      sym_package_segment,
    STATE(31), 1,
      sym_package_name,
  [49] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(203), 2,
      sym__newline,
      anon_sym_SLASH,
  [57] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(207), 1,
      sym__newline,
  [64] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(209), 1,
      ts_builtin_sym_end,
  [71] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(211), 1,
      sym_package_segment,
  [78] = 2,
    ACTIONS(3), 1,
      sym_comment,
    ACTIONS(213), 1,
      sym__newline,
};

static const uint32_t ts_small_parse_table_map[] = {
  [SMALL_STATE(26)] = 0,
  [SMALL_STATE(27)] = 13,
  [SMALL_STATE(28)] = 26,
  [SMALL_STATE(29)] = 39,
  [SMALL_STATE(30)] = 49,
  [SMALL_STATE(31)] = 57,
  [SMALL_STATE(32)] = 64,
  [SMALL_STATE(33)] = 71,
  [SMALL_STATE(34)] = 78,
};

static const TSParseActionEntry ts_parse_actions[] = {
  [0] = {.entry = {.count = 0, .reusable = false}},
  [1] = {.entry = {.count = 1, .reusable = false}}, RECOVER(),
  [3] = {.entry = {.count = 1, .reusable = true}}, SHIFT_EXTRA(),
  [5] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 0, 0, 0),
  [7] = {.entry = {.count = 1, .reusable = false}}, SHIFT(29),
  [9] = {.entry = {.count = 1, .reusable = true}}, SHIFT(5),
  [11] = {.entry = {.count = 1, .reusable = true}}, SHIFT(9),
  [13] = {.entry = {.count = 1, .reusable = true}}, SHIFT(8),
  [15] = {.entry = {.count = 1, .reusable = false}}, SHIFT(16),
  [17] = {.entry = {.count = 1, .reusable = false}}, SHIFT(20),
  [19] = {.entry = {.count = 1, .reusable = false}}, SHIFT(21),
  [21] = {.entry = {.count = 1, .reusable = true}}, SHIFT(22),
  [23] = {.entry = {.count = 1, .reusable = false}}, SHIFT(22),
  [25] = {.entry = {.count = 1, .reusable = false}}, SHIFT_EXTRA(),
  [27] = {.entry = {.count = 1, .reusable = true}}, SHIFT(3),
  [29] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0),
  [31] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(29),
  [34] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [37] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(9),
  [40] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(8),
  [43] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(16),
  [46] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(20),
  [49] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(21),
  [52] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [55] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [58] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_source_file_repeat1, 2, 0, 0), SHIFT_REPEAT(2),
  [61] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_source_file, 1, 0, 0),
  [63] = {.entry = {.count = 1, .reusable = true}}, SHIFT(2),
  [65] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [68] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0),
  [70] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(9),
  [73] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(8),
  [76] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(16),
  [79] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(20),
  [82] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(21),
  [85] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [88] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [91] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_tuple_repeat1, 2, 0, 0), SHIFT_REPEAT(4),
  [94] = {.entry = {.count = 1, .reusable = true}}, SHIFT(19),
  [96] = {.entry = {.count = 1, .reusable = true}}, SHIFT(6),
  [98] = {.entry = {.count = 1, .reusable = true}}, SHIFT(24),
  [100] = {.entry = {.count = 1, .reusable = true}}, SHIFT(4),
  [102] = {.entry = {.count = 1, .reusable = true}}, SHIFT(23),
  [104] = {.entry = {.count = 1, .reusable = true}}, SHIFT(17),
  [106] = {.entry = {.count = 1, .reusable = true}}, SHIFT(10),
  [108] = {.entry = {.count = 1, .reusable = true}}, SHIFT(18),
  [110] = {.entry = {.count = 1, .reusable = true}}, SHIFT(7),
  [112] = {.entry = {.count = 1, .reusable = true}}, SHIFT(25),
  [114] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(5),
  [117] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(9),
  [120] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(8),
  [123] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(16),
  [126] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(20),
  [129] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(21),
  [132] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [135] = {.entry = {.count = 2, .reusable = false}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0), SHIFT_REPEAT(22),
  [138] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_line_items_repeat1, 2, 0, 0),
  [140] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_line_items, 1, 0, 0),
  [142] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_package_declaration, 3, 0, 1),
  [144] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_package_declaration, 3, 0, 1),
  [146] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_simple_statement, 2, 0, 0),
  [148] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_simple_statement, 2, 0, 0),
  [150] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_statement, 1, 0, 0),
  [152] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_statement, 1, 0, 0),
  [154] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_keyword, 1, 0, 0),
  [156] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_keyword, 1, 0, 0),
  [158] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_record, 2, 0, 0),
  [160] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_record, 2, 0, 0),
  [162] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 2, 0, 0),
  [164] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 2, 0, 0),
  [166] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple, 2, 0, 0),
  [168] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_tuple, 2, 0, 0),
  [170] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_line_item, 1, 0, 0),
  [172] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_line_item, 1, 0, 0),
  [174] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_atom, 1, 0, 0),
  [176] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_atom, 1, 0, 0),
  [178] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_punctuation, 1, 0, 0),
  [180] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_punctuation, 1, 0, 0),
  [182] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_list, 3, 0, 0),
  [184] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_list, 3, 0, 0),
  [186] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_tuple, 3, 0, 0),
  [188] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_tuple, 3, 0, 0),
  [190] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_record, 3, 0, 0),
  [192] = {.entry = {.count = 1, .reusable = false}}, REDUCE(sym_record, 3, 0, 0),
  [194] = {.entry = {.count = 1, .reusable = true}}, SHIFT(33),
  [196] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_package_name, 1, 0, 0),
  [198] = {.entry = {.count = 1, .reusable = true}}, REDUCE(sym_package_name, 2, 0, 0),
  [200] = {.entry = {.count = 2, .reusable = true}}, REDUCE(aux_sym_package_name_repeat1, 2, 0, 0), SHIFT_REPEAT(33),
  [203] = {.entry = {.count = 1, .reusable = true}}, REDUCE(aux_sym_package_name_repeat1, 2, 0, 0),
  [205] = {.entry = {.count = 1, .reusable = true}}, SHIFT(26),
  [207] = {.entry = {.count = 1, .reusable = true}}, SHIFT(13),
  [209] = {.entry = {.count = 1, .reusable = true}},  ACCEPT_INPUT(),
  [211] = {.entry = {.count = 1, .reusable = true}}, SHIFT(30),
  [213] = {.entry = {.count = 1, .reusable = true}}, SHIFT(14),
};

enum ts_external_scanner_symbol_identifiers {
  ts_external_token__newline = 0,
  ts_external_token__indent = 1,
  ts_external_token__dedent = 2,
};

static const TSSymbol ts_external_scanner_symbol_map[EXTERNAL_TOKEN_COUNT] = {
  [ts_external_token__newline] = sym__newline,
  [ts_external_token__indent] = sym__indent,
  [ts_external_token__dedent] = sym__dedent,
};

static const bool ts_external_scanner_states[3][EXTERNAL_TOKEN_COUNT] = {
  [1] = {
    [ts_external_token__newline] = true,
    [ts_external_token__indent] = true,
    [ts_external_token__dedent] = true,
  },
  [2] = {
    [ts_external_token__newline] = true,
  },
};

#ifdef __cplusplus
extern "C" {
#endif
void *tree_sitter_bosatsu_external_scanner_create(void);
void tree_sitter_bosatsu_external_scanner_destroy(void *);
bool tree_sitter_bosatsu_external_scanner_scan(void *, TSLexer *, const bool *);
unsigned tree_sitter_bosatsu_external_scanner_serialize(void *, char *);
void tree_sitter_bosatsu_external_scanner_deserialize(void *, const char *, unsigned);

#ifdef TREE_SITTER_HIDE_SYMBOLS
#define TS_PUBLIC
#elif defined(_WIN32)
#define TS_PUBLIC __declspec(dllexport)
#else
#define TS_PUBLIC __attribute__((visibility("default")))
#endif

TS_PUBLIC const TSLanguage *tree_sitter_bosatsu(void) {
  static const TSLanguage language = {
    .version = LANGUAGE_VERSION,
    .symbol_count = SYMBOL_COUNT,
    .alias_count = ALIAS_COUNT,
    .token_count = TOKEN_COUNT,
    .external_token_count = EXTERNAL_TOKEN_COUNT,
    .state_count = STATE_COUNT,
    .large_state_count = LARGE_STATE_COUNT,
    .production_id_count = PRODUCTION_ID_COUNT,
    .field_count = FIELD_COUNT,
    .max_alias_sequence_length = MAX_ALIAS_SEQUENCE_LENGTH,
    .parse_table = &ts_parse_table[0][0],
    .small_parse_table = ts_small_parse_table,
    .small_parse_table_map = ts_small_parse_table_map,
    .parse_actions = ts_parse_actions,
    .symbol_names = ts_symbol_names,
    .field_names = ts_field_names,
    .field_map_slices = ts_field_map_slices,
    .field_map_entries = ts_field_map_entries,
    .symbol_metadata = ts_symbol_metadata,
    .public_symbol_map = ts_symbol_map,
    .alias_map = ts_non_terminal_alias_map,
    .alias_sequences = &ts_alias_sequences[0][0],
    .lex_modes = ts_lex_modes,
    .lex_fn = ts_lex,
    .external_scanner = {
      &ts_external_scanner_states[0][0],
      ts_external_scanner_symbol_map,
      tree_sitter_bosatsu_external_scanner_create,
      tree_sitter_bosatsu_external_scanner_destroy,
      tree_sitter_bosatsu_external_scanner_scan,
      tree_sitter_bosatsu_external_scanner_serialize,
      tree_sitter_bosatsu_external_scanner_deserialize,
    },
    .primary_state_ids = ts_primary_state_ids,
  };
  return &language;
}
#ifdef __cplusplus
}
#endif
