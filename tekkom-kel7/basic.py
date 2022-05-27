#######################################
# IMPORTS
#######################################

from strings_with_arrows import *

import string
import os
import math

#######################################
# CONSTANTS
#######################################

ANGKA = '0123456789'
HURUFs = string.ascii_letters
HURUFs_ANGKA = HURUFs + ANGKA

#######################################
# ERRORS
#######################################
# pos -> position
class Error:
  def __init__(self, pos_mulai, pos_akhir, nama_error, detail):
    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir
    self.nama_error = nama_error
    self.detail = detail
  
  def as_string(self):
    hasil  = f'{self.nama_error}: {self.detail}\n'
    hasil += f'File {self.pos_mulai.nama_file}, line {self.pos_mulai.baris + 1}'
    hasil += '\n\n' + string_with_arrows(self.pos_mulai.ftxt, self.pos_mulai, self.pos_akhir)
    return hasil

class IllegalCharError(Error):
  def __init__(self, pos_mulai, pos_akhir, detail):
    super().__init__(pos_mulai, pos_akhir, 'Karakter Ilegal', detail)

class ExpectedCharError(Error):
  def __init__(self, pos_mulai, pos_akhir, detail):
    super().__init__(pos_mulai, pos_akhir, 'Karakter yang Diharapkan', detail)

class InvalidSyntaxError(Error):
  def __init__(self, pos_mulai, pos_akhir, detail=''):
    super().__init__(pos_mulai, pos_akhir, 'Syntax Tidak Sesuai', detail)

class RTError(Error):
  def __init__(self, pos_mulai, pos_akhir, detail, isi):
    super().__init__(pos_mulai, pos_akhir, 'Runtime Error', detail)
    self.isi = isi

  def as_string(self):
    hasil  = self.generate_traceback()
    hasil += f'{self.nama_error}: {self.detail}'
    hasil += '\n\n' + string_with_arrows(self.pos_mulai.ftxt, self.pos_mulai, self.pos_akhir)
    return hasil

  def generate_traceback(self):
    hasil = ''
    pos = self.pos_mulai
    isi = self.isi

    while isi:
      hasil = f'  File {pos.nama_file}, baris {str(pos.baris + 1)}, pada {isi.display_name}\n' + hasil
      pos = isi.parent_entry_pos
      isi = isi.parent

    return 'Traceback (most recent call last):\n' + hasil

#######################################
# POSITION
#######################################

class Position:
  def __init__(self, index, baris, kolom, nama_file, ftxt):
    self.index = index
    self.baris = baris
    self.kolom = kolom
    self.nama_file = nama_file
    self.ftxt = ftxt

  def advance(self, current_char=None):
    self.index += 1
    self.kolom += 1

    if current_char == '\n':
      self.baris += 1
      self.kolom = 0

    return self

  def copy(self):
    return Position(self.index, self.baris, self.kolom, self.nama_file, self.ftxt)

#######################################
# TOKENS
#######################################

TT_INT				= 'INT'
TT_FLOAT    	= 'FLOAT'
TT_STRING			= 'STRING'
TT_IDENTIFIER	= 'IDENTIFIER'
TT_KEYWORD		= 'KEYWORD'
TT_PLUS     	= 'PLUS'
TT_MINUS    	= 'MINUS'
TT_MUL      	= 'MUL'
TT_DIV      	= 'DIV'
TT_POW				= 'POW'
TT_EQ					= 'EQ' # =
TT_LPAREN   	= 'LPAREN'
TT_RPAREN   	= 'RPAREN'
TT_LSQUARE    = 'LSQUARE'
TT_RSQUARE    = 'RSQUARE'
TT_EE					= 'EE' # ==
TT_NE					= 'NE' # !=
TT_LT					= 'LT' # <
TT_GT					= 'GT' # >
TT_LTE				= 'LTE' # <=
TT_GTE				= 'GTE' # >=
TT_COMMA			= 'COMMA'
TT_ARROW			= 'ARROW'  # ->
TT_NEWLINE		= 'NEWLINE'
TT_EOF				= 'EOF' # end of file

KEYWORDS = [
  'var', # VAR
  'dan', # AND
  'atau',
  'tidak',
  'jika',
  'lain_jika',
  'lain',
  'untuk', # FOR
  'sampai', # TO
  'langkah', # STEP
  'fungsi', #FUN
  'maka', # THEN
  'selesai', # END
  'kembalikan', # RETURN
  'lanjut', # CONTINUE
  'berhenti', # BREAK
  'sementara', # WHILE
]

class Token:
  def __init__(self, type_, value=None, pos_mulai=None, pos_akhir=None):
    self.type = type_
    self.value = value

    if pos_mulai:
      self.pos_mulai = pos_mulai.copy()
      self.pos_akhir = pos_mulai.copy()
      self.pos_akhir.advance()

    if pos_akhir:
      self.pos_akhir = pos_akhir.copy()

  def matches(self, type_, value):
    return self.type == type_ and self.value == value
  
  def __repr__(self):
    if self.value: return f'{self.type}:{self.value}'
    return f'{self.type}'

#######################################
# LEXER
#######################################

class Lexer:
  def __init__(self, nama_file, text):
    self.nama_file = nama_file
    self.text = text
    self.pos = Position(-1, 0, -1, nama_file, text)
    self.current_char = None
    self.advance()
  
  def advance(self):
    self.pos.advance(self.current_char)
    self.current_char = self.text[self.pos.index] if self.pos.index < len(self.text) else None

  def membuat_token(self):
    tokens = []

    while self.current_char != None:
      if self.current_char in ' \t':
        self.advance()
      elif self.current_char == '#':
        self.skip_comment()
      elif self.current_char in ';\n':
        tokens.append(Token(TT_NEWLINE, pos_mulai=self.pos))
        self.advance()
      elif self.current_char in ANGKA:
        tokens.append(self.membuat_angka()) # make_number()
      elif self.current_char in HURUFs:
        tokens.append(self.membuat_identifier()) # make_identifier()
      elif self.current_char == '"':
        tokens.append(self.membuat_string()) # make_string()
      elif self.current_char == '+':
        tokens.append(Token(TT_PLUS, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '-':
        tokens.append(self.membuat_minus()) # make_minus_or_arrow()
      elif self.current_char == '*':
        tokens.append(Token(TT_MUL, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '/':
        tokens.append(Token(TT_DIV, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '^':
        tokens.append(Token(TT_POW, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '(':
        tokens.append(Token(TT_LPAREN, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == ')':
        tokens.append(Token(TT_RPAREN, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '[':
        tokens.append(Token(TT_LSQUARE, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == ']':
        tokens.append(Token(TT_RSQUARE, pos_mulai=self.pos))
        self.advance()
      elif self.current_char == '!':
        token, error = self.membuat_tidak_sama() # make_not_equals()
        if error: return [], error
        tokens.append(token)
      elif self.current_char == '=':
        tokens.append(self.membuat_sama()) # make_equals()
      elif self.current_char == '<':
        tokens.append(self.membuat_kurang_dari()) # make_less_than()
      elif self.current_char == '>':
        tokens.append(self.membuat_lebih_dari()) # make_greater_than()
      elif self.current_char == ',':
        tokens.append(Token(TT_COMMA, pos_mulai=self.pos))
        self.advance()
      else:
        pos_mulai = self.pos.copy()
        char = self.current_char
        self.advance()
        return [], IllegalCharError(pos_mulai, self.pos, "'" + char + "'")

    tokens.append(Token(TT_EOF, pos_mulai=self.pos))
    return tokens, None

  def membuat_angka(self):
    angka_string = ''
    cari_titik = 0
    pos_mulai = self.pos.copy()

    while self.current_char != None and self.current_char in ANGKA + '.':
      if self.current_char == '.':
        if cari_titik == 1: break
        cari_titik += 1
      angka_string += self.current_char
      self.advance()

    if cari_titik == 0:
      return Token(TT_INT, int(angka_string), pos_mulai, self.pos)
    else:
      return Token(TT_FLOAT, float(angka_string), pos_mulai, self.pos)

  def membuat_string(self):
    string = ''
    pos_mulai = self.pos.copy()
    escape_character = False
    self.advance()

    escape_characters = {
      'n': '\n',
      't': '\t'
    }

    while self.current_char != None and (self.current_char != '"' or escape_character):
      if escape_character:
        string += escape_characters.get(self.current_char, self.current_char)
      else:
        if self.current_char == '\\':
          escape_character = True
        else:
          string += self.current_char
      self.advance()
      escape_character = False
    
    self.advance()
    return Token(TT_STRING, string, pos_mulai, self.pos)

  def membuat_identifier(self):
    identifier_string = ''
    pos_mulai = self.pos.copy()

    while self.current_char != None and self.current_char in HURUFs_ANGKA + '_':
      identifier_string += self.current_char
      self.advance()

    tipe_token = TT_KEYWORD if identifier_string in KEYWORDS else TT_IDENTIFIER
    return Token(tipe_token, identifier_string, pos_mulai, self.pos)

  def membuat_minus(self):
    tipe_token = TT_MINUS
    pos_mulai = self.pos.copy()
    self.advance()

    if self.current_char == '>':
      self.advance()
      tipe_token = TT_ARROW

    return Token(tipe_token, pos_mulai=pos_mulai, pos_akhir=self.pos)

  def membuat_tidak_sama(self):
    pos_mulai = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      return Token(TT_NE, pos_mulai=pos_mulai, pos_akhir=self.pos), None

    self.advance()
    return None, ExpectedCharError(pos_mulai, self.pos, "'=' (after '!')")
  
  def membuat_sama(self):
    tipe_token = TT_EQ
    pos_mulai = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tipe_token = TT_EE

    return Token(tipe_token, pos_mulai=pos_mulai, pos_akhir=self.pos)

  def membuat_kurang_dari(self):
    tipe_token = TT_LT
    pos_mulai = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tipe_token = TT_LTE

    return Token(tipe_token, pos_mulai=pos_mulai, pos_akhir=self.pos)

  def membuat_lebih_dari(self):
    tipe_token = TT_GT
    pos_mulai = self.pos.copy()
    self.advance()

    if self.current_char == '=':
      self.advance()
      tipe_token = TT_GTE

    return Token(tipe_token, pos_mulai=pos_mulai, pos_akhir=self.pos)

  def skip_comment(self):
    self.advance()

    while self.current_char != '\n':
      self.advance()

    self.advance()

#######################################
# PARSER
#######################################

class Parser:
  def __init__(self, tokens):
    self.tokens = tokens
    self.token_index = -1
    self.advance()

  def advance(self):
    self.token_index += 1
    self.update_current_token() 
    return self.current_token

  def reverse(self, jumlah=1):
    self.token_index -= jumlah
    self.update_current_token()
    return self.current_token

  def update_current_token(self):
    if self.token_index >= 0 and self.token_index < len(self.tokens):
      self.current_token = self.tokens[self.token_index]

  def parse(self):
    hasil = self.statements()
    if not hasil.error and self.current_token.type != TT_EOF:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        "Token tidak muncul setelah token sebelumnya"
      ))
    return hasil

  ###################################

  def statements(self):
    hasil = ParseResult()
    statements = []
    pos_mulai = self.current_token.pos_mulai.copy()

    while self.current_token.type == TT_NEWLINE:
      hasil.register_advancement()
      self.advance()

    statement = hasil.register(self.statement())
    if hasil.error: return hasil
    statements.append(statement)

    more_statements = True

    while True:
      banyak_baris_baru = 0
      while self.current_token.type == TT_NEWLINE:
        hasil.register_advancement()
        self.advance()
        banyak_baris_baru += 1
      if banyak_baris_baru == 0:
        more_statements = False
      
      if not more_statements: break
      statement = hasil.try_register(self.statement())
      if not statement:
        self.reverse(hasil.to_reverse_count)
        more_statements = False
        continue
      statements.append(statement)

    return hasil.success(ListNode(
      statements,
      pos_mulai,
      self.current_token.pos_akhir.copy()
    ))

  def statement(self):
    hasil = ParseResult()
    pos_mulai = self.current_token.pos_mulai.copy()

    if self.current_token.matches(TT_KEYWORD, 'kembalikan'):
      hasil.register_advancement()
      self.advance()

      expression = hasil.try_register(self.expression())
      if not expression:
        self.reverse(hasil.to_reverse_count)
      return hasil.success(ReturnNode(expression, pos_mulai, self.current_token.pos_mulai.copy()))
    
    if self.current_token.matches(TT_KEYWORD, 'lanjut'):
      hasil.register_advancement()
      self.advance()
      return hasil.success(ContinueNode(pos_mulai, self.current_token.pos_mulai.copy()))
      
    if self.current_token.matches(TT_KEYWORD, 'berhenti'):
      hasil.register_advancement()
      self.advance()
      return hasil.success(BreakNode(pos_mulai, self.current_token.pos_mulai.copy()))

    expression = hasil.register(self.expression())
    if hasil.error:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        "Diharapkan 'kembalikan', 'lanjut', 'berhenti', 'var', 'jika', 'untuk', 'sementara', 'fungsi', int, float, identifier, '+', '-', '(', '[' atau 'tidak'"
      ))
    return hasil.success(expression)

  def expression(self):
    hasil = ParseResult()

    if self.current_token.matches(TT_KEYWORD, 'var'):
      hasil.register_advancement()
      self.advance()

      if self.current_token.type != TT_IDENTIFIER:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          "Identifier yang diharapkan"
        ))

      nama_variabel = self.current_token
      hasil.register_advancement()
      self.advance()

      if self.current_token.type != TT_EQ:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          "Diharapkan '='"
        ))

      hasil.register_advancement()
      self.advance()
      expression = hasil.register(self.expression())
      if hasil.error: return hasil
      return hasil.success(VarAssignNode(nama_variabel, expression))

    node = hasil.register(self.bin_op(self.bandingkan_expression, ((TT_KEYWORD, 'dan'), (TT_KEYWORD, 'or'))))

    if hasil.error:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        "Diharapkan 'kembalikan', 'lanjut', 'berhenti', 'var', 'jika', 'untuk', 'sementara', 'fungsi', int, float, identifier, '+', '-', '(', '[' atau 'tidak'"
      ))

    return hasil.success(node)

  def bandingkan_expression(self):
    hasil = ParseResult()

    if self.current_token.matches(TT_KEYWORD, 'NOT'):
      token_operation = self.current_token
      hasil.register_advancement()
      self.advance()

      node = hasil.register(self.bandingkan_expression())
      if hasil.error: return hasil
      return hasil.success(UnaryOpNode(token_operation, node))
    
    node = hasil.register(self.bin_op(self.aritmatika_expression, (TT_EE, TT_NE, TT_LT, TT_GT, TT_LTE, TT_GTE)))
    
    if hasil.error:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        "Diharapkan int, float, identifier, '+', '-', '(', '[', 'jika', 'untuk', 'sementara', 'fungsi' atau 'tidak'"
      ))

    return hasil.success(node)

  def aritmatika_expression(self):
    return self.bin_op(self.kali_bagi, (TT_PLUS, TT_MINUS))

  def kali_bagi(self):
    return self.bin_op(self.tambah_kurang, (TT_MUL, TT_DIV))

  def tambah_kurang(self):
    hasil = ParseResult()
    token = self.current_token

    if token.type in (TT_PLUS, TT_MINUS):
      hasil.register_advancement()
      self.advance()
      tambah_kurang = hasil.register(self.tambah_kurang())
      if hasil.error: return hasil
      return hasil.success(UnaryOpNode(token, tambah_kurang))

    return self.pangkat()

  def pangkat(self):
    return self.bin_op(self.call, (TT_POW, ), self.tambah_kurang)

  def call(self):
    hasil = ParseResult()
    atom = hasil.register(self.atom())
    if hasil.error: return hasil

    if self.current_token.type == TT_LPAREN:
      hasil.register_advancement()
      self.advance()
      arg_nodes = []

      if self.current_token.type == TT_RPAREN:
        hasil.register_advancement()
        self.advance()
      else:
        arg_nodes.append(hasil.register(self.expression()))
        if hasil.error:
          return hasil.failure(InvalidSyntaxError(
            self.current_token.pos_mulai, self.current_token.pos_akhir,
            "Diharapkan 'kembalikan', 'lanjut', 'berhenti', 'var', 'jika', 'untuk', 'sementara', 'fungsi', int, float, identifier, '+', '-', '(', '[' atau 'tidak'"
          ))

        while self.current_token.type == TT_COMMA:
          hasil.register_advancement()
          self.advance()

          arg_nodes.append(hasil.register(self.expression()))
          if hasil.error: return hasil

        if self.current_token.type != TT_RPAREN:
          return hasil.failure(InvalidSyntaxError(
            self.current_token.pos_mulai, self.current_token.pos_akhir,
            f"Expected ',' or ')'"
          ))

        hasil.register_advancement()
        self.advance()
      return hasil.success(CallNode(atom, arg_nodes))
    return hasil.success(atom)

  def atom(self):
    hasil = ParseResult()
    token = self.current_token

    if token.type in (TT_INT, TT_FLOAT):
      hasil.register_advancement()
      self.advance()
      return hasil.success(AngkaNode(token))

    elif token.type == TT_STRING:
      hasil.register_advancement()
      self.advance()
      return hasil.success(StringNode(token))

    elif token.type == TT_IDENTIFIER:
      hasil.register_advancement()
      self.advance()
      return hasil.success(VarAccessNode(token))

    elif token.type == TT_LPAREN:
      hasil.register_advancement()
      self.advance()
      expression = hasil.register(self.expression())
      if hasil.error: return hasil
      if self.current_token.type == TT_RPAREN:
        hasil.register_advancement()
        self.advance()
        return hasil.success(expression)
      else:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          "Diharapkan ')'"
        ))

    elif token.type == TT_LSQUARE:
      list_expression = hasil.register(self.list_expression())
      if hasil.error: return hasil
      return hasil.success(list_expression)
    
    elif token.matches(TT_KEYWORD, 'jika'):
      if_expression = hasil.register(self.if_expression())
      if hasil.error: return hasil
      return hasil.success(if_expression)

    elif token.matches(TT_KEYWORD, 'untuk'):
      for_expression = hasil.register(self.for_expression())
      if hasil.error: return hasil
      return hasil.success(for_expression)

    elif token.matches(TT_KEYWORD, 'sementara'):
      while_expression = hasil.register(self.while_expression())
      if hasil.error: return hasil
      return hasil.success(while_expression)

    elif token.matches(TT_KEYWORD, 'fungsi'):
      fungsi_definisi = hasil.register(self.fungsi_definisi())
      if hasil.error: return hasil
      return hasil.success(fungsi_definisi)

    return hasil.failure(InvalidSyntaxError(
      token.pos_mulai, token.pos_akhir,
      "Diharapkan  int, float, identifier, '+', '-', '(', '[' atau 'tidak'"
    ))

  def list_expression(self):
    hasil = ParseResult()
    element_nodes = []
    pos_mulai = self.current_token.pos_mulai.copy()

    if self.current_token.type != TT_LSQUARE:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan '['"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_RSQUARE:
      hasil.register_advancement()
      self.advance()
    else:
      element_nodes.append(hasil.register(self.expression()))
      if hasil.error:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          "Diharapkan ']', 'var', 'jika', 'untuk', 'sementara', 'fungsi', int, float, identifier, '+', '-', '(', '[' atau 'tidak'"
        ))

      while self.current_token.type == TT_COMMA:
        hasil.register_advancement()
        self.advance()

        element_nodes.append(hasil.register(self.expression()))
        if hasil.error: return hasil

      if self.current_token.type != TT_RSQUARE:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan ',' atau ']'"
        ))

      hasil.register_advancement()
      self.advance()

    return hasil.success(ListNode(
      element_nodes,
      pos_mulai,
      self.current_token.pos_akhir.copy()
    ))

  def if_expression(self):
    hasil = ParseResult()
    all_cases = hasil.register(self.if_expr_cases('jika'))
    if hasil.error: return hasil
    cases, else_case = all_cases
    return hasil.success(IfNode(cases, else_case))

  def if_expr_b(self):
    return self.if_expr_cases('lain_jika')
    
  def if_expr_c(self):
    hasil = ParseResult()
    else_case = None

    if self.current_token.matches(TT_KEYWORD, 'lain'):
      hasil.register_advancement()
      self.advance()

      if self.current_token.type == TT_NEWLINE:
        hasil.register_advancement()
        self.advance()

        statements = hasil.register(self.statements())
        if hasil.error: return hasil
        else_case = (statements, True)

        if self.current_token.matches(TT_KEYWORD, 'selesai'):
          hasil.register_advancement()
          self.advance()
        else:
          return hasil.failure(InvalidSyntaxError(
            self.current_token.pos_mulai, self.current_token.pos_akhir,
            "Diharapkan 'selesai'"
          ))
      else:
        expression = hasil.register(self.statement())
        if hasil.error: return hasil
        else_case = (expression, False)

    return hasil.success(else_case)

  def if_expr_b_or_c(self):
    hasil = ParseResult()
    cases, else_case = [], None

    if self.current_token.matches(TT_KEYWORD, 'lain_jika'):
      all_cases = hasil.register(self.if_expr_b())
      if hasil.error: return hasil
      cases, else_case = all_cases
    else:
      else_case = hasil.register(self.if_expr_c())
      if hasil.error: return hasil
    
    return hasil.success((cases, else_case))

  def if_expr_cases(self, case_keyword):
    hasil = ParseResult()
    cases = []
    else_case = None

    if not self.current_token.matches(TT_KEYWORD, case_keyword):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan '{case_keyword}'"
      ))

    hasil.register_advancement()
    self.advance()

    kondisi = hasil.register(self.expression())
    if hasil.error: return hasil

    if not self.current_token.matches(TT_KEYWORD, 'maka'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'maka'"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_NEWLINE:
      hasil.register_advancement()
      self.advance()

      statements = hasil.register(self.statements())
      if hasil.error: return hasil
      cases.append((kondisi, statements, True))

      if self.current_token.matches(TT_KEYWORD, 'selesai'):
        hasil.register_advancement()
        self.advance()
      else:
        all_cases = hasil.register(self.if_expr_b_or_c())
        if hasil.error: return hasil
        new_cases, else_case = all_cases
        cases.extend(new_cases)
    else:
      expr = hasil.register(self.statement())
      if hasil.error: return hasil
      cases.append((kondisi, expr, False))

      all_cases = hasil.register(self.if_expr_b_or_c())
      if hasil.error: return hasil
      new_cases, else_case = all_cases
      cases.extend(new_cases)

    return hasil.success((cases, else_case))

  def for_expression(self):
    hasil = ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'untuk'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'untuk'"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type != TT_IDENTIFIER:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan identifier"
      ))

    nama_variabel = self.current_token
    hasil.register_advancement()
    self.advance()

    if self.current_token.type != TT_EQ:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan '='"
      ))
    
    hasil.register_advancement()
    self.advance()

    start_value = hasil.register(self.expression())
    if hasil.error: return hasil

    if not self.current_token.matches(TT_KEYWORD, 'sampai'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'sampai'"
      ))
    
    hasil.register_advancement()
    self.advance()

    end_value = hasil.register(self.expression())
    if hasil.error: return hasil

    if self.current_token.matches(TT_KEYWORD, 'langkah'):
      hasil.register_advancement()
      self.advance()

      step_value = hasil.register(self.expression())
      if hasil.error: return hasil
    else:
      step_value = None

    if not self.current_token.matches(TT_KEYWORD, 'maka'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'maka'"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_NEWLINE:
      hasil.register_advancement()
      self.advance()

      body = hasil.register(self.statements())
      if hasil.error: return hasil

      if not self.current_token.matches(TT_KEYWORD, 'selesai'):
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan 'selesai'"
        ))

      hasil.register_advancement()
      self.advance()

      return hasil.success(ForNode(nama_variabel, start_value, end_value, step_value, body, True))
    
    body = hasil.register(self.statement())
    if hasil.error: return hasil

    return hasil.success(ForNode(nama_variabel, start_value, end_value, step_value, body, False))

  def while_expression(self):
    hasil = ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'sementara'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'sementara'"
      ))

    hasil.register_advancement()
    self.advance()

    kondisi = hasil.register(self.expression())
    if hasil.error: return hasil

    if not self.current_token.matches(TT_KEYWORD, 'maka'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'maka'"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_NEWLINE:
      hasil.register_advancement()
      self.advance()

      body = hasil.register(self.statements())
      if hasil.error: return hasil

      if not self.current_token.matches(TT_KEYWORD, 'selesai'):
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan 'selesai'"
        ))

      hasil.register_advancement()
      self.advance()

      return hasil.success(WhileNode(kondisi, body, True))
    
    body = hasil.register(self.statement())
    if hasil.error: return hasil

    return hasil.success(WhileNode(kondisi, body, False))

  def fungsi_definisi(self):
    hasil = ParseResult()

    if not self.current_token.matches(TT_KEYWORD, 'fungsi'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'fungsi'"
      ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_IDENTIFIER:
      nama_variabel_token = self.current_token
      hasil.register_advancement()
      self.advance()
      if self.current_token.type != TT_LPAREN:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan '('"
        ))
    else:
      nama_variabel_token = None
      if self.current_token.type != TT_LPAREN:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan identifier atau '('"
        ))
    
    hasil.register_advancement()
    self.advance()
    nama_argumen_tokens = []

    if self.current_token.type == TT_IDENTIFIER:
      nama_argumen_tokens.append(self.current_token)
      hasil.register_advancement()
      self.advance()
      
      while self.current_token.type == TT_COMMA:
        hasil.register_advancement()
        self.advance()

        if self.current_token.type != TT_IDENTIFIER:
          return hasil.failure(InvalidSyntaxError(
            self.current_token.pos_mulai, self.current_token.pos_akhir,
            f"Diharapkan identifier"
          ))

        nama_argumen_tokens.append(self.current_token)
        hasil.register_advancement()
        self.advance()
      
      if self.current_token.type != TT_RPAREN:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapkan ',' atau ')'"
        ))
    else:
      if self.current_token.type != TT_RPAREN:
        return hasil.failure(InvalidSyntaxError(
          self.current_token.pos_mulai, self.current_token.pos_akhir,
          f"Diharapakan identifier atau ')'"
        ))

    hasil.register_advancement()
    self.advance()

    if self.current_token.type == TT_ARROW:
      hasil.register_advancement()
      self.advance()

      body = hasil.register(self.expression())
      if hasil.error: return hasil

      return hasil.success(FuncDefNode(
        nama_variabel_token,
        nama_argumen_tokens,
        body,
        True
      ))
    
    if self.current_token.type != TT_NEWLINE:
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan '->' atau NEWLINE"
      ))

    hasil.register_advancement()
    self.advance()

    body = hasil.register(self.statements())
    if hasil.error: return hasil

    if not self.current_token.matches(TT_KEYWORD, 'selesai'):
      return hasil.failure(InvalidSyntaxError(
        self.current_token.pos_mulai, self.current_token.pos_akhir,
        f"Diharapkan 'selesai'"
      ))

    hasil.register_advancement()
    self.advance()
    
    return hasil.success(FuncDefNode(
      nama_variabel_token,
      nama_argumen_tokens,
      body,
      False
    ))

  ###################################

  def bin_op(self, func_a, ops, func_b=None):
    if func_b == None:
      func_b = func_a
    
    hasil = ParseResult()
    left = hasil.register(func_a())
    if hasil.error: return hasil

    while self.current_token.type in ops or (self.current_token.type, self.current_token.value) in ops:
      token_operation = self.current_token
      hasil.register_advancement()
      self.advance()
      right = hasil.register(func_b())
      if hasil.error: return hasil
      left = BinOpNode(left, token_operation, right)

    return hasil.success(left)

#######################################
# NODES
#######################################

class AngkaNode:
  def __init__(self, token):
    self.token = token

    self.pos_mulai = self.token.pos_mulai
    self.pos_akhir = self.token.pos_akhir

  def __repr__(self):
    return f'{self.token}'

class StringNode:
  def __init__(self, token):
    self.token = token

    self.pos_mulai = self.token.pos_mulai
    self.pos_akhir = self.token.pos_akhir

  def __repr__(self):
    return f'{self.token}'

class ListNode:
  def __init__(self, element_nodes, pos_mulai, pos_akhir):
    self.element_nodes = element_nodes

    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir

class VarAccessNode:
  def __init__(self, nama_variabel_token):
    self.nama_variabel_token = nama_variabel_token

    self.pos_mulai = self.nama_variabel_token.pos_mulai
    self.pos_akhir = self.nama_variabel_token.pos_akhir

class VarAssignNode:
  def __init__(self, nama_variabel_token, value_node):
    self.nama_variabel_token = nama_variabel_token
    self.value_node = value_node

    self.pos_mulai = self.nama_variabel_token.pos_mulai
    self.pos_akhir = self.value_node.pos_akhir

class BinOpNode:
  def __init__(self, left_node, token_operation, right_node):
    self.left_node = left_node
    self.token_operation = token_operation
    self.right_node = right_node

    self.pos_mulai = self.left_node.pos_mulai
    self.pos_akhir = self.right_node.pos_akhir

  def __repr__(self):
    return f'({self.left_node}, {self.token_operation}, {self.right_node})'

class UnaryOpNode:
  def __init__(self, token_operation, node):
    self.token_operation = token_operation
    self.node = node

    self.pos_mulai = self.token_operation.pos_mulai
    self.pos_akhir = node.pos_akhir

  def __repr__(self):
    return f'({self.token_operation}, {self.node})'

class IfNode:
  def __init__(self, cases, else_case):
    self.cases = cases
    self.else_case = else_case

    self.pos_mulai = self.cases[0][0].pos_mulai
    self.pos_akhir = (self.else_case or self.cases[len(self.cases) - 1])[0].pos_akhir

class ForNode:
  def __init__(self, nama_variabel_token, start_value_node, end_value_node, step_value_node, body_node, should_return_null):
    self.nama_variabel_token = nama_variabel_token
    self.start_value_node = start_value_node
    self.end_value_node = end_value_node
    self.step_value_node = step_value_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.pos_mulai = self.nama_variabel_token.pos_mulai
    self.pos_akhir = self.body_node.pos_akhir

class WhileNode:
  def __init__(self, condition_node, body_node, should_return_null):
    self.condition_node = condition_node
    self.body_node = body_node
    self.should_return_null = should_return_null

    self.pos_mulai = self.condition_node.pos_mulai
    self.pos_akhir = self.body_node.pos_akhir

class FuncDefNode:
  def __init__(self, nama_variabel_token, nama_argumen_tokens, body_node, should_auto_return):
    self.nama_variabel_token = nama_variabel_token
    self.nama_argumen_tokens = nama_argumen_tokens
    self.body_node = body_node
    self.should_auto_return = should_auto_return

    if self.nama_variabel_token:
      self.pos_mulai = self.nama_variabel_token.pos_mulai
    elif len(self.nama_argumen_tokens) > 0:
      self.pos_mulai = self.nama_argumen_tokens[0].pos_mulai
    else:
      self.pos_mulai = self.body_node.pos_mulai

    self.pos_akhir = self.body_node.pos_akhir

class CallNode:
  def __init__(self, node_to_call, arg_nodes):
    self.node_to_call = node_to_call
    self.arg_nodes = arg_nodes

    self.pos_mulai = self.node_to_call.pos_mulai

    if len(self.arg_nodes) > 0:
      self.pos_akhir = self.arg_nodes[len(self.arg_nodes) - 1].pos_akhir
    else:
      self.pos_akhir = self.node_to_call.pos_akhir

class ReturnNode:
  def __init__(self, node_to_return, pos_mulai, pos_akhir):
    self.node_to_return = node_to_return

    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir

class ContinueNode:
  def __init__(self, pos_mulai, pos_akhir):
    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir

class BreakNode:
  def __init__(self, pos_mulai, pos_akhir):
    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir

#######################################
# PARSE RESULT
#######################################

class ParseResult:
  def __init__(self):
    self.error = None
    self.node = None
    self.last_registered_advance_count = 0
    self.advance_count = 0
    self.to_reverse_count = 0

  def register_advancement(self):
    self.last_registered_advance_count = 1
    self.advance_count += 1

  def register(self, hasil):
    self.last_registered_advance_count = hasil.advance_count
    self.advance_count += hasil.advance_count
    if hasil.error: self.error = hasil.error
    return hasil.node

  def try_register(self, hasil):
    if hasil.error:
      self.to_reverse_count = hasil.advance_count
      return None
    return self.register(hasil)

  def success(self, node):
    self.node = node
    return self

  def failure(self, error):
    if not self.error or self.last_registered_advance_count == 0:
      self.error = error
    return self


#######################################
# RUNTIME RESULT
#######################################

class RTResult:
  def __init__(self):
    self.reset()

  def reset(self):
    self.value = None
    self.error = None
    self.fungsi_mengembalikan_nilai = None
    self.loop_lanjut = False
    self.loop_berhenti = False

  def register(self, hasil):
    self.error = hasil.error
    self.fungsi_mengembalikan_nilai = hasil.fungsi_mengembalikan_nilai
    self.loop_lanjut = hasil.loop_lanjut
    self.loop_berhenti = hasil.loop_berhenti
    return hasil.value

  def success(self, value):
    self.reset()
    self.value = value
    return self

  def success_return(self, value):
    self.reset()
    self.fungsi_mengembalikan_nilai = value
    return self
  
  def success_continue(self):
    self.reset()
    self.loop_lanjut = True
    return self

  def success_break(self):
    self.reset()
    self.loop_berhenti = True
    return self

  def failure(self, error):
    self.reset()
    self.error = error
    return self

  def should_return(self):
    # Note: this will allow you to continue and break outside the current function
    return (
      self.error or
      self.fungsi_mengembalikan_nilai or
      self.loop_lanjut or
      self.loop_berhenti
    )

#######################################
# VALUES
#######################################

class Value:
  def __init__(self):
    self.set_posisi()
    self.set_isi()

  def set_posisi(self, pos_mulai=None, pos_akhir=None):
    self.pos_mulai = pos_mulai
    self.pos_akhir = pos_akhir
    return self

  def set_isi(self, isi=None):
    self.isi = isi
    return self

  def tambahkan_ke(self, other):
    return None, self.illegal_operation(other)

  def kurangkan_dengan(self, other):
    return None, self.illegal_operation(other)

  def kali_dengan(self, other):
    return None, self.illegal_operation(other)

  def bagi_dengan(self, other):
    return None, self.illegal_operation(other)

  def pangkatkan_dengan(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_sama_dengan(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_tidak_sama(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_kurang_dari(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_lebih_dari(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_kurang_dari_sama_dengan(self, other):
    return None, self.illegal_operation(other)

  def get_perbandingan_lebih_dari_sama_dengan(self, other):
    return None, self.illegal_operation(other)

  def cek_and_dengan(self, other):
    return None, self.illegal_operation(other)

  def cek_atau_dengan(self, other):
    return None, self.illegal_operation(other)

  def cek_tidak(self, other):
    return None, self.illegal_operation(other)

  def jalankan(self, args):
    return RTResult().failure(self.illegal_operation())

  def copy(self):
    raise Exception('No copy method defined')

  def apakah_benar(self):
    return False

  def illegal_operation(self, other=None):
    if not other: other = self
    return RTError(
      self.pos_mulai, other.pos_akhir,
      'Operasi ilegal',
      self.isi
    )

class Number(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def tambahkan_ke(self, other):
    if isinstance(other, Number):
      return Number(self.value + other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def kurangkan_dengan(self, other):
    if isinstance(other, Number):
      return Number(self.value - other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def kali_dengan(self, other):
    if isinstance(other, Number):
      return Number(self.value * other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def bagi_dengan(self, other):
    if isinstance(other, Number):
      if other.value == 0:
        return None, RTError(
          other.pos_mulai, other.pos_akhir,
          'Division by zero',
          self.isi
        )

      return Number(self.value / other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def pangkatkan_dengan(self, other):
    if isinstance(other, Number):
      return Number(self.value ** other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_sama_dengan(self, other):
    if isinstance(other, Number):
      return Number(int(self.value == other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_tidak_sama(self, other):
    if isinstance(other, Number):
      return Number(int(self.value != other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_kurang_dari(self, other):
    if isinstance(other, Number):
      return Number(int(self.value < other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_lebih_dari(self, other):
    if isinstance(other, Number):
      return Number(int(self.value > other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_kurang_dari_sama_dengan(self, other):
    if isinstance(other, Number):
      return Number(int(self.value <= other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def get_perbandingan_lebih_dari_sama_dengan(self, other):
    if isinstance(other, Number):
      return Number(int(self.value >= other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def cek_and_dengan(self, other):
    if isinstance(other, Number):
      return Number(int(self.value and other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def cek_atau_dengan(self, other):
    if isinstance(other, Number):
      return Number(int(self.value or other.value)).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def cek_tidak(self):
    return Number(1 if self.value == 0 else 0).set_isi(self.isi), None

  def copy(self):
    copy = Number(self.value)
    copy.set_posisi(self.pos_mulai, self.pos_akhir)
    copy.set_isi(self.isi)
    return copy

  def apakah_benar(self):
    return self.value != 0

  def __str__(self):
    return str(self.value)
  
  def __repr__(self):
    return str(self.value)

Number.null = Number(0)
Number.false = Number(0)
Number.true = Number(1)
Number.math_PI = Number(math.pi)


class String(Value):
  def __init__(self, value):
    super().__init__()
    self.value = value

  def tambahkan_ke(self, other):
    if isinstance(other, String):
      return String(self.value + other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def kali_dengan(self, other):
    if isinstance(other, Number):
      return String(self.value * other.value).set_isi(self.isi), None
    else:
      return None, Value.illegal_operation(self, other)

  def apakah_benar(self):
    return len(self.value) > 0

  def copy(self):
    copy = String(self.value)
    copy.set_posisi(self.pos_mulai, self.pos_akhir)
    copy.set_isi(self.isi)
    return copy

  def __str__(self):
    return self.value

  def __repr__(self):
    return f'"{self.value}"'

class List(Value):
  def __init__(self, elements):
    super().__init__()
    self.elements = elements

  def tambahkan_ke(self, other):
    new_list = self.copy()
    new_list.elements.append(other)
    return new_list, None

  def kurangkan_dengan(self, other):
    if isinstance(other, Number):
      new_list = self.copy()
      try:
        new_list.elements.pop(other.value)
        return new_list, None
      except:
        return None, RTError(
          other.pos_mulai, other.pos_akhir,
          'Elemen pada index ini tidak bisa dikurangi karena index di luar jangkauan',
          self.isi
        )
    else:
      return None, Value.illegal_operation(self, other)

  def kali_dengan(self, other):
    if isinstance(other, List):
      new_list = self.copy()
      new_list.elements.extend(other.elements)
      return new_list, None
    else:
      return None, Value.illegal_operation(self, other)

  def bagi_dengan(self, other):
    if isinstance(other, Number):
      try:
        return self.elements[other.value], None
      except:
        return None, RTError(
          other.pos_mulai, other.pos_akhir,
          'Elemen pada index ini tidak bisa dibagi karena index di luar jangkauan',
          self.isi
        )
    else:
      return None, Value.illegal_operation(self, other)
  
  def copy(self):
    copy = List(self.elements)
    copy.set_posisi(self.pos_mulai, self.pos_akhir)
    copy.set_isi(self.isi)
    return copy

  def __str__(self):
    return ", ".join([str(x) for x in self.elements])

  def __repr__(self):
    return f'[{", ".join([repr(x) for x in self.elements])}]'

class BaseFunction(Value):
  def __init__(self, name):
    super().__init__()
    self.name = name or "<anonymous>"

  def generate_new_context(self):
    new_context = Context(self.name, self.isi, self.pos_mulai)
    new_context.symbol_table = SymbolTable(new_context.parent.symbol_table)
    return new_context

  def cek_argumens(self, arg_names, args):
    hasil = RTResult()

    if len(args) > len(arg_names):
      return hasil.failure(RTError(
        self.pos_mulai, self.pos_akhir,
        f"kelebihan {len(args) - len(arg_names)} argumen pada {self.name}()",
        self.isi
      ))
    
    if len(args) < len(arg_names):
      return hasil.failure(RTError(
        self.pos_mulai, self.pos_akhir,
        f"kurang {len(arg_names) - len(args)} argumen pada {self.name}()",
        self.isi
      ))

    return hasil.success(None)

  def populate_args(self, arg_names, args, exec_ctx):
    for i in range(len(args)):
      nama_argumen = arg_names[i]
      nilai_argumen = args[i]
      nilai_argumen.set_isi(exec_ctx)
      exec_ctx.symbol_table.set(nama_argumen, nilai_argumen)

  def check_and_populate_args(self, arg_names, args, exec_ctx):
    hasil = RTResult()
    hasil.register(self.cek_argumens(arg_names, args))
    if hasil.should_return(): return hasil
    self.populate_args(arg_names, args, exec_ctx)
    return hasil.success(None)

class Function(BaseFunction):
  def __init__(self, name, body_node, arg_names, should_auto_return):
    super().__init__(name)
    self.body_node = body_node
    self.arg_names = arg_names
    self.should_auto_return = should_auto_return

  def execute(self, args):
    hasil = RTResult()
    interpreter = Interpreter()
    exec_ctx = self.generate_new_context()

    hasil.register(self.check_and_populate_args(self.arg_names, args, exec_ctx))
    if hasil.should_return(): return hasil

    value = hasil.register(interpreter.visit(self.body_node, exec_ctx))
    if hasil.should_return() and hasil.fungsi_mengembalikan_nilai == None: return hasil

    ret_value = (value if self.should_auto_return else None) or hasil.fungsi_mengembalikan_nilai or Number.null
    return hasil.success(ret_value)

  def copy(self):
    copy = Function(self.name, self.body_node, self.arg_names, self.should_auto_return)
    copy.set_isi(self.isi)
    copy.set_posisi(self.pos_mulai, self.pos_akhir)
    return copy

  def __repr__(self):
    return f"<fungsi {self.name} berhasil dibuat>"

class BuiltInFunction(BaseFunction):
  def __init__(self, name):
    super().__init__(name)

  def execute(self, args):
    hasil = RTResult()
    exec_ctx = self.generate_new_context()

    nama_method = f'menjalankan_{self.name}'
    method = getattr(self, nama_method, self.no_visit_method)

    hasil.register(self.check_and_populate_args(method.arg_names, args, exec_ctx))
    if hasil.should_return(): return hasil

    return_value = hasil.register(method(exec_ctx))
    if hasil.should_return(): return hasil
    return hasil.success(return_value)
  
  def no_visit_method(self, node, context):
    raise Exception(f'Tidak menjalankan_{self.name} mmethod tidak terdefinisi')

  def copy(self):
    copy = BuiltInFunction(self.name)
    copy.set_isi(self.isi)
    copy.set_posisi(self.pos_mulai, self.pos_akhir)
    return copy

  def __repr__(self):
    return f"<built-in function {self.name}>"

  #####################################

  def menjalankan_print(self, exec_ctx):
    print(str(exec_ctx.symbol_table.get('value')))
    return RTResult().success(Number.null)
  menjalankan_print.arg_names = ['value']
  
  def menjalankan_print_ret(self, exec_ctx):
    return RTResult().success(String(str(exec_ctx.symbol_table.get('value'))))
  menjalankan_print_ret.arg_names = ['value']
  
  def menjalankan_input(self, exec_ctx):
    text = input()
    return RTResult().success(String(text))
  menjalankan_input.arg_names = []

  def menjalankan_input_int(self, exec_ctx):
    while True:
      text = input()
      try:
        angka = int(text)
        break
      except ValueError:
        print(f"'{text}' harus bilangan integer. Coba Lagi!")
    return RTResult().success(Number(angka))
  menjalankan_input_int.arg_names = []

  def menjalankan_clear(self, exec_ctx):
    os.system('cls' if os.name == 'nt' else 'cls') 
    return RTResult().success(Number.null)
  menjalankan_clear.arg_names = []

  def menjalankan_apakah_angka(self, exec_ctx):
    apakah_angka = isinstance(exec_ctx.symbol_table.get("value"), Number)
    return RTResult().success(Number.true if apakah_angka else Number.false)
  menjalankan_apakah_angka.arg_names = ["value"]

  def menjalankan_apakah_string(self, exec_ctx):
    apakah_string = isinstance(exec_ctx.symbol_table.get("value"), String)
    return RTResult().success(Number.true if apakah_string else Number.false)
  menjalankan_apakah_string.arg_names = ["value"]

  def menjalankan_apakah_list(self, exec_ctx):
    apakah_list = isinstance(exec_ctx.symbol_table.get("value"), List)
    return RTResult().success(Number.true if apakah_list else Number.false)
  menjalankan_apakah_list.arg_names = ["value"]

  def menjalankan_apakah_fungsi(self, exec_ctx):
    apakah_fungsi = isinstance(exec_ctx.symbol_table.get("value"), BaseFunction)
    return RTResult().success(Number.true if apakah_fungsi else Number.false)
  menjalankan_apakah_fungsi.arg_names = ["value"]

  def menjalankan_tambahkan_ke(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")
    value = exec_ctx.symbol_table.get("value")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "First argument must be list",
        exec_ctx
      ))

    list_.elements.append(value)
    return RTResult().success(Number.null)
  menjalankan_tambahkan_ke.arg_names = ["list", "value"]

  def menjalankan_keluarkan_dari(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")
    index = exec_ctx.symbol_table.get("index")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argumen pertama harus list",
        exec_ctx
      ))

    if not isinstance(index, Number):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argumen kedua harus angka",
        exec_ctx
      ))

    try:
      element = list_.elements.pop(index.value)
    except:
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        'Elemen pada index ini tidak bisa dikurangi karena index di luar jangkauan',
        exec_ctx
      ))
    return RTResult().success(element)
  menjalankan_keluarkan_dari.arg_names = ["list", "index"]

  def menjalankan_gabungkan(self, exec_ctx):
    listA = exec_ctx.symbol_table.get("listA")
    listB = exec_ctx.symbol_table.get("listB")

    if not isinstance(listA, List):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argumen pertama harus list",
        exec_ctx
      ))

    if not isinstance(listB, List):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argumen kedua harus list",
        exec_ctx
      ))

    listA.elements.extend(listB.elements)
    return RTResult().success(Number.null)
  menjalankan_gabungkan.arg_names = ["listA", "listB"]

  def menjalankan_len(self, exec_ctx):
    list_ = exec_ctx.symbol_table.get("list")

    if not isinstance(list_, List):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argument harus list",
        exec_ctx
      ))

    return RTResult().success(Number(len(list_.elements)))
  menjalankan_len.arg_names = ["list"]

  def menjalankan_run(self, exec_ctx):
    nama_file = exec_ctx.symbol_table.get("nama_file")

    if not isinstance(nama_file, String):
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        "Argumen kedua harus string",
        exec_ctx
      ))

    nama_file = nama_file.value

    try:
      with open(nama_file, "r") as f:
        script = f.read()
    except Exception as e:
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        f"Gagal memuat script \"{nama_file}\"\n" + str(e),
        exec_ctx
      ))

    _, error = run(nama_file, script)
    
    if error:
      return RTResult().failure(RTError(
        self.pos_mulai, self.pos_akhir,
        f"Gagal untuk menjalan script \"{nama_file}\"\n" +
        error.as_string(),
        exec_ctx
      ))

    return RTResult().success(Number.null)
  menjalankan_run.arg_names = ["nama_file"]

BuiltInFunction.print       = BuiltInFunction("print")
BuiltInFunction.print_ret   = BuiltInFunction("print_ret")
BuiltInFunction.input       = BuiltInFunction("input")
BuiltInFunction.input_int   = BuiltInFunction("input_int")
BuiltInFunction.clear       = BuiltInFunction("clear")
BuiltInFunction.apakah_angka   = BuiltInFunction("apakah_angka")
BuiltInFunction.apakah_string   = BuiltInFunction("apakah_string")
BuiltInFunction.apakah_list     = BuiltInFunction("apakah_list")
BuiltInFunction.apakah_fungsi = BuiltInFunction("apakah_fungsi")
BuiltInFunction.tambahkan_ke      = BuiltInFunction("tambahkan_ke")
BuiltInFunction.keluarkan_dari         = BuiltInFunction("keluarkan_dari")
BuiltInFunction.gabungkan      = BuiltInFunction("gabungkan")
BuiltInFunction.len					= BuiltInFunction("len")
BuiltInFunction.run					= BuiltInFunction("run")

#######################################
# CONTEXT
#######################################

class Context:
  def __init__(self, display_name, parent=None, parent_entry_pos=None):
    self.display_name = display_name
    self.parent = parent
    self.parent_entry_pos = parent_entry_pos
    self.symbol_table = None

#######################################
# SYMBOL TABLE
#######################################

class SymbolTable:
  def __init__(self, parent=None):
    self.symbols = {}
    self.parent = parent

  def get(self, name):
    value = self.symbols.get(name, None)
    if value == None and self.parent:
      return self.parent.get(name)
    return value

  def set(self, name, value):
    self.symbols[name] = value

  def remove(self, name):
    del self.symbols[name]

#######################################
# INTERPRETER
#######################################

class Interpreter:
  def visit(self, node, isi):
    nama_method = f'visit_{type(node).__name__}'
    method = getattr(self, nama_method, self.no_visit_method)
    return method(node, isi)

  def no_visit_method(self, node, isi):
    raise Exception(f'No visit_{type(node).__name__} method defined')

  ###################################

  def visit_AngkaNode(self, node, isi):
    return RTResult().success(
      Number(node.token.value).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    )

  def visit_StringNode(self, node, isi):
    return RTResult().success(
      String(node.token.value).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    )

  def visit_ListNode(self, node, isi):
    hasil = RTResult()
    elements = []

    for element_node in node.element_nodes:
      elements.append(hasil.register(self.visit(element_node, isi)))
      if hasil.should_return(): return hasil

    return hasil.success(
      List(elements).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    )

  def visit_VarAccessNode(self, node, isi):
    hasil = RTResult()
    nama_variabel = node.nama_variabel_token.value
    value = isi.symbol_table.get(nama_variabel)

    if not value:
      return hasil.failure(RTError(
        node.pos_mulai, node.pos_akhir,
        f"'{nama_variabel}' tidak terdefinisi",
        isi
      ))

    value = value.copy().set_posisi(node.pos_mulai, node.pos_akhir).set_isi(isi)
    return hasil.success(value)

  def visit_VarAssignNode(self, node, isi):
    hasil = RTResult()
    nama_variable = node.nama_variabel_token.value
    value = hasil.register(self.visit(node.value_node, isi))
    if hasil.should_return(): return hasil

    isi.symbol_table.set(nama_variable, value)
    return hasil.success(value)

  def visit_BinOpNode(self, node, isi):
    hasil = RTResult()
    left = hasil.register(self.visit(node.left_node, isi))
    if hasil.should_return(): return hasil
    right = hasil.register(self.visit(node.right_node, isi))
    if hasil.should_return(): return hasil

    if node.token_operation.type == TT_PLUS:
      result, error = left.tambahkan_ke(right)
    elif node.token_operation.type == TT_MINUS:
      result, error = left.kurangkan_dengan(right)
    elif node.token_operation.type == TT_MUL:
      result, error = left.kali_dengan(right)
    elif node.token_operation.type == TT_DIV:
      result, error = left.bagi_dengan(right)
    elif node.token_operation.type == TT_POW:
      result, error = left.pangkatkan_dengan(right)
    elif node.token_operation.type == TT_EE:
      result, error = left.get_perbandingan_sama_dengan(right)
    elif node.token_operation.type == TT_NE:
      result, error = left.get_perbandingan_tidak_sama(right)
    elif node.token_operation.type == TT_LT:
      result, error = left.get_perbandingan_kurang_dari(right)
    elif node.token_operation.type == TT_GT:
      result, error = left.get_perbandingan_lebih_dari(right)
    elif node.token_operation.type == TT_LTE:
      result, error = left.get_perbandingan_kurang_dari_sama_dengan(right)
    elif node.token_operation.type == TT_GTE:
      result, error = left.get_perbandingan_lebih_dari_sama_dengan(right)
    elif node.token_operation.matches(TT_KEYWORD, 'dan'):
      result, error = left.cek_and_dengan(right)
    elif node.token_operation.matches(TT_KEYWORD, 'atau'):
      result, error = left.cek_atau_dengan(right)

    if error:
      return hasil.failure(error)
    else:
      return hasil.success(result.set_posisi(node.pos_mulai, node.pos_akhir))

  def visit_UnaryOpNode(self, node, isi):
    hasil = RTResult()
    number = hasil.register(self.visit(node.node, isi))
    if hasil.should_return(): return hasil

    error = None

    if node.token_operation.type == TT_MINUS:
      number, error = number.kali_dengan(Number(-1))
    elif node.token_operation.matches(TT_KEYWORD, 'tidak'):
      number, error = number.cek_tidak()

    if error:
      return hasil.failure(error)
    else:
      return hasil.success(number.set_posisi(node.pos_mulai, node.pos_akhir))

  def visit_IfNode(self, node, isi):
    hasil = RTResult()

    for condition, expr, should_return_null in node.cases:
      nilai_kondisi = hasil.register(self.visit(condition, isi))
      if hasil.should_return(): return hasil

      if nilai_kondisi.apakah_benar():
        nilai_expression = hasil.register(self.visit(expr, isi))
        if hasil.should_return(): return hasil
        return hasil.success(Number.null if should_return_null else nilai_expression)

    if node.else_case:
      expr, should_return_null = node.else_case
      nilai_expression = hasil.register(self.visit(expr, isi))
      if hasil.should_return(): return hasil
      return hasil.success(Number.null if should_return_null else nilai_expression)

    return hasil.success(Number.null)

  def visit_ForNode(self, node, isi):
    hasil = RTResult()
    elements = []

    start_value = hasil.register(self.visit(node.start_value_node, isi))
    if hasil.should_return(): return hasil

    end_value = hasil.register(self.visit(node.end_value_node, isi))
    if hasil.should_return(): return hasil

    if node.step_value_node:
      step_value = hasil.register(self.visit(node.step_value_node, isi))
      if hasil.should_return(): return hasil
    else:
      step_value = Number(1)

    i = start_value.value

    if step_value.value >= 0:
      kondisi = lambda: i < end_value.value
    else:
      kondisi = lambda: i > end_value.value
    
    while kondisi():
      isi.symbol_table.set(node.nama_variabel_token.value, Number(i))
      i += step_value.value

      value = hasil.register(self.visit(node.body_node, isi))
      if hasil.should_return() and hasil.loop_lanjut == False and hasil.loop_berhenti == False: return hasil
      
      if hasil.loop_lanjut:
        continue
      
      if hasil.loop_berhenti:
        break

      elements.append(value)

    return hasil.success(
      Number.null if node.should_return_null else
      List(elements).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    )

  def visit_WhileNode(self, node, isi):
    hasil = RTResult()
    elements = []

    while True:
      kondisi = hasil.register(self.visit(node.condition_node, isi))
      if hasil.should_return(): return hasil

      if not kondisi.apakah_benar():
        break

      value = hasil.register(self.visit(node.body_node, isi))
      if hasil.should_return() and hasil.loop_lanjut == False and hasil.loop_berhenti == False: return hasil

      if hasil.loop_lanjut:
        continue
      
      if hasil.loop_berhenti:
        break

      elements.append(value)

    return hasil.success(
      Number.null if node.should_return_null else
      List(elements).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    )

  def visit_FuncDefNode(self, node, isi):
    hasil = RTResult()

    nama_fungsi = node.nama_variabel_token.value if node.nama_variabel_token else None
    body_node = node.body_node
    arg_names = [arg_name.value for arg_name in node.nama_argumen_tokens]
    nilai_fungsi = Function(nama_fungsi, body_node, arg_names, node.should_auto_return).set_isi(isi).set_posisi(node.pos_mulai, node.pos_akhir)
    
    if node.nama_variabel_token:
      isi.symbol_table.set(nama_fungsi, nilai_fungsi)

    return hasil.success(nilai_fungsi)

  def visit_CallNode(self, node, isi):
    hasil = RTResult()
    args = []

    value_to_call = hasil.register(self.visit(node.node_to_call, isi))
    if hasil.should_return(): return hasil
    value_to_call = value_to_call.copy().set_posisi(node.pos_mulai, node.pos_akhir)

    for arg_node in node.arg_nodes:
      args.append(hasil.register(self.visit(arg_node, isi)))
      if hasil.should_return(): return hasil

    return_value = hasil.register(value_to_call.execute(args))
    if hasil.should_return(): return hasil
    return_value = return_value.copy().set_posisi(node.pos_mulai, node.pos_akhir).set_isi(isi)
    return hasil.success(return_value)

  def visit_ReturnNode(self, node, isi):
    hasil = RTResult()

    if node.node_to_return:
      value = hasil.register(self.visit(node.node_to_return, isi))
      if hasil.should_return(): return hasil
    else:
      value = Number.null
    
    return hasil.success_return(value)

  def visit_ContinueNode(self, node, isi):
    return RTResult().success_continue()

  def visit_BreakNode(self, node, isi):
    return RTResult().success_break()

#######################################
# RUN
#######################################

global_symbol_table = SymbolTable()
global_symbol_table.set("null", Number.null)
global_symbol_table.set("false", Number.false)
global_symbol_table.set("true", Number.true)
global_symbol_table.set("nilai_phi", Number.math_PI)
global_symbol_table.set("print", BuiltInFunction.print)
global_symbol_table.set("print_ret", BuiltInFunction.print_ret)
global_symbol_table.set("input", BuiltInFunction.input)
global_symbol_table.set("input_int", BuiltInFunction.input_int)
global_symbol_table.set("clear", BuiltInFunction.clear)
global_symbol_table.set("cls", BuiltInFunction.clear)
global_symbol_table.set("apakah_angka", BuiltInFunction.apakah_angka)
global_symbol_table.set("apakah_string", BuiltInFunction.apakah_string)
global_symbol_table.set("apakah_list", BuiltInFunction.apakah_list)
global_symbol_table.set("apakah_fungsi", BuiltInFunction.apakah_fungsi)
global_symbol_table.set("tambahkan_ke", BuiltInFunction.tambahkan_ke)
global_symbol_table.set("keluarkan_dari", BuiltInFunction.keluarkan_dari)
global_symbol_table.set("gabungkan", BuiltInFunction.gabungkan)
global_symbol_table.set("len", BuiltInFunction.len)
global_symbol_table.set("run", BuiltInFunction.run)

def run(nama_file, text):
  # Generate tokens
  lexer = Lexer(nama_file, text)
  tokens, error = lexer.membuat_token()
  if error: return None, error
  
  # Generate AST
  parser = Parser(tokens)
  ast = parser.parse()
  if ast.error: return None, ast.error

  # Run program
  interpreter = Interpreter()
  isi = Context('<program>')
  isi.symbol_table = global_symbol_table
  result = interpreter.visit(ast.node, isi)

  return result.value, result.error
