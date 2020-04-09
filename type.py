#! /usr/bin/python3
#Authors:        Gavin Grossman 931000651 & Spencer Bao 931001734
#Date Created:  3/08/2020

import re, sys, string

debug = False
dict = { }
tokens = [ ]
statement_list = []


def type( text ):
	global tokens
	tokens = Lexer( text )
	statement_list = parseStmtList( tokens )

	type_map = Type_Map()
	# for statement in statement_list:
	# 	if isinstance(statement, AssignStatement):
	# 		type_map.put(str(statement.identifier), "")
    
	body = BlockStatement(statement_list)
	
	global error_message
	error_message = ""
	for stmt in body.stmtList:
		if error_message != "":
				break
		if (isinstance(stmt, AssignStatement) or isinstance(stmt, WhileStatement) or
			isinstance(stmt, IfStatement) or isinstance(stmt, BlockStatement)):
			stmt.tipe(type_map)
			

		else:
			raise ValueError("Invalid Statement")

	print(type_map)
	print(error_message)
	return

class Type_Map():
	def __init__(self):
		self.tm = {}

	def __str__(self):
		print_tm = ""
		counter = 0
		for i in self.tm:
			print_tm += str(i) + " " + str(self.tm[i])
			if counter < len(self.tm) - 1:
				print_tm += "\n"
			counter += 1
		return print_tm

	def put(self, variable, value):
		self.tm[variable] = value

	def val(self, ident):
		return self.tm[ident]

	def check_keys(self, key):
		if str(key) in self.tm:
			return True
		else:
			return False


class State(): # a dictionary that holds variables which map to its state
	def __init__(self):
		self.state = {}

	def put(self, variable, value):
		self.state[variable] = value

	def __str__(self):
		print_state = "{"
		comma_counter = 0
		for variable in self.state:
			print_state += ("<" + str(variable) + ", " + str(self.state[variable]) + ">")
			if comma_counter != len(self.state) - 1:
				print_state += ", "
				comma_counter += 1
		print_state += "}"	
		return print_state

	def val(self, ident):
		return self.state[ident]


def meaning( text ) :
	global tokens
	tokens = Lexer( text )
	statement_list = parseStmtList( tokens )
	decpart = State()
	for statement in statement_list:
		if isinstance(statement, AssignStatement):
			decpart.put(str(statement.identifier), "undef")

	# print(decpart)

	body = BlockStatement(statement_list)

	for stmt in body.stmtList:
		if (isinstance(stmt, AssignStatement) or isinstance(stmt, WhileStatement) or
			isinstance(stmt, IfStatement) or isinstance(stmt, BlockStatement)):
			stmt.meaning(decpart)
		else:
			raise ValueError("Invalid Statement")
		

	print(decpart)
	return
	#return body.meaning(decpart) #meaning(statement, state)



class Statement( object ): #statement superclass
	def __str__(self):
		return ""

class WhileStatement( Statement ): #statement subclasses they create the string representations for the different statements 
	def __init__(self, expr, block):
		self.expr = expr
		self.block = block
	
	def __str__(self):
		return "while " + str(self.expr) + '\n' + str(self.block) + 'endwhile'

	def meaning(self, state):
		while self.expr.val(state):
			self.block.meaning(state)
		return state

	def tipe(self, type_map):
		global error_message
		if self.expr.tipe(type_map) == "boolean":
			self.block.tipe(type_map)
			return type_map
		else:
			if error_message == "": #this checks if there was already an error in the expr and prints that message first
				error_message = TypeError("Type Error: While statement does not include a boolean expression")
			return type_map

class IfStatement( Statement ):
	def __init__(self, expr, if_block, else_block):
		self.expr = expr
		self.if_block = if_block
		self.else_block = else_block

	def __str__(self):
		else_state = 'else ' + '\n' + str(self.else_block)
		return "if " + str(self.expr) + '\n' + str(self.if_block) + else_state + "endif" 
	
	def meaning(self, state):
		if self.expr.val(state):
			self.if_block.meaning(state)
		else:
			self.else_block.meaning(state)
		return state

	def tipe(self, type_map):
		global error_message
		if self.expr.tipe(type_map) == "boolean":
			self.if_block.tipe(type_map)
			if self.else_block != "":
				self.else_block.tipe(type_map)
			return type_map
		else:
			if error_message == "":
				error_message = TypeError("Type Error: If statement does not include a boolean expression")

class AssignStatement( Statement ):
	def __init__(self, identifier, expr):
		self.identifier = identifier
		self.expr = expr
	
	def __str__(self):
		return "= " + str(self.identifier) + " " + str(self.expr)

	def meaning(self, state):
		state.put(str(self.identifier), self.expr.val(state))
		return state

	def tipe(self, type_map):
		global error_message
		expr_type = self.expr.tipe(type_map)
		if error_message != "":
			return type_map
		if expr_type == "":
			if error_message == "":
				error_message = NameError("Name Error: Variable undefined!")
		if type_map.check_keys(str(self.identifier)) != True:
			type_map.put(str(self.identifier), expr_type)
			return type_map
		elif type_map.val(str(self.identifier)) != expr_type:
			error_message = TypeError("Type Error: " + self.identifier.tipe(type_map) + " = " + self.expr.tipe(type_map) + "!")
			
			

class BlockStatement( Statement ):
	def __init__(self, stmtList):
		self.stmtList = stmtList
	
	def __str__(self):
		print_list = ""
		for stmt in self.stmtList:
			print_list += str(stmt) + "\n"
		return print_list

	def meaning(self, state):
		for stmt in self.stmtList:
			stmt.meaning(state)
		return state
	
	def tipe(self, type_map):
		for stmt in self.stmtList:
			stmt.tipe(type_map)
			return type_map
		

#  Expression class and its subclasses
class Expression( object ):
	def __str__(self):
		return "" 
	
class BinaryExpr( Expression ): # creates a binary tree since the expressions can derive many other expressions
	def __init__(self, op, left, right):
		self.op = op
		self.left = left
		self.right = right
		
	def __str__(self):
		return str(self.op) + " " + str(self.left) + " " + str(self.right)

	def val(self, state):
		left = int(self.left.val(state))
		right = int(self.right.val(state))
		if self.op == "+":
			return left + right
		if self.op == "-":
			return left - right
		if self.op == "*":
			return left * right
		if self.op == "/":
			return left / right
		if self.op == ">":
			return left > right
		if self.op == "<":
			return left < right
		if self.op == ">=":
			return left >= right
		if self.op == "<=":
			return left <= right
		if self.op == "==":
			return left == right
		if self.op == "!=":
			return left != right
			
	def tipe(self, type_map): #check type validity and return the type.
		global error_message
		# print(type_map)
		if type_map.check_keys(self.left) == False and re.match(Lexer.number, str(self.left)) == False:
			error_message = TypeError("Type Error: " + str(self.left) + " is referenced before being defined!")
			return ""
		# elif type_map.check_keys(self.right) == False and re.match(Lexer.number, str(self.right)) == False:
		elif self.right.tipe(type_map) != "number" and self.right.tipe(type_map) != "boolean":
			error_message = TypeError("Type Error: " + str(self.right) + " is referenced before being defined!")
			return ""
		elif self.left.tipe(type_map) != self.right.tipe(type_map):
			return TypeError(str(self.left.tipe(type_map)) + self.op + str(self.right.tipe(type_map)))
		elif (self.op == "+" or self.op == "-" or self.op == "*" or self.op == "/"):
			return "number"
		elif (self.op == ">" or self.op == "<" or self.op == ">=" or self.op == "<=" or self.op == "==" or self.op == "!="):
			return "boolean"
		else:
			error_message = ValueError("Invalid operator")

class Number( Expression ):
	def __init__(self, value):
		self.value = value
		
	def __str__(self):
		return str(self.value)

	def val(self, state):
		return self.value
    
	def tipe(self, type_map):
		return "number"

class String( Expression ):
	def __init__(self, string):
		self.string = string
		
	def __str__(self):
		return str(self.string)
    
class VarRef( Expression ):
	def __init__(self, identifier):
		self.identifier = identifier
		
	def __str__(self):
		return str(self.identifier)

	def val(self, state):
		return state.val(self.identifier)

	def tipe(self, type_map):
		if type_map.check_keys(self.identifier):
			return type_map.val(str(self.identifier))
		else:
			return NameError("Variable undefined!")

def error( msg ):
	sys.exit(msg)

# The "parse" function. This builds a list of tokens from the input string,
# and then hands it to a recursive descent parser for the PAL grammar.

def match(matchtok):
	tok = tokens.peek( )
	if (tok != matchtok): error("Expecting "+ matchtok)
	tokens.next( )
	return tok


def factor( ):
	""" factor     = number | string | ident |  "(" expression ")" """
	tok = tokens.peek( )
	if debug: print ("Factor: ", tok)
	if re.match(Lexer.number, tok): 
		expr = Number(tok)
		tokens.next( )
		return expr
	if re.match(Lexer.string, tok):
		expr = String(tok)
		tokens.next( )
		return expr
	if re.match(Lexer.identifier, tok):
		expr = VarRef(tok)
		tokens.next( )
		return expr
	if tok == "(":
		tokens.next( )  # or match( tok )
		expr = expression( )
		tokens.peek( )
		tok = match(")")
		return expr
	error("Invalid operand")
	return


def term( ):
	""" term    = factor { ('*' | '/') factor } """
	tok = tokens.peek( )
	if debug: print ("Term: ", tok)
	left = factor( )
	tok = tokens.peek( )
	while tok == "*" or tok == "/":
		tokens.next()
		right = factor( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def addExpr( ):
	""" addExpr    = term { ('+' | '-') term } """
	tok = tokens.peek( )
	if debug: print ("addExpr: ", tok)
	left = term( )
	tok = tokens.peek( )
	while tok == "+" or tok == "-": 
		tokens.next()
		right = term( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def relationalExpr( ):
	""" relationalExpr = addExpr [ relation addExpr ] """
	""" relation    = "==" | "!=" | "<" | "<=" | ">" | ">=" """
	tok = tokens.peek( )
	if debug: print ("relationalExpr: ", tok)
	left = addExpr( )
	tok = tokens.peek( )
	while tok == "==" or tok == "!=" or tok == "<" or tok == "<=" or tok == ">" or tok == ">=":
		tokens.next()
		right = addExpr( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def andExpr( ):
	""" andExpr    = relationalExpr { "and" relationalExpr } """
	tok = tokens.peek( )
	if debug: print ("andExpr: ", tok)
	left = relationalExpr( )
	tok = tokens.peek( )
	while tok == "and":
		tokens.next()
		right = relationalExpr( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def expression( ):
	""" expression = andExpr { "or" andExpr } """
	tok = tokens.peek( )
	if debug: print ("expression: ", tok)
	left = andExpr( )
	tok = tokens.peek( )
	while tok == "or":
		tokens.next()
		right = andExpr( )
		left = BinaryExpr(tok, left, right)
		tok = tokens.peek( )
	return left


def parseStmtList( tokens ):
	""" gee = { Statement } """
	stmtList = []
	tok = tokens.peek( )
	while tok is not None:
		stmtList.append(parseStatement(tok))
		tok = tokens.peek() 
	return stmtList


def parseStatement(token): 
	""" statement = parseIfStatement |  parseWhileStatement  |  parseAssign """
	if token == "if": 
		return parseIfStatement()
	elif token == "while":
		return parseWhileStatement()
	elif re.match(Lexer.identifier, token):
		return parseAssign()
	else:
		error("Invalid statement")


def parseIfStatement():
	""" ifStatement = "if" expression block   [ "else" block ] """
	tok = tokens.peek()
	if debug: print("ifstatement: ", tok)
	match("if") 
	expr = expression() 
	if_block = block()
	tok = tokens.peek()
	if tok == "else":
		match("else")
		else_block = block()
	else: 
		else_block = ""
	return IfStatement(expr, if_block, else_block) 


def parseWhileStatement(  ):
	""" whileStatement = "while"  expression  block """
	tok = tokens.peek()
	if debug: print("whilestatement: ", tok)
	match("while")
	expr = expression()
	while_block = block()
	return WhileStatement(expr, while_block)


def parseAssign(  ):
	""" assign = ident "=" expression  eoln """
	tok = tokens.peek()
	if debug: print("assign: ", tok) 
	if re.match(Lexer.identifier, tok):
		target = VarRef(tok)
		tokens.next()
	match("=")
	source = expression()
	match(";")
	return AssignStatement(target, source)


def block(  ):
	""" block = ":" eoln indent stmtList undent """
	tok = tokens.peek( )
	if debug: print ("block: ", tok)
	match(":")
	match(";")
	match("@")
	stmtList = []
	tok = tokens.peek( )
	while tok != "~":
		stmtList.append(parseStatement(tok))
		tok = tokens.peek()
	match("~")
	return BlockStatement(stmtList)


def parse( text ) :
	global tokens
	tokens = Lexer( text )
	statement_list = parseStmtList( tokens )
	print (BlockStatement(statement_list))
	return



# Lexer, a private class that represents lists of tokens from a Gee
# statement. This class provides the following to its clients:
#
#   o A constructor that takes a string representing a statement
#       as its only parameter, and that initializes a sequence with
#       the tokens from that string.
#
#   o peek, a parameterless message that returns the next token
#       from a token sequence. This returns the token as a string.
#       If there are no more tokens in the sequence, this message
#       returns None.
#
#   o removeToken, a parameterless message that removes the next
#       token from a token sequence.
#
#   o __str__, a parameterless message that returns a string representation
#       of a token sequence, so that token sequences can print nicely

class Lexer :
	
	# The constructor with some regular expressions that define Gee's lexical rules.
	# The constructor uses these expressions to split the input expression into
	# a list of substrings that match Gee tokens, and saves that list to be
	# doled out in response to future "peek" messages. The position in the
	# list at which to dole next is also saved for "nextToken" to use.
	
	special = r"\(|\)|\[|\]|,|:|;|@|~|;|\$"
	relational = "<=?|>=?|==?|!="
	arithmetic = "\+|\-|\*|/"
	#char = r"'."
	string = r"'[^']*'" + "|" + r'"[^"]*"'
	number = r"\-?\d+(?:\.\d+)?"
	literal = string + "|" + number
	#idStart = r"a-zA-Z"
	#idChar = idStart + r"0-9"
	#identifier = "[" + idStart + "][" + idChar + "]*"
	identifier = "[a-zA-Z]\w*"
	lexRules = literal + "|" + special + "|" + relational + "|" + arithmetic + "|" + identifier
	
	def __init__( self, text ) :
		self.tokens = re.findall( Lexer.lexRules, text ) #re.findall(pattern, string) - returns all non-overlapping matches
		self.position = 0
		self.indent = [ 0 ]
	

	# The peek method. This just returns the token at the current position in the
	# list, or None if the current position is past the end of the list.
	
	def peek( self ) :
		if self.position < len(self.tokens) :
			return self.tokens[ self.position ]
		else :
			return None
	
	
	# The removeToken method. All this has to do is increment the token sequence's
	# position counter.
	
	def next( self ) :
		self.position = self.position + 1
		return self.peek( )
	
	
	# An "__str__" method, so that token sequences print in a useful form.
	
	def __str__( self ) :
		return "<Lexer at " + str(self.position) + " in " + str(self.tokens) + ">"


def chkIndent(line):
	ct = 0
	for ch in line:
		if ch != " ": return ct
		ct += 1
	return ct

def delComment(line):
	pos = line.find("#")
	if pos > -1:
		line = line[0:pos]
		line = line.rstrip()
	return line

def mklines(filename):
	inn = open(filename, "r")
	lines = [ ]
	pos = [0]
	ct = 0
	for line in inn:
		ct += 1
		line = line.rstrip( )+";"
		line = delComment(line)
		if len(line) == 0 or line == ";": continue
		indent = chkIndent(line)
		line = line.lstrip( )
		if indent > pos[-1]:
			pos.append(indent)
			line = '@' + line
		elif indent < pos[-1]:
			while indent < pos[-1]:
				del(pos[-1])
				line = '~' + line
		print (ct, "\t", line)
		lines.append(line)
	undent = ""
	for i in pos[1:]:
		undent += "~"
	lines.append(undent)
	return lines


def main():
	"""main program for testing"""
	global debug
	ct = 0
	for opt in sys.argv[1:]:
		if opt[0] != "-": break
		ct = ct + 1
		if opt == "-d": debug = True
	if len(sys.argv) < 2+ct:
		print ("Usage:  %s filename" % sys.argv[0])
		return
	#parse("".join(mklines(sys.argv[1+ct])))
	#meaning("".join(mklines(sys.argv[1+ct])))
	type("".join(mklines(sys.argv[1+ct])))
	return


main()
