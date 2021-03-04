LEX=ml-lex
YACC=ml-yacc
SML=sml

lexer: PlcLexer.lex
	$(LEX) $<

grammar: PlcParser.yacc
	$(YACC) $<

all: testParser.sml lexer grammar
	$(SML) $<

clean:
	rm -f PlcLexer.lex.sml PlcParser.yacc.sig PlcParser.yacc.sml