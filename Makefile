LEX=ml-lex
YACC=ml-yacc
SML=sml

lexer: PlcLexer.lex
	$(LEX) $<

grammar: PlcParser.yacc
	$(YACC) $<

interpTest: InterpTests.sml Plc.sml lexer grammar
	$(SML) < $<

checkerTest: CheckTests.sml Plc.sml lexer grammar
	$(SML) < $<

test: testParser.sml lexer grammar
	$(SML) < $<

all: Plc.sml lexer grammar
	$(SML) $<

clean:
	rm -f PlcLexer.lex.sml PlcParser.yacc.sig PlcParser.yacc.sml