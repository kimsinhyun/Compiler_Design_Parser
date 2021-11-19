package MiniC.Parser;


import MiniC.Scanner.Token;
import MiniC.Scanner.SourcePos;
import MiniC.Parser.SyntaxError;
import MiniC.Scanner.Scanner;
import MiniC.ErrorReporter;

public class Parser {

  private Scanner scanner;
  private ErrorReporter errorReporter;
  private Token currentToken;

  public Parser(Scanner lexer, ErrorReporter reporter) {
    scanner = lexer;
    errorReporter = reporter;
  }

  // accept() checks whether the current token matches tokenExpected.
  // If so, it fetches the next token.
  // If not, it reports a syntax error.
  void accept (int tokenExpected) throws SyntaxError {
    if (currentToken.kind == tokenExpected) {
      currentToken = scanner.scan();
    } else {
      syntaxError("\"%\" expected here", Token.spell(tokenExpected));
    }
  }

  // acceptIt() unconditionally accepts the current token
  // and fetches the next token from the scanner.
  void acceptIt() {
    currentToken = scanner.scan();
  }

  void syntaxError(String messageTemplate, String tokenQuoted) throws SyntaxError {
    SourcePos pos = currentToken.GetSourcePos();
    errorReporter.reportError(messageTemplate, tokenQuoted, pos);
    throw(new SyntaxError());
  }

  boolean isTypeSpecifier(int token) {
    if (token == Token.VOID ||
      token == Token.INT  ||
      token == Token.BOOL ||
      token == Token.FLOAT) {
      return true;
    } else {
      return false;
    }
  }

  boolean isLiteral(int token) {
    if (token == Token.INTLITERAL ||
      token == Token.FLOATLITERAL  ||
      token == Token.STRINGLITERAL  ||
      token == Token.BOOLLITERAL) {
      return true;
    } else {
      return false;
    }
  }

  boolean isConditionLiteral(int token) {
    if (token == Token.EQ ||
      token == Token.NOTEQ  ||
      token == Token.LESSEQ  ||
      token == Token.GREATER  ||
      token == Token.GREATEREQ  ||
      token == Token.LESS) {
      return true;
    } else {
      return false;
    }
  }

  boolean isFirst_args(int token) {
    if (token == Token.INTLITERAL ||
      token == Token.FLOATLITERAL  ||
      token == Token.STRINGLITERAL  ||
      token == Token.BOOLLITERAL ||
      token == Token.PLUS ||
      token == Token.MINUS ||
      token == Token.TIMES ||
      token == Token.DIV ||
      token == Token.NOT ||
      token == Token.LEFTPAREN ||
      token == Token.ID) {
      return true;
    } else {
      return false;
    }
  }

  boolean isFirst_unary_expr(int token) {
    if (token == Token.INTLITERAL ||
      token == Token.FLOATLITERAL  ||
      token == Token.STRINGLITERAL  ||
      token == Token.BOOLLITERAL ||
      token == Token.PLUS ||
      token == Token.MINUS ||
      token == Token.NOT ||
      token == Token.LEFTPAREN ||
      token == Token.ID) {
      return true;
    } else {
      return false;
    }
  }
  boolean ifFirst_primary_expr(int token){
    if (token == Token.INTLITERAL ||
      token == Token.FLOATLITERAL  ||
      token == Token.STRINGLITERAL  ||
      token == Token.BOOLLITERAL ||
      token == Token.LEFTPAREN ||
      token == Token.ID) {
      return true;
    } else {
      return false;
    }
  }

  boolean isFirst_stmt(int token) {
    if (token == Token.LEFTBRACE ||
      token == Token.IF  ||
      token == Token.WHILE  ||
      token == Token.FOR ||
      token == Token.RETURN ||
      token == Token.ID) {
      return true;
    } else {
      return false;
    }
  }

  ///////////////////////////////////////////////////////////////////////////////
  //
  // toplevel parse() routine:
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parse() {

    currentToken = scanner.scan(); // get first token from scanner...

    try {
      parseProgram();
      if (currentToken.kind != Token.EOF) {
        syntaxError("\"%\" not expected after end of program",
            currentToken.GetLexeme());
      }
    }
    catch (SyntaxError s) {return; /* to be refined in Assignment 3...*/ }
    return;
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseProgram():
  //
  // program ::= ( (VOID|INT|BOOL|FLOAT) ID ( FunPart | VarPart ) )*
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseProgram() throws SyntaxError {
    while (isTypeSpecifier(currentToken.kind)) {
      acceptIt();
      accept(Token.ID);
      if(currentToken.kind == Token.LEFTPAREN) {
        parseFunPart();
      } else {
        parseVarPart();
      }
    }
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseFunPart():
  //
  // FunPart ::= ( "(" ParamsList? ")" CompoundStmt )
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseFunPart() throws SyntaxError {
    // We already know that the current token is "(".
    // Otherwise use accept() !
    accept(Token.LEFTPAREN);
    if (isTypeSpecifier(currentToken.kind)) {
      parseParamsList();
    }
    accept(Token.RIGHTPAREN);
    parseCompoundStmt();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseParamsList():
  //
  // ParamsList ::= ParamsDecl ( "," ParamsDecl ) *
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseParamsList() throws SyntaxError {
    // to be completed by you...
    parameter_decl();
    while(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      parameter_decl();
    }
  }
  private void parameter_decl() throws SyntaxError{
    if(isTypeSpecifier(currentToken.kind)){
      acceptIt();
    }
    declarator();
  }


  ///////////////////////////////////////////////////////////////////////////////
  //
  // parseCompoundStmt():
  //
  // CompoundStmt ::= "{" VariableDefinition* Stmt* "}"
  //
  ///////////////////////////////////////////////////////////////////////////////

  public void parseCompoundStmt() throws SyntaxError {
    // to be completed by you...
    accept(Token.LEFTBRACE);
    while(isTypeSpecifier(currentToken.kind)){
      acceptIt();
      accept(Token.ID);
      parseVarPart();
    }
    while(isFirst_stmt(currentToken.kind)){
      stmt();
    }
    accept(Token.RIGHTBRACE);
  } 

  private void stmt() throws SyntaxError{
    if(currentToken.kind == Token.LEFTBRACE){
      parseCompoundStmt();
    }
    else if(currentToken.kind == Token.IF){
      if_stmt();
    }
    else if(currentToken.kind == Token.WHILE){
      while_stmt();
    }
    else if(currentToken.kind == Token.FOR){
      for_stmt();
    }
    else if(currentToken.kind == Token.RETURN){
      accept(Token.RETURN);
      if(isFirst_args(currentToken.kind)){     //First(expr) = First(args)
        expr();
      }
      accept(Token.SEMICOLON);
    }
    else if(currentToken.kind == Token.ID){
      accept(Token.ID);
      if(currentToken.kind == Token.ASSIGN){
        accept(Token.ASSIGN);
        expr();
        accept(Token.SEMICOLON);
      }
      else if (currentToken.kind == Token.LEFTBRACKET){
        accept(Token.LEFTBRACKET);
        expr();
        accept(Token.RIGHTBRACKET);
        accept(Token.ASSIGN);
        expr();
        accept(Token.SEMICOLON);
      }
      else if(currentToken.kind == Token.LEFTPAREN){
        arglist();
        accept(Token.SEMICOLON);
      }
    }
  }
  private void if_stmt() throws SyntaxError{
    accept(Token.IF);
    accept(Token.LEFTPAREN);
    expr();
    accept(Token.RIGHTPAREN);
    stmt();
    if(currentToken.kind == Token.ELSE){
      accept(Token.ELSE);
      stmt();
    }
  }
  private void while_stmt() throws SyntaxError{
    accept(Token.WHILE);
    accept(Token.LEFTPAREN);
    expr();
    accept(Token.RIGHTPAREN);
    stmt();
  }
  private void for_stmt() throws SyntaxError{
    accept(Token.FOR);
    accept(Token.LEFTPAREN);
    if(currentToken.kind == Token.ID){
      asgnexpr();
    }
    accept(Token.SEMICOLON);
    if(isFirst_args(currentToken.kind)){
      expr();
    }
    accept(Token.SEMICOLON);
    if(currentToken.kind == Token.ID){
      asgnexpr();
    }
    accept(Token.RIGHTPAREN);
    stmt();
  }

  private void asgnexpr() throws SyntaxError{
    accept(Token.ID);
    accept(Token.ASSIGN);
    expr();
  }

  // VarPart ::= ( "[" INTLITERAL "]" )?  ( "=" initializer ) ? ( "," init_decl)* ";"
  public void parseVarPart() throws SyntaxError {
    if(currentToken.kind == Token.LEFTBRACKET){
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
    if(currentToken.kind == Token.ASSIGN){
      accept(Token.ASSIGN);
      initializer();
    }
    while(currentToken.kind == Token.COMMA){
      accept(Token.COMMA);
      init_decl();
    }
    accept(Token.SEMICOLON);


  }
  
  private void init_decl() throws SyntaxError{
    declarator();
    if(currentToken.kind  == Token.ASSIGN){
      accept(Token.ASSIGN);
      initializer();
    }
  }
 
  // declarator:==   ID | ID "[" INTLITERAL "]"
  private void declarator() throws SyntaxError{
    accept(Token.ID);
    if(currentToken.kind == Token.LEFTBRACKET){
      accept(Token.LEFTBRACKET);
      accept(Token.INTLITERAL);
      accept(Token.RIGHTBRACKET);
    }
  }

  // initializer ::=  expr |   "{" expr ( "," expr )*  "}"
  private void initializer() throws SyntaxError{
    if(currentToken.kind == Token.LEFTBRACE){       // | "{" expr ( "," expr )* "}"
      accept(Token.LEFTBRACE);
      expr();
      while(currentToken.kind == Token.COMMA){
        accept(Token.COMMA);
        expr();
      }
      accept(Token.RIGHTBRACE);
    }
    // expr
    else{
      expr();
    }
  }

  private void expr() throws SyntaxError{
    and_expr();
    while(currentToken.kind == Token.OR){
      accept(Token.OR);
      and_expr();
    }
  }

  private void and_expr() throws SyntaxError{
    relational_expr();
    while(currentToken.kind == Token.AND){
      accept(Token.AND);
      relational_expr();
    }
  }
  private void relational_expr() throws SyntaxError{
    add_expr();
    if(isConditionLiteral(currentToken.kind)){
      acceptIt();
      add_expr();
    }
  }

  private void add_expr() throws SyntaxError{
    mult_expr();
    while((currentToken.kind == Token.PLUS) || (currentToken.kind == Token.MINUS)){
      acceptIt();
      mult_expr();
    }
  }

  private void mult_expr() throws SyntaxError{
    if(isFirst_unary_expr(currentToken.kind)){
      unary_expr();
    }
    
    while(currentToken.kind == Token.TIMES || currentToken.kind == Token.DIV){
      acceptIt();
      unary_expr();
    }

  }
  private void unary_expr() throws SyntaxError{
    if((currentToken.kind == Token.PLUS) || (currentToken.kind == Token.MINUS) || (currentToken.kind == Token.NOT)){
      acceptIt();
      unary_expr();
    }
    else if(ifFirst_primary_expr(currentToken.kind)){
      primary_expr();
    }
  }
  private void primary_expr() throws SyntaxError{
    if(isLiteral(currentToken.kind)){
      acceptIt();
    }
    else if(currentToken.kind == Token.LEFTPAREN){      // "(" expr ")"
      accept(Token.LEFTPAREN);
      expr();
      accept(Token.RIGHTPAREN);
    }
    else if(currentToken.kind == Token.ID){
      accept(Token.ID);
      if (currentToken.kind == Token.LEFTBRACKET){      // ID "[" expr "]"
        accept(Token.LEFTBRACKET);
        expr();
        accept(Token.RIGHTBRACKET);
      }
      else if(currentToken.kind == Token.LEFTPAREN){      // ID arglist?   -> FISRT(arglist) = "("
        arglist();
      }
    } 
  }

  private void arglist() throws SyntaxError{
    accept(Token.LEFTPAREN);
    // first(args) = {+, -, *, /, !, ID, (, INTLITERAL, BOOLLITERAL, FLOATLITERAL,STRINGLITERAL)} 
    if(isFirst_args(currentToken.kind)){                //args?   FIRST(args)
      expr();
      while(currentToken.kind == Token.COMMA){
        accept(Token.COMMA);
        expr();
      }
    }
    accept(Token.RIGHTPAREN);
  }
}
