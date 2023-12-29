//
// Analyzer for simple C programs.  This component performs
// semantic analysis, in particular collecting variable
// names and their types. The analysis also checks to ensure
// variable names are unique --- no duplicates.
//
// If all is well, a "symbol table" is built and returned,
// containing all variables and their types. A symbol table
// is a list of tuples of the form (name, type).  Example:
//
//   [("x", "int"); ("y", "int"); ("z", "real")]
//
// Modified by:
//   Sharva Thakur
//   UIN :- 654135206
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module analyzer =
  //
  // NOTE: all functions in the module must be indented.
  //



  let private matchToken expected_token (tokens : string list) =
    //
    // if the next token matches the expected token,  
    // keep parsing by returning the rest of the tokens.
    // Otherwise throw an exception because there's a 
    // syntax error, effectively stopping compilation
    // at the first error.
    //
    let next_token = List.head tokens

    if expected_token = next_token then  
      List.tail tokens
    else if next_token.StartsWith (expected_token) then
      List.tail tokens
    else
      failwith ("expecting " + expected_token + ", but found " + next_token)

    

  // 
  // is_empty
  //
  // This function handles if the code is empty
  // 
  let private is_empty tokens = 
    let t1 = matchToken ";" tokens
    t1


  //
  // vardecl
  //
  // This function handles variable decreation (int i;)
  //
  let private vardecl tokens symtab  = 
    let a = List.head tokens
    // check if it is int
    if a = "int" then 
      let t1 = matchToken "int" tokens
      let next_token = List.head t1
      // check if there is an identifier
      if next_token.StartsWith ("identifier") then
        let a = next_token.Split(':') 
        let b = ( a[1], "int")
        if List.contains (a[1], "real") symtab || List.contains b symtab then 
          failwith ("redefinition of variable '" + a[1] + "'")
        let symtab2 = b :: symtab
        let t2 =  matchToken "identifier" t1
        let t3 = matchToken ";" t2
        (t3, symtab2)
      else 
        failwith ("expecting identifier or literal, but found " + next_token )

    else if a = "real" then 
      let t1 = matchToken "real" tokens

      let next_token = List.head t1
      // check if there is an identifier
      if next_token.StartsWith ("identifier") then
        let a = next_token.Split(':') 
        let b = ( a[1], "real")
        if List.contains (a[1], "int") symtab || List.contains b symtab then 
          failwith ("redefinition of variable '" + a[1] + "'")
        let symtab2 = b :: symtab
        let t2 =  matchToken "identifier" t1
        let t3 = matchToken ";" t2
        (t3, symtab2)
      else 
        failwith ("expecting identifier or literal, but found " + next_token )

    else 
      failwith ("expecting identifier or literal, but found " + a)

  //
  // input
  //
  // This function handles the input cases (cin >> i;)
  //
  let private input tokens = 
    let t1 = matchToken "cin" tokens
    let t2 = matchToken ">>" t1
    let t3 = matchToken "identifier" t2
    let t4= matchToken ";" t3
    t4 

  // 
  // expr_value
  //
  // This function handles the expr_value and checks for idetifiers, int literals, string lierals and boolean
  //
  let private expr_value tokens = 
    let (next_token: string) = List.head tokens

    if next_token.StartsWith ("identifier") then
      let t2 =  matchToken "identifier" tokens
      t2
    else if next_token.StartsWith ("int_literal") then 
      let t2 =  matchToken "int_literal" tokens
      t2
    else if next_token.StartsWith ("str_literal") then 
      let t2 =  matchToken "str_literal" tokens
      t2
    else if next_token.StartsWith ("real_literal") then 
      let t2 =  matchToken "real_literal" tokens
      t2
    else if next_token = "true" then 
      let t2 =  matchToken "true" tokens
      t2
    else if next_token = "false" then 
      let t2 =  matchToken "false" tokens
      t2
    else 
      failwith ("expecting identifier or literal, but found " + next_token)

    
    



  //
  // output_value
  //
  // This function handles the output values which can be idetifers, string, int, boolaen or endl
  //
  let private output_value tokens = 
    let a = List.head tokens
    if a = "endl" then
      let t1 = matchToken "endl" tokens
      t1
    else 
      let t1 = expr_value tokens
      t1
    
  //
  // output
  //
  // This function handles the cout function
  //
  let private output tokens = 
    let t1 = matchToken "cout" tokens
    let t2 = matchToken "<<" t1
    let t3 = output_value t2
    let t4= matchToken ";" t3
    t4


  //
  // expr_op
  //
  // This function handles the all the operators for the conditions 
  //
  let private expr_op tokens = 
    let a = List.head tokens
    if a = "+" then 
      let t1 = matchToken "+" tokens
      t1
    else if a = "-" then
      let t1 = matchToken "-" tokens
      t1
    else if a = "*" then
      let t1 = matchToken "*" tokens
      t1
    else if a = "/" then
      let t1 = matchToken "/" tokens
      t1
    else if a = "^" then
      let t1 = matchToken "^" tokens
      t1
    else if a = "<" then
      let t1 = matchToken "<" tokens
      t1
    else if a = "<=" then
      let t1 = matchToken "<=" tokens
      t1
    else if a = ">" then
      let t1 = matchToken ">" tokens
      t1
    else if a = ">=" then
      let t1 = matchToken ">=" tokens
      t1
    else if a = "==" then
      let t1 = matchToken "==" tokens
      t1
    else if a = "!=" then
      let t1 = matchToken "!=" tokens
      t1
    else 
      failwith ("expecting expression operator, but found " + a)



  //
  // expr
  //
  // This function handles all the experssion or checks if it is just one ecpression or an asssignment
  //
  let private expr tokens = 
    let t1 = expr_value tokens
    let a = List.head t1
    if a = "+" || a = "-" || a = "*" || a = "/" || a = "^" || a = "<" || a = "<=" || a = ">" || a = ">=" || a = "==" || a = "!=" then
      let t2 = expr_op t1
      let t3  = expr_value t2
      t3
    else 
      t1


  //
  // assignment
  //
  // This function handles the assignment like i = 0 or i = 0 + 1
  //
  let private assignment tokens =
    let t1 =  matchToken "identifier" tokens
    let t2 = matchToken "=" t1
    let t3 = expr t2
    let t4= matchToken ";" t3
    t4

  //
  // condition
  //
  // This checks for the condition
  //
  let private condition tokens = 
    let t1 = expr tokens
    t1 



  //
  // stmt
  //
  // This function handles all the statments and calls their respective functions
  //
  let rec private stmt tokens symtab  = 
    let a = List.head tokens

    if a = ";" then
      let t1 =  is_empty tokens
      (t1, symtab)
    else if a = "int" then
      let (t1, symtab1) = vardecl tokens symtab
      (t1, symtab1)
    else if a = "real" then 
      let (t1, symtab1) = vardecl tokens symtab
      (t1, symtab1)    
    else if a = "cin" then
      let t1 = input tokens
      (t1, symtab)
    else if a = "cout" then
      let t1 = output tokens
      (t1, symtab)
    else if a.StartsWith ("identifier") then
      let t1 = assignment tokens
      (t1 ,symtab)
    else if a = "if" then
      ifstmt tokens symtab
    else
      failwith ("expecting statement, but found " + a)

    

  //
  // then_part
  //
  // This function hadles the then part after if stmt
  //
  and private then_part tokens symtab = 
    let (t1, symtab1) = stmt tokens symtab
    (t1 ,symtab1)

  //
  // else_part
  //
  // This function handles the else condition after if 
  //
  and private else_part tokens symtab = 
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens // match and discard “else”
      stmt T2 symtab// parse the stmt with remaining tokens
    else
      (tokens, symtab) // EMPTY is legal, so do nothing and return tokens unchanged 

  //
  // ifstmt
  //
  // This function handles the if confition
  //
  and private ifstmt tokens symtab = 
    let t1 = matchToken "if" tokens 
    let t2 = matchToken "(" t1
    let t3 = condition t2
    let t4 = matchToken ")" t3
    let (t5,symtab1) = then_part t4 symtab
    let (t6,symtab2) = else_part t5 symtab1
    (t6, symtab2)


  //
  // morestmts
  //
  // This function handles the statements after stmt
  //    
  let rec private morestmts tokens symtab  = 
    let a = List.head tokens

    if a <> "}" then
      let (t1, symtab1) = stmt tokens symtab
      let (t2,symtab2) = morestmts t1 symtab1
      (t2, symtab2)
    else
      (tokens , symtab)


  //
  // stmts
  //
  // This function is called inside simpleC to check for statemnts
  //
  let private stmts tokens symtab = 
    let (T1,symtab1) = stmt tokens symtab
    let (T2,symtab2) = morestmts T1 symtab1
    (T2 ,symtab2)

  let private simpleC tokens = 
    let symtab = []
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7,symtab1) = stmts T6 symtab
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
    (T9,symtab1)


  //
  // build_symboltable tokens
  //
  // Given a list of tokens, analyzes the program by looking
  // at variable declarations and collecting them into a
  // list. This list is known as a symbol table. Returns
  // a tuple (result, symboltable), where result is a string 
  // denoting "success" if valid, otherwise a string of the 
  // form "semantic_error:...".
  //
  // On success, the symboltable is a list of tuples of the
  // form (name, type), e.g. [("x","int"); ("y","real")]. On 
  // an error, the returned list is empty [].
  //
  let build_symboltable tokens = 
    try
      let (T2, symboltable) = simpleC tokens
      ("success", symboltable)
    with 
      | ex -> ("semantic_error: " + ex.Message, [])
