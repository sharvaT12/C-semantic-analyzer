     //
// Analyzer for simple C programs.  This component performs
// type checking.  The analyzer returns a string denoting
// success or failure. The string "success" if the input 
// program is legal, otherwise the string "type_error: ..." 
// is returned denoting an invalid simple C program.
//
// Modified by:
//   Sharva Thakur
//   654135206
//
// Original author:
//   Prof. Joe Hummel
//   U. of Illinois, Chicago
//   CS 341, Spring 2022
//

namespace compiler

module checker =
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
  let private vardecl tokens  = 
    let a = List.head tokens
    // check if it is int
    if a = "int" then 
      let t1 = matchToken "int" tokens
      let next_token = List.head t1
      // check if there is an identifier
      if next_token.StartsWith ("identifier") then
        let t2 =  matchToken "identifier" t1
        let t3 = matchToken ";" t2
        t3
      else 
        failwith ("expecting identifier or literal, but found " + next_token )
    // for real
    else if a = "real" then 
      let t1 = matchToken "real" tokens

      let next_token = List.head t1
      // check if there is an identifier
      if next_token.StartsWith ("identifier") then
        let t2 =  matchToken "identifier" t1
        let t3 = matchToken ";" t2
        t3
      else 
        failwith ("expecting identifier or literal, but found " + next_token )

    else 
      failwith ("expecting identifier or literal, but found " + a)

  //
  // input
  //
  // This function handles the input cases (cin >> i;)
  //
  let private input tokens symboltable = 
    let t1 = matchToken "cin" tokens
    let t2 = matchToken ">>" t1
    let b =  List.head t2
    let a = b.Split(':') 
    if List.contains (a[1], "int") symboltable = false && List.contains (a[1], "real") symboltable = false then
      failwith ("variable '" + a[1] + "' undefined")
    let t3 = matchToken "identifier" t2
    let t4= matchToken ";" t3
    t4 

  // 
  // expr_value
  //
  // This function handles the expr_value and checks for idetifiers, int literals, string lierals and boolean
  // This also returns the type
  //
  let private expr_value tokens symboltable = 
    let (next_token: string) = List.head tokens

    if next_token.StartsWith ("identifier") then
      let t2 =  matchToken "identifier" tokens
      let b =  List.head tokens
      let a = b.Split(':') 
      if List.contains (a[1], "int") symboltable then
        (t2, "int")
      else if List.contains (a[1], "real") symboltable then
        (t2, "real")
      else
        failwith ("variable '" + a[1] + "' undefined") // if not found in list

    else if next_token.StartsWith ("int_literal") then 
      let t2 =  matchToken "int_literal" tokens
      (t2, "int")
    else if next_token.StartsWith ("str_literal") then 
      let t2 =  matchToken "str_literal" tokens
      (t2, "str")
    else if next_token.StartsWith ("real_literal") then 
      let t2 =  matchToken "real_literal" tokens
      (t2, "real")
    else if next_token = "true" then 
      let t2 =  matchToken "true" tokens
      (t2, "bool")
    else if next_token = "false" then 
      let t2 =  matchToken "false" tokens
      (t2, "bool")
    else 
      failwith ("expecting identifier or literal, but found " + next_token)

    
    



  //
  // output_value
  //
  // This function handles the output values which can be idetifers, string, int, boolaen or endl
  //
  let private output_value tokens symboltable = 
    let a = List.head tokens
    if a = "endl" then
      let t1 = matchToken "endl" tokens
      t1
    else 
      let (t1, a) = expr_value tokens symboltable
      t1
    
  //
  // output
  //
  // This function handles the cout function
  //
  let private output tokens symboltable = 
    let t1 = matchToken "cout" tokens
    let t2 = matchToken "<<" t1
    let t3 = output_value t2 symboltable
    let t4= matchToken ";" t3
    t4


  //
  // expr_op
  //
  // This function handles the all the operators for the conditions and if the conditions are legal or not
  //
  let private expr_op tokens left_type right_type = 
    let a = List.head tokens
    
    

    if a = "+" then 
      if left_type <> "int" && left_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> "int" && right_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "+" tokens
      t1
    else if a = "-" then
      if left_type <> "int" && left_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> "int" && right_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "-" tokens
      t1
    else if a = "*" then
      if left_type <> "int" && left_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> "int" && right_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "*" tokens
      t1
    else if a = "/" then
      if left_type <> "int" && left_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> "int" && right_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "/" tokens
      t1
    else if a = "^" then
      if left_type <> "int" && left_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> "int" && right_type <> "real" then 
        failwith ("operator " + a + " must involve 'int' or 'real'")
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "^" tokens
      t1
    else if a = "<" then
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "<" tokens
      t1
    else if a = "<=" then
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken "<=" tokens
      t1
    else if a = ">" then
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken ">" tokens
      t1
    else if a = ">=" then
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")
      let t1 = matchToken ">=" tokens
      t1  
    else if a = "==" then
      // printfn "%s \n" left_type
      // printfn "%s \n" right_type
      if right_type <> left_type then 
        failwith ("type mismatch '" + left_type + "' " + a + " '" + right_type + "'")

      if left_type = "real" || right_type = "real" then 
        printfn "warning: comparing real numbers with == may never be true"
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
  let private expr tokens symboltable flag = 
    let (t1 , left) = expr_value tokens symboltable
    let a = List.head t1

    

    if a = "+" || a = "-" || a = "*" || a = "/" || a = "^" || a = "<" || a = "<=" || a = ">" || a = ">=" || a = "==" || a = "!=" then
      let (temp, right)  = expr_value (List.tail t1) symboltable

      if a = "<" || a = "<=" || a = ">" || a = ">=" || a = "==" || a = "!="  then
        if (left <> right) then
          failwith ("type mismatch '" + left + "' " + a + " '" + right + "'")

      if flag = "int" || flag = "real" then 
        if a = "<" || a = "<=" || a = ">" || a = ">=" || a = "==" || a = "!=" then
          failwith ("cannot assign '" + "bool" + "' to variable of type '" + flag  + "'") 

      if flag = "c" then 
        if a <> "<" && a <> "<=" && a <> ">" && a <> ">=" && a <> "==" && a <> "!=" then
          failwith ("if condition must be 'bool', but found '" + left + "'")
        

      
      let t2 = expr_op t1 left right
      let (t3, r)  = expr_value t2 symboltable

      t3
    else if flag = "c" && left <> "bool" then 
       failwith ("if condition must be 'bool', but found '" + left + "'")
    else 
      t1


  //
  // assignment
  //
  // This function handles the assignment like i = 0 or i = 0 + 1
  //
  let private assignment tokens symboltable =
  
    let t1 =  matchToken "identifier" tokens
    let b =  List.head tokens
    let a = b.Split(':') 
    if List.contains (a[1], "int") symboltable = false && List.contains (a[1], "real") symboltable = false then
      failwith ("variable '" + a[1] + "' undefined")
    
    

    

    let t2 = matchToken "=" t1


    let b =  List.head t2
    let c = b.Split('_') 

    //printfn "%s \n" c[0]

    

    if(List.contains(a[1], "int") symboltable) then 
      let t3 = expr t2 symboltable "int"
      if (c[0].StartsWith("identifier")) then 
        let s = c[0].Split(':')

        try
          let (q,e) = List.find (fun (x,y) -> x = s[1]) symboltable
          if (c[0] <> "int" && e<> "int") then
            failwith ("cannot assign '" + c[0] + "' to variable of type '" + "int" + "'")
        with
          | ex -> failwith("variable '" + s[1] + "' undefined")
      else 
        if(c[0] <> "int" ) then 
          if c[0] = "true" || c[0] = "false" then 
            failwith ("cannot assign '" + "bool" + "' to variable of type '" + "int" + "'")
          else 
            failwith ("cannot assign '" + c[0] + "' to variable of type '" + "int" + "'")
      let t4= matchToken ";" t3
      (t4,symboltable)
    else
      let t3 = expr t2 symboltable "real"
      if (c[0].StartsWith("identifier")) then 
        
        let s = c[0].Split(':')

        try
          let (q,e) = List.find (fun (x,y) -> x = s[1]) symboltable
          if (c[0] <> "real" && e<> "real" && c[0] <> "int" && e<> "int") then
            failwith ("cannot assign '" + c[0] + "' to variable of type '" + "real" + "'")
        
        with
          | ex -> failwith("variable '" + s[1] + "' undefined")
      else 
        if(c[0] <> "int" && c[0] <> "real" ) then 
          if c[0] = "true" || c[0] = "false" then 
            failwith ("cannot assign '" + "bool" + "' to variable of type '" + "real" + "'")
          else 
            failwith ("cannot assign '" + c[0] + "' to variable of type '" + "real" + "'")
      let t4= matchToken ";" t3
      (t4,symboltable)
    
   

   

    

  //
  // condition
  //
  // This checks for the condition
  //
  let private condition tokens symboltable = 
    let t1 = expr tokens symboltable "c"
    t1 



  //
  // stmt
  //
  // This function handles all the statments and calls their respective functions
  //
  let rec private stmt tokens symboltable = 
    let a = List.head tokens

    if a = ";" then
      let t1 =  is_empty tokens
      (t1, symboltable)
    else if a = "int" then
      let t1 = vardecl tokens
      (t1, symboltable)
    else if a = "real" then 
      let t1 = vardecl tokens
      (t1, symboltable)    
    else if a = "cin" then
      let t1 = input tokens symboltable
      (t1, symboltable)
    else if a = "cout" then
      let t1 = output tokens symboltable
      (t1, symboltable)
    else if a.StartsWith ("identifier") then
      let (t1, symboltable) = assignment tokens symboltable
      (t1, symboltable)
    else if a = "if" then
      ifstmt tokens symboltable
    else
      failwith ("expecting statement, but found " + a)

    

  //
  // then_part
  //
  // This function hadles the then part after if stmt
  //
  and private then_part tokens symboltable = 
    let (t1,symboltable) = stmt tokens symboltable
    (t1, symboltable)

  //
  // else_part
  //
  // This function handles the else condition after if 
  //
  and private else_part tokens symboltable = 
    let next_token = List.head tokens

    if next_token = "else" then
      let T2 = matchToken "else" tokens // match and discard “else”
      let (T3, symboltable) = stmt T2 symboltable// parse the stmt with remaining tokens
      (T3,symboltable)
    else
      (tokens, symboltable)// EMPTY is legal, so do nothing and return tokens unchanged 

  //
  // ifstmt
  //
  // This function handles the if confition
  //
  and private ifstmt tokens symboltable = 
    let t1 = matchToken "if" tokens 
    let t2 = matchToken "(" t1
    let t3 = condition t2 symboltable
    let t4 = matchToken ")" t3
    let (t5, symboltable) = then_part t4 symboltable
    let (t6,symboltable) = else_part t5 symboltable
    (t6, symboltable)


  //
  // morestmts
  //
  // This function handles the statements after stmt
  //    
  let rec private morestmts tokens symboltable = 
    let a = List.head tokens

    if a <> "}" then
      let (t1, symboltable) = stmt tokens symboltable
      let (t2 ,symboltable) = morestmts t1 symboltable
      (t2, symboltable)
    else
      (tokens, symboltable)


  //
  // stmts
  //
  // This function is called inside simpleC to check for statemnts
  //
  let private stmts tokens symboltable = 
    let (T1, symboltable) = stmt tokens symboltable
    let (T2, symboltable) = morestmts T1 symboltable
    (T2, symboltable)


  let private simpleC tokens symboltable = 
    let T2 = matchToken "void" tokens
    let T3 = matchToken "main" T2
    let T4 = matchToken "(" T3
    let T5 = matchToken ")" T4
    let T6 = matchToken "{" T5
    let (T7, symboltable) = stmts T6 symboltable
    let T8 = matchToken "}" T7
    let T9 = matchToken "$" T8 // $ => EOF, there should be no more tokens
    T9        


  //
  // typecheck tokens symboltable
  //
  // Given a list of tokens and a symbol table, type-checks 
  // the program to ensure program's variables and expressions
  // are type-compatible. If the program is valid, returns 3
  // the string "success". If the program contains a semantic
  // error or warning, returns a string of the form
  // "type_error: ...".
  //
  let typecheck tokens symboltable = 
    try
      let T2 = simpleC tokens symboltable
      "success"
    with 
      | ex -> "type_error: " + ex.Message

