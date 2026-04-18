open System
open System.IO
open System.Text.RegularExpressions

// ==========================================
// 1. АБСТРАКТНОЕ СИНТАКСИЧЕСКОЕ ДЕРЕВО (AST)
// ==========================================
type Expr =
    | Num of int
    | Str of string
    | Bool of bool
    | Var of string
    | List of Expr list
    | Assign of string * Expr
    | Func of string * string list * Expr
    | Call of string * Expr list
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | Block of Expr list

type Value =
    | VNum of int
    | VStr of string
    | VBool of bool
    | VList of Value list
    | VClosure of string * string list * Expr * ref<Map<string, Value>>
    | VNative of (Value list -> Value)

and Environment = Map<string, Value>

// ==========================================
// 2. ЛЕКСЕР
// ==========================================
let tokenize (code: string) =
    let pattern = @"\""[^\n]*?\""|[a-zA-Z_][a-zA-Z0-9_]*|\d+|==|<=|>=|<|>|[{}()=+\-*/,\[\]]"
    [ for m in Regex.Matches(code, pattern) -> m.Value ]

// ==========================================
// 3. РУЧНОЙ ПАРСЕР
// ==========================================
let rec parseExpr toks =
    let e1, t1 = parseTerm toks
    let rec loop left ts =
        match ts with
        | op :: rest when op = "+" || op = "-" || op = "*" || op = "==" || op = "<" || op = ">" ->
            let right, nextTs = parseTerm rest
            loop (BinOp(op, left, right)) nextTs
        | _ -> left, ts
    loop e1 t1

and parseTerm toks =
    match toks with
    | "if" :: t1 ->
        let cond, t2 = parseExpr t1
        let trueBr, t3 = parseBlock t2
        match t3 with
        | "else" :: t4 ->
            let falseBr, t5 = parseBlock t4
            If(cond, trueBr, falseBr), t5
        | _ -> If(cond, trueBr, Block []), t3
        
    | "func" :: name :: "(" :: t1 ->
        let rec parseParams ts acc =
            match ts with
            | ")" :: rest -> List.rev acc, rest
            | "," :: rest -> parseParams rest acc
            | p :: rest -> parseParams rest (p :: acc)
            // ИСПРАВЛЕНИЕ: Добавлена обработка внезапного конца файла в аргументах
            | [] -> failwith "Ожидалась закрывающая скобка ')' в аргументах функции"
            
        let args, t2 = parseParams t1 []
        let body, t3 = parseBlock t2
        Func(name, args, body), t3
        
    | name :: "=" :: t1 ->
        let expr, t2 = parseExpr t1
        Assign(name, expr), t2
        
    | name :: "(" :: t1 ->
        let rec parseArgs ts acc =
            match ts with
            | ")" :: rest -> List.rev acc, rest
            | "," :: rest -> parseArgs rest acc
            | _ ->
                let e, rest = parseExpr ts
                parseArgs rest (e :: acc)
        let args, t2 = parseArgs t1 []
        Call(name, args), t2
        
    | "[" :: t1 ->
        let rec parseList ts acc =
            match ts with
            | "]" :: rest -> List (List.rev acc), rest
            | "," :: rest -> parseList rest acc
            | _ ->
                let e, rest = parseExpr ts
                parseList rest (e :: acc)
        parseList t1 []
        
    | "(" :: t1 ->
        let e, t2 = parseExpr t1
        match t2 with
        | ")" :: t3 -> e, t3
        | _ -> failwith "Ожидалась закрывающая скобка ')'"
        
    | "{" :: _ as t1 -> 
        parseBlock t1
        
    | "true" :: t1 -> Bool true, t1
    | "false" :: t1 -> Bool false, t1
    | str :: t1 when str.StartsWith("\"") -> Str (str.Trim('"')), t1
    | num :: t1 when Char.IsDigit(num.[0]) -> Num (int num), t1
    | var :: t1 when Char.IsLetter(var.[0]) || var.[0] = '_' -> Var var, t1
    | [] -> failwith "Неожиданный конец файла"
    | tok :: _ -> failwithf "Синтаксическая ошибка: %s" tok

and parseBlock toks =
    match toks with
    | "{" :: t1 ->
        let rec parseStmts ts acc =
            match ts with
            | "}" :: rest -> Block (List.rev acc), rest
            | _ ->
                let e, rest = parseExpr ts
                parseStmts rest (e :: acc)
        parseStmts t1 []
    | _ -> failwith "Ожидалось начало блока '{'"

let parseProgram toks =
    let rec loop ts acc =
        if List.isEmpty ts then Block (List.rev acc)
        else
            let e, rest = parseExpr ts
            loop rest (e :: acc)
    loop toks []

// ==========================================
// 4. ВЫЧИСЛИТЕЛЬ (EVALUATOR)
// ==========================================
let rec eval (env: Environment) (expr: Expr) : Value * Environment =
    match expr with
    | Num n -> VNum n, env
    | Str s -> VStr s, env
    | Bool b -> VBool b, env
    | Var x -> Map.find x env, env
    
    | List exprs ->
        let vals = List.map (fun e -> fst (eval env e)) exprs
        VList vals, env
    
    | Assign (name, e) ->
        let v, _ = eval env e
        v, Map.add name v env
        
    | Func (name, args, body) ->
        let envRef = ref env
        let closure = VClosure(name, args, body, envRef)
        let newEnv = Map.add name closure env
        envRef.Value <- newEnv
        closure, newEnv
        
    | Call (fName, argsExpr) ->
        let argsValues = List.map (fun e -> fst (eval env e)) argsExpr
        match Map.find fName env with
        | VNative f -> f argsValues, env
        | VClosure (_, argNames, body, cEnvRef) ->
            let callEnv = List.fold2 (fun acc name v -> Map.add name v acc) cEnvRef.Value argNames argsValues
            let res, _ = eval callEnv body
            res, env
        | _ -> failwith "Это не функция!"
        
    | If (cond, tBranch, fBranch) ->
        let cVal, _ = eval env cond
        match cVal with
        | VBool true -> eval env tBranch
        | VBool false -> eval env fBranch
        | _ -> failwith "Условие в if должно быть булевым"
        
    | BinOp (op, e1, e2) ->
        let v1, _ = eval env e1
        let v2, _ = eval env e2
        match op, v1, v2 with
        | "+", VNum a, VNum b -> VNum (a + b), env
        | "-", VNum a, VNum b -> VNum (a - b), env
        | "*", VNum a, VNum b -> VNum (a * b), env
        | "==", VNum a, VNum b -> VBool (a = b), env
        | "<", VNum a, VNum b -> VBool (a < b), env
        | ">", VNum a, VNum b -> VBool (a > b), env
        | _ -> failwith "Ошибка операции"
        
    | Block exprs ->
        let rec evalStmts es currentEnv lastVal =
            match es with
            | [] -> lastVal, currentEnv
            | e :: rest ->
                let v, nextEnv = eval currentEnv e
                evalStmts rest nextEnv v
        evalStmts exprs env (VNum 0)

// ==========================================
// 5. СТАНДАРТНАЯ БИБЛИОТЕКА
// ==========================================
let rec formatVal v =
    match v with
    | VStr s -> s
    | VNum n -> string n
    | VBool b -> if b then "true" else "false"
    | VList lst -> "[" + String.Join(", ", List.map formatVal lst) + "]"
    | _ -> "<функция>"

let globalEnv : Environment = 
    Map.ofList [
        ("print", VNative (fun args -> 
            printfn "%s" (formatVal args.[0])
            VNum 0
        ));
        ("head", VNative (fun args -> 
            match args.[0] with | VList (h::_) -> h | _ -> failwith "Пустой список в head"
        ));
        ("tail", VNative (fun args -> 
            match args.[0] with | VList (_::t) -> VList t | _ -> failwith "Ошибка tail"
        ));
        ("cons", VNative (fun args -> 
            match args.[1] with | VList t -> VList (args.[0] :: t) | _ -> failwith "cons ждет список"
        ));
        ("isEmpty", VNative (fun args -> 
            match args.[0] with | VList [] -> VBool true | VList _ -> VBool false | _ -> failwith "Не список"
        ))
    ]

// ==========================================
// ЗАПУСК
// ==========================================
let args = fsi.CommandLineArgs
if args.Length < 2 then
    printfn "Использование: dotnet fsi GoFun.fsx <файл.gof>"
else
    let path = args.[1]
    if File.Exists path then
        try
            let code = File.ReadAllText path
            let ast = parseProgram (tokenize code)
            eval globalEnv ast |> ignore
        with ex -> printfn "ОШИБКА: %s" ex.Message
    else printfn "Файл не найден!"