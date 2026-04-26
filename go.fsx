open System
open System.IO

// ==========================================
// 1. AST (Абстрактное Синтаксическое Дерево)
// ==========================================

type Expr =
    | Num of int
    | Str of string
    | Bool of bool
    | Var of string
    | List of Expr list
    | Assign of string * Expr          // let x = ...
    | Func of string * string list * Expr // Именованные функции (Рекурсия)
    | Lambda of string list * Expr     // Анонимные функции (fun)
    | Call of Expr * Expr list         // Вызов функции
    | If of Expr * Expr * Expr         // Условия
    | BinOp of string * Expr * Expr    // Бинарные операции
    | Block of Expr list               // Блок кода {}

// ==========================================
// 2. VALUES (Значения) и Окружение
// ==========================================

type Value =
    | VNum of int
    | VStr of string
    | VBool of bool
    | VList of Value list
    | VClosure of string * string list * Expr * Env ref // Замыкания
    | VNative of (Value list -> Value)                  // Встроенные функции
    | VThunk of Expr * Env ref * Value option ref       // Ленивые вычисления (Thunks)

and Env = Map<string, Value>

// ==========================================
// 3. TOKENIZER (Лексический анализатор)
// ==========================================

let tok (s:string) =
    let rec f i acc =
        if i >= s.Length then List.rev acc else
        let c = s.[i]

        if Char.IsWhiteSpace c then f (i+1) acc
        elif Char.IsDigit c then
            let mutable j = i
            while j < s.Length && Char.IsDigit s.[j] do j <- j + 1
            f j (s.Substring(i, j-i) :: acc)
        elif Char.IsLetter c || c = '_' then
            let mutable j = i
            while j < s.Length && (Char.IsLetter s.[j] || Char.IsDigit s.[j] || s.[j] = '_') do
                j <- j + 1
            f j (s.Substring(i, j-i) :: acc)
        elif c = '"' then // Чтение строк
            let mutable j = i+1
            while j < s.Length && s.[j] <> '"' do j <- j + 1
            f (j+1) (s.Substring(i, j-i+1) :: acc)
        elif c = '=' && i + 1 < s.Length && s.[i+1] = '=' then // Оператор ==
            f (i+2) ("==" :: acc)
        else
            f (i+1) ((string c) :: acc)

    f 0 []

// ==========================================
// 4. PARSER (Синтаксический анализатор)
// ==========================================

let rec pe t =
    let a,b = pt t
    let rec g l ts =
        match ts with
        | op::r when op="+" || op="-" || op="*" || op="==" ->
            let x,y = pt r
            g (BinOp(op,l,x)) y
        | _ -> l, ts
    g a b

and pt t =
    match t with
    | "fun"::"("::r ->
        let rec args ts acc =
            match ts with
            | ")"::rest -> List.rev acc, rest
            | ","::rest -> args rest acc
            | x::xs -> args xs (x::acc)
            | _ -> failwith "args error"
        let a,t1 = args r []
        let body,t2 = pb t1
        Lambda(a, body), t2

    | "if"::r ->
        let c,t1 = pe r
        let t2,t3 = pb t1
        match t3 with
        | "else"::t4 ->
            let f1,t5 = pb t4
            If(c,t2,f1), t5
        | _ -> If(c,t2,Block []), t3

    | "func"::n::"("::r ->
        let rec args ts acc =
            match ts with
            | ")"::rest -> List.rev acc, rest
            | ","::rest -> args rest acc
            | x::xs -> args xs (x::acc)
            | _ -> failwith "func args error"
        let a,t1 = args r []
        let b,t2 = pb t1
        Func(n,a,b), t2

    | "let"::n::"="::r ->
        let e,t2 = pe r
        Assign(n,e), t2

    | "("::r ->
        let e,t2 = pe r
        match t2 with
        | ")"::x -> e,x
        | _ -> failwith "missing )"

    | "["::r ->
        let rec elems ts acc =
            match ts with
            | "]"::rest -> List(List.rev acc), rest
            | ","::rest -> elems rest acc
            | _ ->
                let e,x = pe ts
                elems x (e::acc)
        elems r []

    | x::"("::r ->
        let rec args ts acc =
            match ts with
            | ")"::rest -> List.rev acc, rest
            | ","::rest -> args rest acc
            | _ ->
                let e,x = pe ts
                args x (e::acc)
        let a,t1 = args r []
        Call(Var x, a), t1

    | "true"::r -> Bool true, r
    | "false"::r -> Bool false, r
    | x::r when Char.IsDigit x.[0] -> Num(int x), r
    | x::r when x.StartsWith("\"") -> Str(x.Trim('"')), r // <-- ИСПРАВЛЕНИЕ ДЛЯ СТРОК
    | x::r -> Var x, r
    | _ -> failwith "parse error"

and pb t =
    match t with
    | "{"::r ->
        let rec block ts acc =
            match ts with
            | "}"::rest -> Block(List.rev acc), rest
            | _ ->
                let e,x = pe ts
                block x (e::acc)
        block r []
    | _ -> failwith "missing {"

let prog t =
    let rec f ts acc =
        match ts with
        | [] -> Block(List.rev acc)
        | _ ->
            let e,r = pe ts
            f r (e::acc)
    f t []

// ==========================================
// 5. EVALUATOR (Интерпретатор + Ленивость)
// ==========================================

let rec force (v: Value) : Value =
    match v with
    | VThunk(e, env, cache) ->
        match cache.Value with
        | Some v -> v
        | None ->
            let v,_ = ev env.Value e
            cache.Value <- Some v // Мемоизация ленивого вычисления
            v
    | _ -> v

and bindArgs (env: Env) (names: string list) (args: Value list) : Env =
    let rec loop env names args =
        match names, args with
        | n::ns, a::as' ->
            loop (Map.add n a env) ns as'
        | _ -> env
    loop env names args

and applyClosure (f: Value) (args: Value list) : Value =
    match f with
    | VClosure(name, names, body, r) ->
        let argCount = List.length args
        let paramCount = List.length names

        // Частичное применение (Каррирование)
        if argCount < paramCount then
            let bound = names |> List.take argCount
            let remaining = names |> List.skip argCount
            let env2 = bindArgs r.Value bound args
            VClosure(name, remaining, body, ref env2)

        // Точное совпадение аргументов
        elif argCount = paramCount then
            let env2 = bindArgs r.Value names args
            fst (ev env2 body)

        // Передано больше аргументов, чем нужно
        else
            let firstArgs = args |> List.take paramCount
            let restArgs = args |> List.skip paramCount
            let env2 = bindArgs r.Value names firstArgs
            let result = fst (ev env2 body)
            applyClosure result restArgs

    | _ -> failwith "not a function"

and ev (env: Env) (e: Expr) : Value * Env =
    match e with
    | Num n -> VNum n, env
    | Str s -> VStr s, env
    | Bool b -> VBool b, env

    | Var x ->
        match Map.tryFind x env with
        | Some v -> force v, env
        | None -> failwith ("undefined variable: " + x)

    | List xs ->
        let vs = xs |> List.map (fun x -> force (fst (ev env x)))
        VList vs, env

    | Assign(n,e) -> // Ленивое связывание (let x = ...)
        let v = VThunk(e, ref env, ref None)
        v, Map.add n v env

    | Func(n,args,body) -> // Именованная рекурсивная функция
        let r = ref env
        let f = VClosure(n,args,body,r)
        let env2 = Map.add n f env
        r.Value <- env2 // Замыкание окружения на само себя
        f, env2

    | Lambda(args,body) -> // Анонимная функция
        VClosure("",args,body,ref env), env

    | Call(fexpr,args) ->
        let fval = force (fst (ev env fexpr))
        let argVals = args |> List.map (fun x -> force (fst (ev env x)))

        match fval with
        | VNative f -> f argVals, env
        | _ -> applyClosure fval argVals, env

    | If(c,t,f) ->
        let v = force (fst (ev env c))
        match v with
        | VBool true -> ev env t
        | VBool false -> ev env f
        | _ -> failwith "if condition must be boolean"

    | BinOp(op,a,b) ->
        let v1 = force (fst (ev env a))
        let v2 = force (fst (ev env b))
        match op,v1,v2 with
        | "+",VNum x,VNum y -> VNum(x+y), env
        | "-",VNum x,VNum y -> VNum(x-y), env
        | "*",VNum x,VNum y -> VNum(x*y), env
        | "==",VNum x,VNum y -> VBool(x=y), env
        | _ -> failwith "invalid operator or types"

    | Block xs ->
        let rec loop xs env last =
            match xs with
            | [] -> last, env
            | x::r ->
                let v,e2 = ev env x
                loop r e2 v
        loop xs env (VNum 0)

// ==========================================
// 6. СТАНДАРТНАЯ БИБЛИОТЕКА (Std Lib)
// ==========================================

let env0 =
    Map.ofList [
        ("print", VNative(fun a ->
            match a.[0] with
            | VNum n -> printf "%d\n" n
            | VStr s -> printf "%s\n" s
            | VBool b -> printf "%b\n" b
            | VList lst -> 
                let strList = lst |> List.map (function | VNum n -> string n | VStr s -> sprintf "\"%s\"" s | _ -> "?")
                printf "[%s]\n" (String.Join(", ", strList))
            | _ -> printfn "%A" a.[0]
            VNum 0))

        ("map", VNative(fun a ->
            match a with
            | [f; VList lst] ->
                VList (lst |> List.map (fun x -> applyClosure f [x]))
            | _ -> failwith "map expects a function and a list"))

        ("fold", VNative(fun a ->
            match a with
            | [f; acc; VList lst] ->
                List.fold (fun st x -> applyClosure f [st; x]) acc lst
            | _ -> failwith "fold expects a function, an accumulator and a list"))

        ("readFile", VNative(fun a ->
            match a.[0] with
            | VStr path -> VStr(File.ReadAllText path)
            | _ -> failwith "readFile expects a string path"))
    ]

// ==========================================
// 7. ЗАПУСК И ТЕСТЫ ПО ТРЕБОВАНИЯМ ЗАДАНИЯ
// ==========================================

let testCode = "
print(\"=== 1. Factorial & Recursion ===\")
func fact(n) {
    if n == 0 { 
        1 
    } else { 
        n * fact(n - 1) 
    }
}
let resFact = fact(5)
print(resFact)

print(\"=== 2. Partial Application (Closures) ===\")
let add = fun(x,y) { x + y }
let add5 = add(5)
print(add5(10))

print(\"=== 3. Lists & Map ===\")
let numbers = [1, 2, 3, 4, 5]
let square = fun(x) { x * x }
let squares = map(square, numbers)
print(squares)

print(\"=== 4. Fold ===\")
let sum = fun(acc, x) { acc + x }
let total = fold(sum, 0, numbers)
print(total)

print(\"=== 5. Inline Lambda Functions ===\")
let tens = map(fun(x) { x * 10 }, numbers)
print(tens)
"

printfn "Running interpreter...\n"
let t = tok testCode
let ast = prog t
let _, _ = ev env0 ast
printfn "\nExecution finished!"