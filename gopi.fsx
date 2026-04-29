open System
open System.IO

type Expr =
    | Num of int
    | Str of string
    | Bool of bool
    | Var of string
    | List of Expr list
    | Assign of string * Expr
    | Func of string * string list * Expr
    | Lambda of string list * Expr
    | Call of Expr * Expr list
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | Block of Expr list


type Value =
    | VNum of int
    | VStr of string
    | VBool of bool
    | VList of Value list
    | VClosure of string * string list * Expr * Env ref
    | VNative of (Value list -> Value)
    | VThunk of Expr * Env ref * Value option ref

and Env = Map<string, Value>

let tok (s:string) =
    let rec f i acc =
        if i >= s.Length then List.rev acc else
        let c = s.[i]

        if Char.IsWhiteSpace c then f (i+1) acc
        elif c = '/' && i + 1 < s.Length && s.[i+1] = '/' then
            let mutable j = i + 2
            while j < s.Length && s.[j] <> '\n' do j <- j + 1
            f j acc
        elif Char.IsDigit c then
            let mutable j = i
            while j < s.Length && Char.IsDigit s.[j] do j <- j + 1
            f j (s.Substring(i, j-i) :: acc)
        elif Char.IsLetter c || c = '_' then
            let mutable j = i
            while j < s.Length && (Char.IsLetter s.[j] || Char.IsDigit s.[j] || s.[j] = '_') do
                j <- j + 1
            f j (s.Substring(i, j-i) :: acc)
        elif c = '"' then
            let mutable j = i+1
            while j < s.Length && s.[j] <> '"' do j <- j + 1
            f (j+1) (s.Substring(i, j-i+1) :: acc)
        elif c = '=' && i + 1 < s.Length && s.[i+1] = '=' then
            f (i+2) ("==" :: acc)
        else
            f (i+1) ((string c) :: acc)
    f 0 []

let rec pe t =
    let a,b = p_add t
    let rec g l ts =
        match ts with
        | op::r when op="==" || op="<" || op=">" ->
            let x,y = p_add r
            g (BinOp(op,l,x)) y
        | _ -> l, ts
    g a b

and p_add t =
    let a,b = p_mul t
    let rec g l ts =
        match ts with
        | op::r when op="+" || op="-" ->
            let x,y = p_mul r
            g (BinOp(op,l,x)) y
        | _ -> l, ts
    g a b

and p_mul t =
    let a,b = pt t
    let rec g l ts =
        match ts with
        | op::r when op="*" || op="/" ->
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

    | n::"="::r when Char.IsLetter(n.[0]) || n.[0] = '_' ->
        let e,t2 = pe r
        Assign(n,e), t2

    | "("::r ->
        let e,t2 = pe r
        match t2 with | ")"::x -> e,x | _ -> failwith "missing )"
    | "["::r ->
        let rec elems ts acc =
            match ts with
            | "]"::rest -> List(List.rev acc), rest
            | ","::rest -> elems rest acc
            | _ -> let e,x = pe ts in elems x (e::acc)
        elems r []
    | x::"("::r ->
        let rec args ts acc =
            match ts with
            | ")"::rest -> List.rev acc, rest
            | ","::rest -> args rest acc
            | _ -> let e,x = pe ts in args x (e::acc)
        let a,t1 = args r []
        Call(Var x, a), t1
    | "true"::r -> Bool true, r
    | "false"::r -> Bool false, r
    | x::r when Char.IsDigit x.[0] -> Num(int x), r
    | x::r when x.StartsWith("\"") -> Str(x.Trim('"')), r
    | x::r -> Var x, r
    | _ -> failwith "parse error"

and pb t =
    match t with
    | "{"::r ->
        let rec block ts acc =
            match ts with
            | "}"::rest -> Block(List.rev acc), rest
            | _ -> let e,x = pe ts in block x (e::acc)
        block r []
    | _ -> failwith "missing {"

let prog t =
    let rec f ts acc =
        match ts with | [] -> Block(List.rev acc) | _ -> let e,r = pe ts in f r (e::acc)
    f t []

let rec force (v: Value) : Value =
    match v with
    | VThunk(e, env, cache) ->
        match cache.Value with
        | Some v -> v
        | None -> let v,_ = ev env.Value e in cache.Value <- Some v; v
    | _ -> v

and bindArgs (env: Env) (names: string list) (args: Value list) : Env =
    let rec loop env names args =
        match names, args with
        | n::ns, a::as' -> loop (Map.add n a env) ns as'
        | _ -> env
    loop env names args

and applyClosure (f: Value) (args: Value list) : Value =
    match f with
    | VClosure(name, names, body, r) ->
        let argCount, paramCount = List.length args, List.length names
        if argCount < paramCount then
            let bound, remaining = List.take argCount names, List.skip argCount names
            VClosure(name, remaining, body, ref (bindArgs r.Value bound args))
        elif argCount = paramCount then
            fst (ev (bindArgs r.Value names args) body)
        else
            let firstArgs, restArgs = List.take paramCount args, List.skip paramCount args
            applyClosure (fst (ev (bindArgs r.Value names firstArgs) body)) restArgs
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
    | List xs -> VList (xs |> List.map (fun x -> force (fst (ev env x)))), env
    | Assign(n,e) ->
        let v = VThunk(e, ref env, ref None)
        v, Map.add n v env
    | Func(n,args,body) ->
        let r = ref env
        let f = VClosure(n,args,body,r)
        let env2 = Map.add n f env
        r.Value <- env2
        f, env2
    | Lambda(args,body) -> VClosure("",args,body,ref env), env
    | Call(fexpr,args) ->
        let fval = force (fst (ev env fexpr))
        let argVals = args |> List.map (fun x -> force (fst (ev env x)))
        match fval with | VNative f -> f argVals, env | _ -> applyClosure fval argVals, env
    | If(c,t,f) ->
        match force (fst (ev env c)) with
        | VBool true -> ev env t | VBool false -> ev env f | _ -> failwith "if needs bool"
    | BinOp(op,a,b) ->
        let v1, v2 = force (fst (ev env a)), force (fst (ev env b))
        match op,v1,v2 with
        | "+",VNum x,VNum y -> VNum(x+y), env
        | "+",VList x,VList y -> VList(x @ y), env
        | "-",VNum x,VNum y -> VNum(x-y), env
        | "*",VNum x,VNum y -> VNum(x*y), env
        | "/",VNum x,VNum y -> VNum(x/y), env
        | "==",VNum x,VNum y -> VBool(x=y), env
        | "<",VNum x,VNum y -> VBool(x<y), env
        | ">",VNum x,VNum y -> VBool(x>y), env
        | _ -> failwith "invalid operator or types"
    | Block xs ->
        let rec loop xs env last =
            match xs with | [] -> last, env | x::r -> let v,e2 = ev env x in loop r e2 v
        loop xs env (VNum 0)

let env0 =
    Map.ofList [
        ("print", VNative(fun a ->
            match a.[0] with
            | VNum n -> printf "%d\n" n | VStr s -> printf "%s\n" s | VBool b -> printf "%b\n" b
            | VList lst -> 
                let strList = lst |> List.map (function | VNum n -> string n | VStr s -> sprintf "\"%s\"" s | _ -> "?")
                printf "[%s]\n" (String.Join(", ", strList))
            | _ -> printfn "%A" a.[0]
            VNum 0))
        ("input", VNative(fun a -> 
            VStr(Console.ReadLine())))
        ("map", VNative(fun a -> match a with | [f; VList lst] -> VList (lst |> List.map (fun x -> applyClosure f [x])) | _ -> failwith "map error"))
        ("fold", VNative(fun a -> match a with | [f; acc; VList lst] -> List.fold (fun st x -> applyClosure f [st; x]) acc lst | _ -> failwith "fold error"))
        ("len", VNative(fun a -> match a.[0] with | VList lst -> VNum(lst.Length) | _ -> failwith "len error"))
        ("head", VNative(fun a -> match a.[0] with | VList (x::_) -> x | _ -> failwith "head error"))
        ("tail", VNative(fun a -> match a.[0] with | VList (_::xs) -> VList xs | _ -> failwith "tail error"))
        ("filter", VNative(fun a -> 
            match a with
            | [f; VList lst] -> VList (lst |> List.filter (fun x -> match applyClosure f [x] with VBool b -> b | _ -> false))
            | _ -> failwith "filter error"))
        ("readFile", VNative(fun a ->
            match a.[0] with
            | VStr path -> VStr(File.ReadAllText path)
            | _ -> failwith "readFile error"))
        ("writeFile", VNative(fun a ->
            match a.[0], a.[1] with
            | VStr path, VStr content -> File.WriteAllText(path, content); VNum 0
            | _ -> failwith "writeFile error"))
    ]

let rec repl env =
    printf "gopi> "
    let line = Console.ReadLine()
    if line = null then ()
    elif String.IsNullOrWhiteSpace(line) then repl env
    elif line = "exit" || line = "quit" then ()
    else
        try
            let t = tok line
            let ast = prog t
            let v, newEnv = ev env ast
            match force v with
            | VNum n -> printfn "%d" n
            | VStr s -> printfn "\"%s\"" s
            | VBool b -> printfn "%b" b
            | VList lst -> 
                let strList = lst |> List.map (function | VNum n -> string n | VStr s -> sprintf "\"%s\"" s | _ -> "?")
                printfn "[%s]" (String.Join(", ", strList))
            | _ -> ()
            repl newEnv
        with
        | ex -> 
            printfn "Ошибка: %s" ex.Message
            repl env

let args = fsi.CommandLineArgs

if args.Length > 1 then
    let filepath = args.[1]
    if File.Exists(filepath) then
        let code = File.ReadAllText(filepath)
        let t = tok code
        let ast = prog t
        let _, _ = ev env0 ast
        printfn "\nВыполнено"
    else
        printfn "Файл: '%s' не найден." filepath
else
    printfn "Gopi REPL"
    printfn "Напишите 'exit' для выхода или нажмите 'Ctrl +c'"
    repl env0