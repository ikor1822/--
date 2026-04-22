open System
open System.IO

type Expr =
    | Num of int | Str of string | Bool of bool | Var of string
    | List of Expr list
    | Assign of string * Expr
    | Func of string * string list * Expr
    | Call of string * Expr list
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | Block of Expr list

type Value =
    | VNum of int | VStr of string | VBool of bool
    | VList of Value list
    | VClosure of string * string list * Expr * ref<Map<string, Value>>
    | VNative of (Value list -> Value)

type Env = Map<string, Value>

let tok (s:string) =
    let rec f i a =
        if i>=s.Length then List.rev a else
        let c=s.[i]
        if Char.IsWhiteSpace c then f (i+1) a else
        if Char.IsDigit c then
            let mutable j=i
            while j<s.Length && Char.IsDigit s.[j] do j<-j+1
            f j (s.Substring(i,j-i)::a)
        elif Char.IsLetter c || c='_' then
            let mutable j=i
            while j<s.Length && (Char.IsLetter s.[j] || Char.IsDigit s.[j] || s.[j]='_') do j<-j+1
            f j (s.Substring(i,j-i)::a)
        elif c='"' then
            let mutable j=i+1
            while j<s.Length && s.[j]<>'"' do j<-j+1
            f (j+1) (s.Substring(i,j-i+1)::a)
        else
            if i+1<s.Length && s.[i]='=' && s.[i+1]='=' then
                f (i+2) ("=="::a)
            else
                f (i+1) ((string c)::a)
    f 0 []

let rec pe t =
    let a,b = pt t
    let rec g l ts =
        match ts with
        | op::r when op="+"||op="-"||op="*"||op="=="||op="<"||op=">" ->
            let x,y = pt r
            g (BinOp(op,l,x)) y
        | _ -> l,ts
    g a b

and pt t =
    match t with
    | "if"::r ->
        let c,t1 = pe r
        let t2,t3 = pb t1
        match t3 with
        | "else"::t4 ->
            let f1,t5 = pb t4
            If(c,t2,f1),t5
        | _ -> If(c,t2,Block []),t3

    | "func"::n::"("::r ->
        let rec h ts a =
            match ts with
            | ")"::x -> List.rev a,x
            | ","::x -> h x a
            | x::xs -> h xs (x::a)
            | _ -> failwith "func args"
        let args,t1 = h r []
        let b,t2 = pb t1
        Func(n,args,b),t2

    | n::"="::r ->
        let e,t2 = pe r
        Assign(n,e),t2

    | n::"("::r ->
        let rec h ts a =
            match ts with
            | ")"::x -> List.rev a,x
            | ","::x -> h x a
            | _ ->
                let e,x = pe ts
                h x (e::a)
        let args,t1 = h r []
        Call(n,args),t1

    | "["::r ->
        let rec h ts a =
            match ts with
            | "]"::x -> List(List.rev a),x
            | ","::x -> h x a
            | _ ->
                let e,x = pe ts
                h x (e::a)
        h r []

    | "("::r ->
        let e,t2 = pe r
        match t2 with
        | ")"::x -> e,x
        | _ -> failwith ")"

    | "{"::_ -> pb t

    | "true"::r -> Bool true,r
    | "false"::r -> Bool false,r
    | x::r when x.StartsWith("\"") -> Str(x.Trim('"')),r
    | x::r when Char.IsDigit x.[0] -> Num(int x),r
    | x::r -> Var x,r
    | _ -> failwith "parse"

and pb t =
    match t with
    | "{"::r ->
        let rec h ts a =
            match ts with
            | "}"::x -> Block(List.rev a),x
            | _ ->
                let e,x = pe ts
                h x (e::a)
        h r []
    | _ -> failwith "{"

let prog t =
    let rec f ts a =
        match ts with
        | [] -> Block(List.rev a)
        | _ ->
            let e,r = pe ts
            f r (e::a)
    f t []

let rec ev env e =
    match e with
    | Num n -> VNum n,env
    | Str s -> VStr s,env
    | Bool b -> VBool b,env

    | Var x ->
        match Map.tryFind x env with
        | Some v -> v,env
        | None -> failwith ("нет переменной: " + x)

    | List xs ->
        let vs = List.map (fun x -> fst (ev env x)) xs
        VList vs,env

    | Assign(n,e) ->
        let v,_ = ev env e
        v,Map.add n v env

    | Func(n,a,b) ->
        let r = ref env
        let f = VClosure(n,a,b,r)
        let e2 = Map.add n f env
        r.Value <- e2
        f,e2

    | Call(n,a) ->
        let vs = List.map (fun x -> fst (ev env x)) a
        match Map.tryFind n env with
        | Some (VNative f) -> f vs,env
        | Some (VClosure(_,names,b,r)) ->
            let e2 = List.fold2 (fun acc k v -> Map.add k v acc) r.Value names vs
            let v,_ = ev e2 b
            v,env
        | _ -> failwith ("нет функции: " + n)

    | If(c,t,f) ->
        let v,_ = ev env c
        match v with
        | VBool true -> ev env t
        | VBool false -> ev env f
        | _ -> failwith "if"

    | BinOp(op,a,b) ->
        let v1,_ = ev env a
        let v2,_ = ev env b
        match op,v1,v2 with
        | "+",VNum x,VNum y -> VNum(x+y),env
        | "-",VNum x,VNum y -> VNum(x-y),env
        | "*",VNum x,VNum y -> VNum(x*y),env
        | "==",VNum x,VNum y -> VBool(x=y),env
        | "<",VNum x,VNum y -> VBool(x<y),env
        | ">",VNum x,VNum y -> VBool(x>y),env
        | _ -> failwith "op"

    | Block xs ->
        let rec f xs env last =
            match xs with
            | [] -> last,env
            | x::r ->
                let v,e2 = ev env x
                f r e2 v
        f xs env (VNum 0)

let rec s v =
    match v with
    | VNum n -> string n
    | VStr x -> x
    | VBool b -> if b then "true" else "false"
    | VList l -> "[" + String.Join(", ", List.map s l) + "]"
    | _ -> "<f>"

let env0 =
    Map.ofList [
        ("print", VNative(fun a -> printfn "%s" (s a.[0]); VNum 0))
        ("head", VNative(fun a -> match a.[0] with | VList(h::_) -> h | _ -> failwith "h"))
        ("tail", VNative(fun a -> match a.[0] with | VList(_::t) -> VList t | _ -> failwith "t"))
        ("cons", VNative(fun a -> match a.[1] with | VList t -> VList(a.[0]::t) | _ -> failwith "c"))
        ("isEmpty", VNative(fun a -> match a.[0] with | VList [] -> VBool true | _ -> VBool false))
        ("readFile", VNative(fun a ->
            match a.[0] with
            | VStr path -> VStr(File.ReadAllText path)
            | _ -> failwith "rf"))
    ]

let a = fsi.CommandLineArgs

if a.Length < 2 then
    printfn "file?"
else
    let code = File.ReadAllText a.[1]
    let t = tok code
    let ast = prog t
    ev env0 ast |> ignore