open System

type Expr =
    | Num of int | Str of string | Bool of bool | Var of string
    | List of Expr list
    | Assign of string * Expr
    | Func of string * string list * Expr
    | Call of string * Expr list
    | If of Expr * Expr * Expr
    | BinOp of string * Expr * Expr
    | Block of Expr list

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
    | "let"::n::"="::r ->
        let e,t2 = pe r
        Assign(n,e),t2
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
let code = "
func fact(n) {
    if n == 0 {
        1
    } else {
        n * fact(n - 1)
    }
}

let x = 10
fact(x)
"

printfn "%s" code

let t = tok code
printfn "%A" t

let ast = prog t
printfn "%A" ast