module Bench

let timer f x =
    let stopWatch = System.Diagnostics.Stopwatch.StartNew()
    f x |> ignore
    stopWatch.Stop()
    stopWatch.Elapsed.TotalMilliseconds

let benchmark name s p =
    let ss = [|timer s (); timer s (); timer s (); timer s ();|]
    let ps = [|timer p (); timer p (); timer p (); timer p ();|]

    // throw out the largest; compute the average of the other 3
    let average (a: float array) =
        let (sum, max) = Array.fold (fun (sum,max) t -> (sum + t, if t > max then t else max)) (0.0,0.0) a
        (sum - max) / (float ((Array.length a) - 1))

    printf "%-10s | " name
    Array.iter (fun f -> printf "%6.1f " f) ss
    printf " | "
    Array.iter (fun f -> printf "%6.1f " f) ps
    printf " | "
    printfn "%6.1f  |  %6.1f " (average ss) (average ps)

// a slow identity function
let waittime = 10000
let id x =
    let rec wait n = if n <= 0 then () else wait (n-1)
    wait waittime; x

let sz = 10000

let ss = Sequential.S.tabulate (fun i -> i % 2) sz
let ps = Parallel.S.tabulate   (fun i -> i % 2) sz

let tabulate_benchmark () =
    benchmark "tabulate" 
        (fun () -> Sequential.S.tabulate (fun i -> id i) sz)
        (fun () -> Parallel.S.tabulate   (fun i -> id i) sz)

let map_benchmark () =
    benchmark "map"
        (fun () -> Sequential.S.map (fun i -> id i) ss)
        (fun () -> Parallel.S.map   (fun i -> id i) ps)

let reduce_benchmark () =
    benchmark "reduce"
        (fun () -> Sequential.S.reduce (fun i j -> id (i + j)) 0 ss)
        (fun () -> Parallel.S.reduce   (fun i j -> id (i + j)) 0 ps)    

let main (argv : string []) =
    printfn "---------------------------------------------------------------------------------------------"
    printfn "BenchMark  |     Sequential Trials        |     Parallel Trials          | Seq Avg |  Par Avg"
    tabulate_benchmark ()
    map_benchmark ()
    reduce_benchmark ()
    // filter_benchmark ()
    printfn "---------------------------------------------------------------------------------------------"
    0
