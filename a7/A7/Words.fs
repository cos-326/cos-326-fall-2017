module Words

open Sequence

// return the array of words in a given file
let get_words (filename:string) : string list =
    let separators = [|' ';'_';';';':';'(';')';'\\';'/';'>';'<';'{';'}';'.';'!';'\n';'\r';',';'?';'$';'*'|]
    let item = System.IO.File.ReadAllText filename
    item.Split(separators, System.StringSplitOptions.RemoveEmptyEntries)
    |> Array.map (fun x -> x.ToLower())
    |> Array.toList

// convert every file in the given directory
let get_files (directory:string) : (string * string list) S.t =
    if not (System.IO.Directory.Exists directory) then
        printfn "directory doesn't exist %s" directory; exit 0
    
    let files = System.IO.Directory.GetFiles directory

    // read files in parallel
    let data =
        seq { for i in 0 .. files.Length-1 ->
                  async {
                         return (files.[i], get_words files.[i])
                        }
            }
    data
    |> Async.Parallel
    |> Async.RunSynchronously
    |> S.from_array

let print_words (ws:string array) =
    for i in 0..ws.Length-1 do
        printf "%s," ws.[i]
    printfn ""

let main (argv : string []) =

    // check args are ok
    if not (argv.Length = 2) then
        printfn "usage: program <filename> <word>"; exit 0

    let directory = argv.[0]
    let key = argv.[1].ToLower()

    match Counter.counter key (get_files directory) with
        | None -> printfn "%s was not found in any files in %s" key directory
        | Some (file, occurrences) -> printfn "file %s had %d" file occurrences
    
    0
