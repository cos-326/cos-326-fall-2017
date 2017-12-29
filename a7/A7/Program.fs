// Learn more about F# at http://fsharp.org
// See the 'F# Tutorial' project for more help.

[<EntryPoint>]
let main argv =
    if argv.Length = 0 then 
        Bench.main argv
    else
        Words.main argv