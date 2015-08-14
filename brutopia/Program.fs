open System

exception UnknownSeason of Enum

type Season =
    | Spring = 0
    | Autumn = 1

type Model = {
    year : int;
    season : Season;
    running : Boolean;
}

let processSpringSeason model =
    {model with season = Season.Autumn }

let processAutumnSeason model =
    { model with year = model.year + 1
                 season = Season.Spring }

let advanceSeason model =
    match model.season with
        | Season.Autumn -> processAutumnSeason model
        | Season.Spring -> processSpringSeason model
        | _ -> raise (UnknownSeason model.season)

let seasonText season =
    match season with
        | Season.Autumn -> "Autumn"
        | Season.Spring -> "Spring"
        | _ -> raise (UnknownSeason season)

let outputStatus model =
    printfn "Status for the year: %d" model.year
    printfn "Current season: %s" (seasonText model.season)
    model

let rec mainLoop model =
    let newModel = advanceSeason model |> outputStatus
    Console.ReadKey() |> ignore
    mainLoop newModel

[<EntryPoint>]
let main argv = 
    let model = { year = 1;
                  season = Season.Spring;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
