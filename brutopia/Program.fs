open System

exception UnknownSeason of Enum

type Season =
    | Spring = 0
    | Autumn = 1

type Model = {
    year : int;
    season : Season;
    population : uint64;
    running : Boolean;
}

let newPopulation model =
    (uint64((float)model.population * 1.004))

let processSpringSeason model =
    {model with season = Season.Autumn 
                population = (newPopulation model) }

let processAutumnSeason model =
    { model with year = model.year + 1
                 season = Season.Spring
                 population = (newPopulation model) }

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
    printfn "Population: %d" model.population
    model

let rec mainLoop model =
    let newModel = advanceSeason model |> outputStatus
    Console.ReadKey() |> ignore
    mainLoop newModel

[<EntryPoint>]
let main argv = 
    let model = { year = 1;
                  season = Season.Spring;
                  population = (uint64)2503042;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
