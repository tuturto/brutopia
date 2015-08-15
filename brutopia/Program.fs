open System

exception UnknownSeason of Enum

type Season =
    | Spring = 0
    | Autumn = 1

type Model = {
    year : int;
    season : Season;
    population : int64;
    running : Boolean;
}

let newPopulation model =
    let r = System.Random()
    model.population + (int64)((float32)model.population * ((float32)(r.Next(1, 5)) / 1000.0f))

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
                  population = (int64)500000;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
