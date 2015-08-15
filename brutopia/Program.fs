open System

exception UnknownSeason of Enum

type Season =
    | Spring = 0
    | Autumn = 1

type Model = {
    year : int;
    season : Season;
    population : int64;
    food : int64;
    running : Boolean;
}

let foodRequired model =
    model.population * 2L

let newPopulation model =
    let r = System.Random()
    let rate = match (model.food, foodRequired model) with
               | (available, required) when available * 2L > required -> (float32)(r.Next(1, 6))
               | (available, required) when available > required -> (float32)(r.Next(1, 4))
               | (0L, _) -> -(float32)(r.Next(500, 900))
               | (available, required) when required / 4L > available -> -(float32)(r.Next(20, 60))
               | (available, required) when required / 3L > available -> -(float32)(r.Next(10, 40))
               | (available, required) when required / 2L > available -> -(float32)(r.Next(10, 20))
               | _ -> -(float32)(r.Next(5, 10))
    model.population + (int64)((float32)model.population * (rate / 1000.0f))
    
let processSpringSeason model =
    {model with season = Season.Autumn 
                population = (newPopulation model)
                food = max (model.food - foodRequired model) 0L}

let processAutumnSeason model =
    { model with year = model.year + 1
                 season = Season.Spring
                 population = (newPopulation model) 
                 food = max (model.food - foodRequired model) 0L }

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
    printfn "Food: %d" model.food
    model

let rec mainLoop model =
    let newModel = advanceSeason model |> outputStatus
    Console.ReadKey() |> ignore
    mainLoop newModel

[<EntryPoint>]
let main argv = 
    let model = { year = 1;
                  season = Season.Spring;
                  population = 500000L;
                  food = 10000000L;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
