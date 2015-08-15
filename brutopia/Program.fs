open System

type Season =
    | Spring of int
    | Autumn of int

type Harvest =
    | None
    | Some of int64

type Model = {
    season : Season;
    population : int64;
    food : int64;
    fields : int64;
    harvest : Harvest;
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

let harvestedAmount model =
    model.fields * 4L

let advanceSeason model =
    let harvest = match model.season with
                      | Spring (_) -> None
                      | Autumn (_) -> Some <| harvestedAmount model
    let food = match harvest with
                   | None -> max (model.food - foodRequired model) 0L
                   | Some (amount) -> max (model.food + (harvestedAmount model) - foodRequired model) 0L
    let population = newPopulation model
    let season = match model.season with
                     | Spring (year) -> Autumn year
                     | Autumn (year) -> Spring <| year + 1
    { model with season = season
                 population = population
                 food = food
                 harvest = harvest }

let outputStatus model =
    printfn "%s" <| match model.season with
                        | Autumn (year) -> sprintf "Autumn of year %d" year
                        | Spring (year) -> sprintf "Spring of year %d" year
    printfn "Population: %d" model.population
    printfn "Food: %d / %d" model.food <| foodRequired model
    printfn "harvested: %s" <| match model.harvest with
                                   | None -> sprintf "harvest is not yet ready"
                                   | Some (amount) -> sprintf "%d" amount
    model

let rec mainLoop model =
    let newModel = advanceSeason model |> outputStatus
    Console.ReadKey() |> ignore
    mainLoop newModel

[<EntryPoint>]
let main argv = 
    let model = { season = Spring 1;
                  population = 500000L;
                  food = 10000000L;
                  fields = 25000L;
                  harvest = None;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
