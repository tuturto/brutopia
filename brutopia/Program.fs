open System

type Season =
    | Spring of int
    | Autumn of int

type Harvest =
    | None
    | Some of int

type Advice =
    | NoAdvice
    | SomeAdvice of String

type Model = {
    season : Season;
    population : int;
    food : int;
    fields : int;
    harvest : Harvest;
    running : Boolean;
}

let foodRequired model =
    model.population * 2

let newPopulation model =
    let r = Random()
    let rate = match (model.food, foodRequired model) with
               | (available, required) when available * 2 > required -> (float32)(r.Next(1, 6))
               | (available, required) when available > required -> (float32)(r.Next(1, 4))
               | (0, _) -> -(float32)(r.Next(500, 900))
               | (available, required) when required / 4 > available -> -(float32)(r.Next(20, 60))
               | (available, required) when required / 3 > available -> -(float32)(r.Next(10, 40))
               | (available, required) when required / 2 > available -> -(float32)(r.Next(10, 20))
               | _ -> -(float32)(r.Next(5, 10))
    model.population + (int)((float32)model.population * (rate / 1000.0f)) + r.Next(1, 10)

let harvestedAmount model =
    40 * min model.fields model.population 

let advanceSeason model =
    let harvest = match model.season with
                      | Spring (_) -> None
                      | Autumn (_) -> Some <| harvestedAmount model
    let food = match harvest with
                   | None -> max (model.food - foodRequired model) 0
                   | Some (amount) -> max (model.food + (harvestedAmount model) - foodRequired model) 0
    let population = newPopulation model
    let season = match model.season with
                     | Spring (year) -> Autumn year
                     | Autumn (year) -> Spring <| year + 1
    { model with season = season
                 population = population
                 food = food
                 harvest = harvest }

let outputStatus model =
    printfn ""
    printfn "%s" <| match model.season with
                        | Autumn (year) -> sprintf "Autumn of year %d" year
                        | Spring (year) -> sprintf "Spring of year %d" year
    printfn "Population: %d" model.population
    printfn "Food: %d / %d" model.food <| foodRequired model
    printfn "harvested: %s" <| match model.harvest with
                                   | None -> sprintf "harvest is not yet ready"
                                   | Some (amount) -> sprintf "%d" amount
    model

let giveAdvice model =
    let r = Random()    
    let adviceList = [
        (if model.food < foodRequired model then SomeAdvice "We don't have enough food, maybe more fields is needed?" else NoAdvice);
        (if model.population < model.fields then SomeAdvice "We have more fields than farmers, so some of the fields aren't producing anything." else NoAdvice);
        (match model.harvest with
             | None -> NoAdvice
             | Some (amount) -> if amount < 2 * (foodRequired model) then SomeAdvice "Our fields don't produce enough harvest to feed everyone in the long run." else NoAdvice)
    ]
    let allAdvice = adviceList |> List.filter (fun x -> match x with
                                                       | NoAdvice -> false
                                                       | _ -> true) 
    if allAdvice.Length > 0 then
        let advice = (allAdvice |> List.item (r.Next allAdvice.Length))
        printfn ""
        printfn "Advice: %s" <| match advice with
                                    | NoAdvice -> ""
                                    | (SomeAdvice text) -> text
    model

let rec mainLoop model =
    let newModel = advanceSeason model |> outputStatus |> giveAdvice
    Console.ReadKey() |> ignore
    mainLoop newModel

[<EntryPoint>]
let main argv = 
    let model = { season = Spring 1;
                  population = 500;
                  food = 2000;
                  fields = 50;
                  harvest = None;
                  running = true }
    mainLoop model    
    0 // return an integer exit code
