module Backend.App

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open MongoDB.Driver

open Foods
open Food.Service
open Food.Handlers

let webApp =
    choose [
        GET >=> route "/api/food" >=> getFoodHandler
        POST >=> route "/api/food" >=> postFoodHandler
        PUT >=> route "/api/food" >=> putFoodHandler
        DELETE >=> routef "/api/food/%s" deleteFoodhandler
        RequestErrors.notFound (text "Not Found")
    ]

let configureApp (app : IApplicationBuilder) =
    app.UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    let mongo = MongoClient (Environment.GetEnvironmentVariable "MONGO_URL")
    let db = mongo.GetDatabase "zucchini"

    services.AddGiraffe() |> ignore
    services.AddFoodService(db.GetCollection<Food>("food")) |> ignore

[<EntryPoint>]
let main _ =
    WebHostBuilder()
        .UseKestrel()
        .Configure(Action<IApplicationBuilder> configureApp)
        .ConfigureServices(configureServices)
        .Build()
        .Run()
    0