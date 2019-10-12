module Food.Handlers

open System
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection
open Giraffe
open Microsoft.AspNetCore.Http
open FSharp.Control.Tasks.V2.ContextInsensitive
open Foods

let getFoodHandler (next: HttpFunc) (ctx: HttpContext) =
    let find = ctx.GetService<FoodFind>() 
    let foods = find All
    json foods next ctx

let postFoodHandler (next: HttpFunc) (ctx: HttpContext) =
    task {
      let save = ctx.GetService<FoodSave>()
      let! food = ctx.BindJsonAsync<Food>()
      let food = { food with
                    Id = ShortGuid.fromGuid(Guid.NewGuid());
                    Date = DateTimeOffset(DateTime.Now).ToUnixTimeMilliseconds() }
      return! json (save food) next ctx
    }

let putFoodHandler (next: HttpFunc) (ctx: HttpContext) =
    task {
      let save = ctx.GetService<FoodSave>()
      let! food = ctx.BindJsonAsync<Food>()
      return! json (save food) next ctx
    }

let deleteFoodhandler (id: string) (next: HttpFunc) (ctx: HttpContext) =
    let delete = ctx.GetService<FoodDelete>()
    json (delete id) next ctx