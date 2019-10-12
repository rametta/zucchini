module Food.Service

open MongoDB.Driver
open Microsoft.Extensions.DependencyInjection

open Foods

let find (collection : IMongoCollection<Food>) (criteria : FoodCriteria): Food[] =
  match criteria with
  | All -> collection.Find(Builders.Filter.Empty).ToEnumerable() |> Seq.toArray

let save (collection : IMongoCollection<Food>) (food : Food) : Food =
  let foods = collection.Find(fun x -> x.Id = food.Id).ToEnumerable()

  match Seq.isEmpty foods with
  | true -> collection.InsertOne food
  | false ->
    let filter = Builders<Food>.Filter.Eq((fun x -> x.Id), food.Id)
    let update =
      Builders<Food>.Update
        .Set((fun x -> x.Title), food.Title)
        .Set((fun x -> x.Done), food.Done)

    collection.UpdateOne(filter, update) |> ignore

  food

let delete (collection : IMongoCollection<Food>) (id : string) : bool =
  collection.DeleteOne(Builders<Food>.Filter.Eq((fun x -> x.Id), id)).DeletedCount > 0L

type IServiceCollection with
  member this.AddFoodService(collection : IMongoCollection<Food>) =
    this.AddSingleton<FoodFind>(find collection) |> ignore
    this.AddSingleton<FoodSave>(save collection) |> ignore
    this.AddSingleton<FoodDelete>(delete collection) |> ignore