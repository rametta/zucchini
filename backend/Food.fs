module Foods

[<CLIMutable>]
type Food = {
    Id : string
    Title : string
    Done : bool
    Date : int64
}

type FoodSave = Food -> Food

type FoodCriteria =
  | All

type FoodFind = FoodCriteria -> Food[]

type FoodDelete = string -> bool