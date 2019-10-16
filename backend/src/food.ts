import mongoose, { Schema, Document } from 'mongoose'

export interface IFood extends Document {
  id: string
  title: string
  done: boolean
  date: number
}

const FoodSchema: Schema = new Schema({
  title: { type: String, required: true },
  done: Boolean,
  date: Number
})

export const Food = mongoose.model<IFood>('Food', FoodSchema);