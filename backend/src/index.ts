import express, { Application } from 'express'
import bodyParser from 'body-parser'
import mongoose from 'mongoose'
import dotenv from 'dotenv'
import { Food } from './food'

dotenv.config()

const app: Application = express()

app.use(bodyParser.urlencoded({ extended: true }))
app.use(bodyParser.json())

app.get('/api/food', (req, res) => {
  Food.find({}, (err, data) => {
    if (err) {
      res.status(500).json(err)
      return
    }

    res.json(data)
  })
})

app.post('/api/food', (req, res) => {
  const food = new Food({
    title: req.body.title,
    done: false,
    date: Math.round((new Date()).getTime())
  })

  food.save({ validateBeforeSave: true }, (err, data) => {
    if (err) {
      res.status(500).json(err)
      return
    }

    res.json(data)
  })
})

app.put('/api/food', (req, res) => {
  Food.updateOne({ _id: req.body._id }, req.body, err => {
    if (err) {
      res.status(500).json(err)
      return
    }

    Food.findById(req.body._id, (err2, food) => {
      if (err2 || !food) {
        res.status(500).json(err2)
        return
      }

      res.json(food)
    })
  })
})

app.delete('/api/food/:id', (req, res) => {
  Food.deleteOne({ _id: req.params.id }, err => {
    if (err) {
      res.status(500).json(err)
      return
    }

    res.json(true)
  })
})

mongoose.set('useCreateIndex', true)
mongoose.Promise = global.Promise
mongoose
  .connect(
    process.env.MONGO_URL || '',
    { useNewUrlParser: true }
  )
  .then(() =>
    app.listen(process.env.PORT || 8080, () =>
      console.log('API listening')
    )
  )
  .catch(() => console.log('Could not connect to DB'))