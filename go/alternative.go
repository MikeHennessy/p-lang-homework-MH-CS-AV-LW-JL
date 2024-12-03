package main

import (
	"fmt"
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

// A little utility that simulates performing a task for a random duration.
// For example, calling do(10, "Remy", "is cooking") will compute a random
// number of milliseconds between 5000 and 10000, log "Remy is cooking",
// and sleep the current goroutine for that much time.

func do(seconds int, action ...any) {
	log.Println(action...)
	randomMillis := 500*seconds + rand.Intn(500*seconds)
	time.Sleep(time.Duration(randomMillis) * time.Millisecond)
}

type Order struct {
	id         uint64
	customer   string
	reply      chan *Order
	preparedBy string
}

type Cook struct {
	name string
	busy bool
	mu   sync.Mutex
}

var nextId atomic.Uint64
var waiter = make(chan *Order, 3)

func NewOrder(customer string) *Order {
	return &Order{
		id:       nextId.Add(1),
		customer: customer,
		reply:    make(chan *Order, 1),
	}
}

func customerEnters(customer string, latch *sync.WaitGroup) {
	defer latch.Done()

	for mealsEaten := 0; mealsEaten < 5; mealsEaten++ {
		order := NewOrder(customer)
		log.Println(customer + " has placed an order and is now waiting...")

		select {
		case waiter <- order:
			meal := <-order.reply
			var message1 string = fmt.Sprintf("is eating his food (Order ID: %d, prepared by: %s)...", meal.id, meal.preparedBy)
			do(2, customer, message1)

		case <-time.After(7 * time.Second):
			do(5, customer, "waited too long and left... (they will return to try again shortly)")
			break
		}
	}
	log.Println(customer + " finished 5 meals and is heading home...")
}

func prepareOrder(c *Cook) {
	for order := range waiter {
		c.mu.Lock()
		if !c.busy {
			c.busy = true
			c.mu.Unlock()

			var message2 string = fmt.Sprintf("is preparing order #%d...", order.id)
			do(10, c.name, message2)

			c.mu.Lock()
			c.busy = false
			c.mu.Unlock()

			order.preparedBy = c.name
			order.reply <- order
		} else {
			c.mu.Unlock()
			waiter <- order
		}
	}
}

func main() {
	log.Println("Restaurant Opening!")
	var waitGroup sync.WaitGroup

	customerNames := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}
	for _, customer := range customerNames {
		waitGroup.Add(1)
		go customerEnters(customer, &waitGroup)
	}

	cooks := []Cook{
		{name: "Remy"},
		{name: "Linguine"},
		{name: "Colette"},
	}

	for _, cook := range cooks {
		go prepareOrder(&cook)
	}

	waitGroup.Wait()
	log.Println("Restaurant closing!")
}
