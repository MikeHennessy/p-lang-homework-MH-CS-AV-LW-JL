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
func do(seconds int, action string, rng *rand.Rand) {
	log.Println(action)
	randomMillis := 500*seconds + rng.Intn(500*seconds)
	time.Sleep(time.Duration(randomMillis) * time.Millisecond)
}

// Order represents a customer's order.
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
	startingWork bool
}

var nextId atomic.Uint64
var waiter = make(chan *Order, 3)

// NewOrder creates a new order with a unique ID.
func NewOrder(customer string) *Order {
	// Only generate the ID when the order is placed with the waiter
	orderId := nextId.Add(1)
	return &Order{
		id:       orderId,
		customer: customer,
		reply:    make(chan *Order, 1),
	}
}

// customerEnters simulates a customer entering the restaurant, placing orders, and eating.
func customerEnters(customer string, waitingGroup *sync.WaitGroup, mealsEaten *int32) {
	defer waitingGroup.Done()

	// Continue placing orders until 5 meals have been eaten
	for {
		if *mealsEaten >= 5 {
			log.Printf("%s going home \n", customer)
			return
		}

		// Create a new order for the customer
		order := NewOrder(customer)

		// Try placing the order with the waiter
		select {
		case waiter <- order:
			log.Printf("%s placed order %d \n", customer, order.id)
			// Wait for the meal to be prepared
			meal := <-order.reply
			// Simulate eating the meal
			do(2, fmt.Sprintf("%s eating cooked order %d prepared by %s", customer, meal.id, order.preparedBy), rand.New(rand.NewSource(time.Now().UnixNano())))
			atomic.AddInt32(mealsEaten, 1)
		case <-time.After(7 * time.Second):
			// Customer waited too long and is leaving, will come back later
			do(5, fmt.Sprintf("%s waiting too long, abandoning order %d", customer, order.id), rand.New(rand.NewSource(time.Now().UnixNano())))
			time.Sleep(time.Duration(rand.Intn(2500)+2500) * time.Millisecond)
		}
	}
}

// prepareOrder simulates a cook preparing an order.
func prepareOrder(cook *Cook) {
	for order := range waiter {
		cook.mu.Lock()
		if !cook.busy {
			cook.busy = true
			cook.mu.Unlock()

			// Simulate cooking time
			do(10, fmt.Sprintf("%s cooking order %d for %s", cook.name, order.id, order.customer), rand.New(rand.NewSource(time.Now().UnixNano())))

			// Mark cook as not busy and deliver the meal to the customer
			cook.mu.Lock()
			cook.busy = false
			cook.mu.Unlock()

			order.preparedBy = c.name
			order.reply <- order
		} else {
			cook.mu.Unlock()
		}
	}
}

func main() {
	var waitingGroup sync.WaitGroup

	// List of customers
	customerNames := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}

	// Track how many meals each customer has eaten
	mealsEaten := make(map[string]*int32)

	// Start customer goroutines
	for _, customer := range customerNames {
		mealsEaten[customer] = new(int32)
		waitingGroup.Add(1)
		go customerEnters(customer, &waitingGroup, mealsEaten[customer])
	}

	// Start 3 cook goroutines
	cooks := []Cook{
		{name: "Remy"},
		{name: "Colette"},
		{name: "Linguini"},
	}

	for _, cook := range cooks {
		log.Printf("%s starting work", cook.name)
		go prepareOrder(&cook)
	}

	// Wait for all customers to finish
	waitingGroup.Wait()
	log.Println("Restaurant closing")
}
