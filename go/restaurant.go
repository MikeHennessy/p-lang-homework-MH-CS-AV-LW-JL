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
	log.Println(action) // This will include the timestamp automatically
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
}

var nextId atomic.Uint64          // Global atomic counter for meal IDs
var waiter = make(chan *Order, 3) // Waiter can hold only 3 orders at a time

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
func customerEnters(customer string, wg *sync.WaitGroup, mealsEaten *int32) {
	defer wg.Done()

	// Continue placing orders until 5 meals have been eaten
	for {
		if *mealsEaten >= 5 { // Check if the customer has eaten 5 meals
			log.Printf("%s has eaten 5 meals and is heading home...\n", customer)
			return // Stop placing orders when 5 meals are eaten
		}

		log.Printf("%s has entered the restaurant and is waiting to place an order...\n", customer)

		// Create a new order for the customer
		order := NewOrder(customer)

		// Try placing the order with the waiter
		select {
		case waiter <- order: // If the waiter has space, the order is placed
			log.Printf("%s has placed order ID %d and is now waiting...\n", customer, order.id)
			// Wait for the meal to be prepared
			meal := <-order.reply
			// Simulate eating the meal
			do(2, fmt.Sprintf("%s is eating meal ID %d", customer, meal.id), rand.New(rand.NewSource(time.Now().UnixNano())))
			atomic.AddInt32(mealsEaten, 1) // Increment meals eaten count
		case <-time.After(7 * time.Second): // Wait for at most 7 seconds
			// Customer waited too long and is leaving, will come back later
			do(5, fmt.Sprintf("%s waited too long and is leaving... Customer comes back later.", customer), rand.New(rand.NewSource(time.Now().UnixNano())))
			time.Sleep(time.Duration(rand.Intn(2500)+2500) * time.Millisecond) // Customer waits 2.5 - 5 seconds before coming back
		}
	}
}

// prepareOrder simulates a cook preparing an order.
func prepareOrder(c *Cook) {
	for order := range waiter {
		c.mu.Lock()
		if !c.busy {
			c.busy = true
			c.mu.Unlock()

			// Simulate cooking time
			do(10, fmt.Sprintf("%s is cooking order ID %d", c.name, order.id), rand.New(rand.NewSource(time.Now().UnixNano())))

			// Mark cook as not busy and deliver the meal to the customer
			c.mu.Lock()
			c.busy = false
			c.mu.Unlock()

			order.preparedBy = c.name
			order.reply <- order
		} else {
			c.mu.Unlock()
		}
	}
}

func main() {

	var wg sync.WaitGroup
	// List of customers
	customerNames := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}

	// Track how many meals each customer has eaten
	mealsEaten := make(map[string]*int32)

	// Start customer goroutines
	for _, customer := range customerNames {
		mealsEaten[customer] = new(int32)
		wg.Add(1)
		go customerEnters(customer, &wg, mealsEaten[customer])
	}

	// Start 3 cook goroutines
	cooks := []Cook{
		{name: "Remy"},
		{name: "Colette"},
		{name: "Linguini"},
	}

	for _, cook := range cooks {
		go prepareOrder(&cook)
	}

	// Wait for all customers to finish
	wg.Wait()
	log.Println("All customers have finished eating and gone home. Restaurant is shutting down.")
}
