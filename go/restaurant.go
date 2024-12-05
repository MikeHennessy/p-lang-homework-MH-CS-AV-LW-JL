package main

import (
	"fmt"
	"log"
	"math/rand"
	"sync"
	"sync/atomic"
	"time"
)

func do(seconds int, action string, rng *rand.Rand) {
	log.Println(action)
	randomMillis := 500*seconds + rng.Intn(500*seconds)
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
	orderId := nextId.Add(1)
	return &Order{
		id:       orderId,
		customer: customer,
		reply:    make(chan *Order, 1),
	}
}

func customerEnters(customer string, customers *sync.WaitGroup, mealsEaten *int32) {
	defer customers.Done()

	for {
		if *mealsEaten >= 5 {
			log.Printf("%s has eaten 5 meals and is heading home...\n", customer)
			return
		}

		log.Printf("%s has entered the restaurant and is waiting to place an order...\n", customer)

		order := NewOrder(customer)

		select {
		case waiter <- order:
			log.Printf("%s has placed order ID %d and is now waiting...\n", customer, order.id)
			meal := <-order.reply
			do(2, fmt.Sprintf("%s is eating meal ID %d", customer, meal.id), rand.New(rand.NewSource(time.Now().UnixNano())))
			atomic.AddInt32(mealsEaten, 1)
		case <-time.After(7 * time.Second):
			do(5, fmt.Sprintf("%s waited too long and is leaving... Customer comes back later.", customer), rand.New(rand.NewSource(time.Now().UnixNano())))
			time.Sleep(time.Duration(rand.Intn(2500)+2500) * time.Millisecond)
		}
	}
}

func prepareOrder(chef *Cook) {
	for order := range waiter {
		chef.mu.Lock()
		if !chef.busy {
			chef.busy = true
			chef.mu.Unlock()
			do(10, fmt.Sprintf("%s is cooking order ID %d", chef.name, order.id), rand.New(rand.NewSource(time.Now().UnixNano())))
			chef.mu.Lock()
			chef.busy = false
			chef.mu.Unlock()
			order.preparedBy = chef.name
			order.reply <- order
		} else {
			chef.mu.Unlock()
		}
	}
}

func main() {

	var customers sync.WaitGroup
	customerNames := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}

	mealsEaten := make(map[string]*int32)

	for _, customer := range customerNames {
		mealsEaten[customer] = new(int32)
		customers.Add(1)
		go customerEnters(customer, &customers, mealsEaten[customer])
	}

	cooks := []Cook{
		{name: "Remy"},
		{name: "Colette"},
		{name: "Linguini"},
	}

	for _, cook := range cooks {
		go prepareOrder(&cook)
	}
	customers.Wait()
	log.Println("All customers have finished eating and gone home. Restaurant is shutting down.")
}
