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
	name         string
	busy         bool
	mu           sync.Mutex
	startingWork bool
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
			log.Printf("%s going home \n", customer)
			return
		}

		order := NewOrder(customer)

		select {
		case waiter <- order:
			log.Printf("%s placed order %d \n", customer, order.id)
			meal := <-order.reply
			do(2, fmt.Sprintf("%s eating cooked order %d prepared by %s", customer, meal.id, order.preparedBy), rand.New(rand.NewSource(time.Now().UnixNano())))
			atomic.AddInt32(mealsEaten, 1)
		case <-time.After(7 * time.Second):
			do(5, fmt.Sprintf("%s waiting too long, abandoning order %d", customer, order.id), rand.New(rand.NewSource(time.Now().UnixNano())))
			time.Sleep(time.Duration(rand.Intn(2500)+2500) * time.Millisecond)
		}
	}
}

func prepareOrder(cook *Cook) {
	for order := range waiter {
		cook.mu.Lock()
		if !cook.busy {
			cook.busy = true
			cook.mu.Unlock()
			do(10, fmt.Sprintf("%s cooking order %d for %s", cook.name, order.id, order.customer), rand.New(rand.NewSource(time.Now().UnixNano())))
			cook.mu.Lock()
			cook.busy = false
			cook.mu.Unlock()
			order.preparedBy = cook.name
			order.reply <- order
		} else {
			cook.mu.Unlock()
		}
	}
}

func main() {
	var waitingGroup sync.WaitGroup
	customerNames := []string{"Ani", "Bai", "Cat", "Dao", "Eve", "Fay", "Gus", "Hua", "Iza", "Jai"}
	mealsEaten := make(map[string]*int32)

	for _, customer := range customerNames {
		mealsEaten[customer] = new(int32)
		waitingGroup.Add(1)
		go customerEnters(customer, &waitingGroup, mealsEaten[customer])
	}

	cooks := []Cook{
		{name: "Remy"},
		{name: "Colette"},
		{name: "Linguini"},
	}

	for _, cook := range cooks {
		log.Printf("%s starting work", cook.name)
		go prepareOrder(&cook)
	}
	waitingGroup.Wait()
	log.Println("Restaurant closing")
}
