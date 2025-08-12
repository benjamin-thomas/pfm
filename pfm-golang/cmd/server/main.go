package main

import (
	"fmt"
	"log"
	"net/http"
	"os"
	"pfm/internal/database"
	"pfm/internal/handlers"
	"pfm/queries"
)

func main() {
	db := database.MustInitDB()
	defer func() {
		if err := db.Close(); err != nil {
			log.Printf("Error closing database: %v", err)
		}
	}()

	queries_ := queries.New(db)

	// Initialize handlers with dependencies
	h := handlers.New(queries_)

	// Register routes
	h.RegisterRoutes()

	port := os.Getenv("PORT")
	if port == "" {
		port = "3000"
	}

	fmt.Printf("\nRunning server on port: %s\n", port)
	log.Fatal(http.ListenAndServe(":"+port, nil))
}
