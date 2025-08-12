package database

import (
	"context"
	"database/sql"
	_ "embed"
	"log"
	"pfm/internal/utils"
	"pfm/queries"
	"reflect"

	_ "modernc.org/sqlite"
)

//go:embed schema.sql
var schema string

//go:embed seed.sql
var seed string

func MustInitDB() *sql.DB {
	ctx := context.Background()

	db := utils.Must(sql.Open("sqlite", ":memory:"))

	// create tables
	utils.Must(db.ExecContext(ctx, schema))

	// seed
	if _, err := db.ExecContext(ctx, seed); err != nil {
		log.Fatal(err)
	}

	queries_ := queries.New(db)

	// list all categories
	categories := utils.Must(queries_.ListCategories(ctx))
	log.Println(categories)

	insertedCategory := utils.Must(queries_.CreateCategory(ctx, "Equity2"))
	log.Println(insertedCategory)

	// get the category we just inserted
	fetchedCategory := utils.Must(queries_.GetCategory(ctx, insertedCategory.CategoryID))

	// prints true
	log.Printf("\n\n---\n")
	log.Printf("Both match: %t", reflect.DeepEqual(insertedCategory, fetchedCategory))
	log.Printf("insertedCategory : %+v", insertedCategory)
	log.Printf("fetchedCategory  : %+v", fetchedCategory)
	log.Printf("\n---\n\n")

	return db
}
