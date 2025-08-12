package handlers

import (
	"context"
	"fmt"
	"log"
	"net/http"
	"os"
	"pfm/queries"

	"github.com/a-h/templ"
)

type Handlers struct {
	queries *queries.Queries
}

func New(queries *queries.Queries) *Handlers {
	return &Handlers{
		queries: queries,
	}
}

func (h *Handlers) RegisterRoutes() {
	// Demo routes to show templates work
	fmt.Println()
	component := hello("John")
	_ = component.Render(context.Background(), os.Stdout)
	fmt.Println()
	_ = button("John", "Hello").Render(context.Background(), os.Stdout)
	fmt.Println()

	http.Handle("/", templ.Handler(hello("Ben")))
	http.Handle("/hi", templ.Handler(headerTemplate("Benjamin")))
	http.HandleFunc("/greet", h.handleGreet)
	http.HandleFunc("/categories", h.handleCategories)
}

func (h *Handlers) handleGreet(w http.ResponseWriter, r *http.Request) {
	name := r.URL.Query().Get("name")
	if name == "" {
		name = "Anonymous"
	}
	w.Header().Set("Content-Type", "text/html")
	_ = headerTemplate(name).Render(r.Context(), w)
}

func (h *Handlers) handleCategories(w http.ResponseWriter, r *http.Request) {
	w.Header().Set("Content-Type", "text/html")
	categories, err := h.queries.ListCategories(r.Context())
	if err != nil {
		log.Fatal(err)
	}
	_ = listCategoriesTemplate(categories).Render(r.Context(), w)
}
