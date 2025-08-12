-- name: CreateCategory :one
INSERT INTO categories
            ( name
            )
     VALUES ( ?
            )
  RETURNING *
;

-- name: GetCategory :one
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
 WHERE category_id = ?
;


-- name: ListCategories :many
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
;


-- name: ListCategoriesNonStale :many
SELECT category_id
     , name
     , created_at
     , updated_at
  FROM categories
 WHERE updated_at > (strftime('%s', 'now') - 90 * 24 * 60 * 60)
;


-- name: UpdateCategory :exec
UPDATE categories
   SET name        = ?
 WHERE category_id = ?
;


-- name: DeleteCategory :exec
DELETE FROM categories
      WHERE category_id = ?
;
