package utils

import "log"

func Must[T any](v T, err error) T {
	if err != nil {
		log.Fatal(err)
	}
	return v
}

func Must1(err error) {
	if err != nil {
		log.Fatal(err)
	}
}