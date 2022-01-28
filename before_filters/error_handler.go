package before_filters

import (
	"fmt"
	"io"
	"net/http"
)

func handleInternalError(res http.ResponseWriter, err error) {
	http.Error(res, err.Error(), http.StatusInternalServerError)
	panic(err)
	return
}

func returnBadRequest(res http.ResponseWriter, message string) {
	res.WriteHeader(http.StatusBadRequest)
	s := fmt.Sprintf(message)
	_, err := io.WriteString(res, s)
	if err != nil {
		handleInternalError(res, err)
	}
}
