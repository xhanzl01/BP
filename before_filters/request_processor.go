package before_filters

import (
	"bytes"
	"encoding/json"
	"fmt"
	"io"
	"io/ioutil"
	"net/http"
	"strings"
)

type RequestBody struct {
	Jsonrpc string
	Method  string
	Params  []interface{}
	Id      int
}

// CheckNumberOfParams checks the number of params in request and compares it to the correct interval.
// If numberOfParams is smaller than minParams or bigger than maxParams the function returns error message.
func CheckNumberOfParams(numberOfParams int, minParams int, maxParams int) (err error) {
	if !(numberOfParams >= minParams && numberOfParams <= maxParams) {
		return fmt.Errorf("invalid number of params")
	}
	return nil
}

// CheckBodyValidity tries to unmarshal request into rb. If the function fails, it returns error message.
func CheckBodyValidity(body []byte, rb *RequestBody) (err error) {
	bodyReader := bytes.NewReader(body)
	err = json.NewDecoder(bodyReader).Decode(&rb)
	if err != nil {
		return fmt.Errorf("invalid request body format")
	}
	return nil
}

// RouteRequestsIn calls all the checks and filters. If every check and filter returns no error, the function returns
// new body in form of Reader. If error occurs, function immediately returns an error.
func RouteRequestsIn(req *http.Request) (error, io.Reader) {
	var rb RequestBody

	// Request body processing - if wrong, breaks the request and returns response with error
	body, err := ioutil.ReadAll(req.Body)
	if err != nil {
		return err, nil
	}

	err = CheckBodyValidity(body, &rb)
	if err != nil {
		return err, nil
	}

	s := strings.Split(rb.Method, "_")
	if len(s) < 2 {
		return fmt.Errorf("invalid method name"), nil
	}
	fmt.Println("used method: " + s[1])

	m, ok := ProxyRequestRoutes[s[1]]
	if !ok {
		return fmt.Errorf("invalid method name"), nil
	}

	err = CheckNumberOfParams(len(rb.Params), m.minParams, m.maxParams)
	if err != nil {
		return err, nil
	}

	if len(rb.Params) != 0 {
		for i, p := range m.InputParameters {
			_, err := p.check(rb.Params[i], p.isMandatory)
			if err != nil {
				return err, nil
			}
		}
	}

	body, err = json.Marshal(rb)
	if err != nil {
		return err, nil
	}
	return nil, bytes.NewReader(body)
}
