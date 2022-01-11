package before_filters

import (
	"net/http"
	"strings"
)

// FilterHeaders deletes all headers from a request that are not in the whiteListArray
func FilterHeaders(header http.Header) (newHeader http.Header, err error) {

	var whiteListArray []string
	whiteListArray = append(whiteListArray, "Accept", "Accept-Charset", "Content-Encoding", "Content-Length", "Content-Type")

	newHeader = http.Header{"Content-Type": nil}
	for i := 0; i < len(whiteListArray); i++ {
		val, ok := header[whiteListArray[i]]
		if ok {
			newVal := strings.Join(val[:], ",")
			newHeader.Set(whiteListArray[i], newVal)
		}
	}
	return
}
