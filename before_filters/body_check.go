package before_filters

import (
	"fmt"
	"github.com/ethereum/go-ethereum/common"
	"github.com/ethereum/go-ethereum/common/hexutil"
)

// ValueCheck takes interface as the param from request body and bool which is set to true, if param is mandatory and
// must be set in request body
type ValueCheck func(interface{}, bool) (bool, error)

// CheckAddress checks whether val is a valid ethereum wallet using regex
func CheckAddress(val interface{}, isMandatory bool) (ok bool, err error) {
	v, ok := val.(string)
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory address param")
		}
		return true, nil
	}

	ok = common.IsHexAddress(v)
	if !ok {
		return false, fmt.Errorf("invalid format of address param")
	}
	return true, nil
}

// CheckHex checks whether val is a valid hex number
func CheckHex(val interface{}, isMandatory bool) (ok bool, err error) {
	v, ok := val.(string)
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory param")
		}
		return true, nil
	}

	_, err = hexutil.Decode(v)
	if err != nil {
		return false, fmt.Errorf("wrong hex format")
	}

	return true, nil
}

// CheckBlockParameter takes whole Block Parameter param and checks whether the form of each part is valid
func CheckBlockParameter(val interface{}, isMandatory bool) (ok bool, err error) {
	v, ok := val.(string)
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory param")
		}
		return true, nil

	}
	intv, err := hexutil.DecodeUint64(v)
	if err != nil {
		if v == "latest" || v == "earliest" || v == "pending" {
			return true, nil
		}
		return false, fmt.Errorf("invalid block parameter format")
	}
	//topBlock :=
	if intv > getTopBlock() {
		return false, fmt.Errorf("invalid number of block")
	}

	return true, nil
}

// CheckHash checks whether val is a hash
func CheckHash(val interface{}, isMandatory bool) (ok bool, err error) {
	_, ok = val.(string)
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory hash param")
		}
		return true, nil
	}
	return true, nil
}

// CheckBoolean checks whether val is a bool value
// TODO not done yet
func CheckBoolean(val interface{}, isMandatory bool) (ok bool, err error) {
	_, ok = val.(bool)
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory param")
		}
		return true, nil
	}
	//if !strings.EqualFold(v, "false") || !strings.EqualFold(v, "true") {
	//	return false, fmt.Errorf("invalid boolean param")
	//}
	return true, nil
}

// CheckTransactionCallObject takes the whole Transaction Call Object param and checks whether the form of each
// part is valid
func CheckTransactionCallObject(val interface{}, isMandatory bool) (ok bool, err error) {
	m, ok := val.(map[string]interface{})
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory param")
		}
		return true, nil
	}

	ok, err = CheckAddress(m["from"], true)
	if !ok {
		return false, err
	}

	ok, err = CheckAddress(m["to"], true)
	if !ok {
		return false, err
	}

	ok, err = CheckHex(m["gas"], false)
	if !ok {
		return false, err
	}

	ok, err = CheckHex(m["gasPrice"], false)
	if !ok {
		return false, err
	}

	ok, err = CheckHex(m["value"], false)
	if !ok {
		return false, err
	}

	ok, err = CheckHex(m["data"], false)
	if !ok {
		return false, err
	}
	return true, nil
}

// CheckFilterObject takes the whole Filter Object param and checks whether the form of each part is valid
func CheckFilterObject(val interface{}, isMandatory bool) (ok bool, err error) {
	//TODO cannot specify both BlockHash and FromBlock/ToBlock,
	m, ok := val.(map[string]interface{})
	if !ok {
		if isMandatory {
			return false, fmt.Errorf("missing mandatory param")
		}
		return true, nil
	}
	ok, err = CheckAddress(m["address"], false)
	if !ok {
		return false, err
	}

	ok, err = CheckBlockParameter(m["fromBlock"], false)
	if !ok {
		return false, err
	}

	ok, err = CheckBlockParameter(m["toBlock"], false)
	if !ok {
		return false, err
	}

	// TODO finish topics checker
	//if !(reflect.TypeOf(m["topics"]) == reflect.TypeOf(reflect.Interface)) {
	//	return false, fmt.Errorf("mandatory param is either missing or is in wrong form")
	//}

	ok, err = CheckHash(m["blockhash"], false)
	if !ok {
		return false, err
	}

	return true, nil
}
