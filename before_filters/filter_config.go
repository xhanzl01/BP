package before_filters

type OutputFilter func(*RequestBody, *ResponseBody) (bool, error)

type ResponseBody struct {
}

type InputParameter struct {
	name        string
	isMandatory bool
	check       ValueCheck
}

type ProxyRequestType struct {
	minParams       int
	maxParams       int
	OutputFilters   []OutputFilter
	InputParameters []InputParameter
}

// ProxyRequestRoutes maps name of the method to filters
var ProxyRequestRoutes = map[string]ProxyRequestType{
	"accounts": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"blockNumber": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"call": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "transaction_call_object",
				isMandatory: true,
				check:       CheckTransactionCallObject,
			},
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"chainId": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"estimateGas": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "transaction_call_object",
				isMandatory: true,
				check:       CheckTransactionCallObject,
			},
		},
	},
	"gasPrice": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"getBalance": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "address",
				isMandatory: true,
				check:       CheckAddress,
			},
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getBlockByHash": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_hash",
				isMandatory: true,
				check:       CheckHash,
			},
			{
				name:        "show_transaction_details_flag",
				isMandatory: true,
				check:       CheckBoolean,
			},
		},
	},
	"getBlockByNumber": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
			{
				name:        "show_transaction_details_flag",
				isMandatory: true,
				check:       CheckBoolean,
			},
		},
	},
	"getBlockTransactionCountByHash": {
		minParams: 1,
		maxParams: 1,
		InputParameters: []InputParameter{
			{
				name:        "block_hash",
				isMandatory: true,
				check:       CheckHash,
			},
		},
	},
	"getBlockTransactionCountByNumber": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getCode": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "address",
				isMandatory: true,
				check:       CheckAddress,
			},
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getLogs": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "filter_object",
				isMandatory: false,
				check:       CheckFilterObject,
			},
		},
	},
	"getStorageAt": {
		minParams:     1,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "address",
				isMandatory: false,
				check:       CheckAddress,
			},
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getTransactionByBlockHashAndIndex": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_hash",
				isMandatory: true,
				check:       RegexHex,
			},
			{
				name:        "transaction_index_position",
				isMandatory: true,
				check:       RegexHex,
			},
		},
	},
	"getTransactionByBlockNumberAndIndex": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
			{
				name:        "transaction_index_position",
				isMandatory: true,
				check:       RegexHex,
			},
		},
	},
	"getTransactionByHash": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "transaction_hash",
				isMandatory: true,
				check:       CheckHash,
			},
		},
	},
	"getTransactionCount": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "address",
				isMandatory: true,
				check:       CheckAddress,
			},
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getTransactionReceipt": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "transaction_hash",
				isMandatory: true,
				check:       CheckHash,
			},
		},
	},
	"getUncleByBlockHashAndIndex": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_hash",
				isMandatory: true,
				check:       RegexHex,
			},
			{
				name:        "uncle_index_position",
				isMandatory: true,
				check:       RegexHex,
			},
		},
	},
	"getUncleByBlockNumberAndIndex": {
		minParams:     2,
		maxParams:     2,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
			{
				name:        "uncle_index_position",
				isMandatory: true,
				check:       RegexHex,
			},
		},
	},
	"getUncleByBlockHash": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_hash",
				isMandatory: true,
				check:       CheckHash,
			},
		},
	},
	"getUncleCountByBlockNumber": {
		minParams:     1,
		maxParams:     1,
		OutputFilters: []OutputFilter{},
		InputParameters: []InputParameter{
			{
				name:        "block_parameter",
				isMandatory: true,
				check:       CheckBlockParameter,
			},
		},
	},
	"getWork": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"hashrate": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"mining": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"protocolVersion": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"sendRawTransaction": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"submitWork": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
	"syncing": {
		minParams:     0,
		maxParams:     0,
		OutputFilters: []OutputFilter{},
	},
}
