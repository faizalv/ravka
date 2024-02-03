package runtime

import (
	"sync"
)

var once sync.Once

type ContainerInterface interface {
	GetParseRuntime() ParseInterface
}

type Runtime struct {
	parseRuntime ParseInterface
}

var runtime *Runtime

func GetRuntimeContainer(parseRuntime ParseInterface) *Runtime {
	if runtime == nil {
		once.Do(func() {
			runtime = &Runtime{
				parseRuntime: parseRuntime,
			}
		})
	}

	return runtime
}

func (r *Runtime) GetParseRuntime() ParseInterface {
	return r.parseRuntime
}
