package runtime

import "ravka/token"

type WatcherInterface interface {
	Register(eventKey string, initialValue token.Token)
	Listen(eventKey string, cb func(newVal token.Token, oldVal token.Token))
}
