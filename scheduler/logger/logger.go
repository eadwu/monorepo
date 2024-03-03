package logger

import (
	"os"
	"runtime"
	"strconv"
	"strings"
	"time"

	"github.com/rs/zerolog"
	"github.com/rs/zerolog/log"
)

type LogLevel int

const (
	INFO LogLevel = iota
	DEBUG
	TRACE
)

var PrettyLogging = false
var Verbosity = INFO

func Init() {
	zerolog.TimeFieldFormat = time.RFC3339Nano

	log.Logger = log.With().Caller().Logger()
	if PrettyLogging {
		log.Logger = log.Output(zerolog.ConsoleWriter{Out: os.Stderr})
	}

	switch Verbosity {
	case INFO:
		zerolog.SetGlobalLevel(zerolog.InfoLevel)
	case DEBUG:
		zerolog.SetGlobalLevel(zerolog.DebugLevel)
	case TRACE:
		zerolog.SetGlobalLevel(zerolog.TraceLevel)
	}

	zerolog.CallerMarshalFunc = func(pc uintptr, file string, line int) string {
		basename := file
		if lastSlashIndex := strings.LastIndex(basename, "/"); lastSlashIndex != -1 {
			basename = basename[lastSlashIndex+1:]
		}

		function := ""
		if f := runtime.FuncForPC(pc); f != nil {
			name := f.Name()
			if lastSlashIndex := strings.LastIndex(name, "/"); lastSlashIndex != -1 {
				name = name[lastSlashIndex+1:]
			}

			function = name + "()"
		}

		return basename + ":" + strconv.Itoa(line) + " " + function
	}
}
