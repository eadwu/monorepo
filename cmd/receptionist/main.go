package main

import (
	"dtensor/scheduler/logger"
	"flag"
	"os"

	"github.com/nats-io/nats.go"
	"github.com/rs/zerolog/log"
)

func main() {
	flag.BoolVar(&logger.PrettyLogging, "pretty", false, "Prettify output of logs")
	flag.Parse()

	switch os.Getenv("LOG_LEVEL") {
	case "TRACE":
		logger.Verbosity = logger.TRACE
	case "DEBUG":
		logger.Verbosity = logger.DEBUG
	default:
		logger.Verbosity = logger.INFO
	}

	logger.Init()

	NATS_URL := flag.Arg(0)
	nc, err := nats.Connect(NATS_URL)
	if err != nil {
		log.Fatal().Msgf("Failed to connect to NATS server located at `%s`", NATS_URL)
	}
	defer nc.Close()
	log.Info().Msgf("Connected to NATS server located at `%s`", NATS_URL)
}
