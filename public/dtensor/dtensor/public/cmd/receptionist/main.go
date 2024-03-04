package main

import (
	"dtensor/scheduler/logger"
	"dtensor/scheduler/receptionist"
	"flag"
	"os"

	"github.com/nats-io/nats.go"
	"github.com/rs/zerolog/log"
)

const (
	LOCALHOST = "127.0.0.1"
	GRPC_PORT = 9090
	HTTP_PORT = 8081
)

func main() {
	address := flag.String("address", LOCALHOST, "Address to listen on")
	grpcPort := flag.Uint64("grpc", GRPC_PORT, "Port to listen for gRPC requests")
	httpPort := flag.Uint64("http", HTTP_PORT, "Port to listen for HTTP requests")
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
	defer nc.Drain()
	defer nc.Close()
	log.Info().Msgf("Connected to NATS server located at `%s`", NATS_URL)

	r, err := receptionist.New(nc)
	if err != nil {
		log.Fatal().Msg("Failed to generate unique identifier")
	}
	r.Routine(*address, *grpcPort, *httpPort)
}
