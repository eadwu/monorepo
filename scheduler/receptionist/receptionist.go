package receptionist

import (
	"context"
	"fmt"
	"net"
	"net/http"

	"github.com/google/uuid"
	"github.com/grpc-ecosystem/grpc-gateway/v2/runtime"
	"github.com/nats-io/nats.go"
	"github.com/rs/zerolog/log"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	pb "dtensor/scheduler/receptionist/grpc"
)

type Receptionist struct {
	identifier uuid.UUID
	nc         *nats.Conn
}

func New(nc *nats.Conn) (Receptionist, error) {
	id, err := uuid.NewRandom()
	if err != nil {
		return Receptionist{
			identifier: uuid.New(),
			nc:         nc,
		}, err
	}

	instance := Receptionist{
		identifier: id,
		nc:         nc,
	}
	return instance, nil
}

func (r Receptionist) Routine(addr string, grpcPort uint64, httpPort uint64) {
	grpcEndpoint := fmt.Sprintf("%s:%d", addr, grpcPort)
	httpEndpoint := fmt.Sprintf("%s:%d", addr, httpPort)
	log.Trace().Msgf("`%s`: gRPC: `%s` HTTP: `%s`", r.identifier, grpcEndpoint, httpEndpoint)
	log.Debug().Msgf("Receptionist `%s` attempting to join guild", r.identifier)

	// Spawn gRPC endpoint
	{
		conn, err := net.Listen("tcp", grpcEndpoint)
		if err != nil {
			log.Fatal().Msgf("Failed to setup grpc interface: %v", err)
		}

		var opts []grpc.ServerOption
		grpcServer := grpc.NewServer(opts...)
		pb.RegisterReceptionistServer(grpcServer, &ReceptionistInterface{})
		go grpcServer.Serve(conn)
	}

	// Mux'ed HTTP REST API
	{
		ctx := context.Background()
		ctx, cancel := context.WithCancel(ctx)
		defer cancel()

		mux := runtime.NewServeMux()
		opts := []grpc.DialOption{grpc.WithTransportCredentials(insecure.NewCredentials())}
		if err := pb.RegisterReceptionistHandlerFromEndpoint(ctx, mux, grpcEndpoint, opts); err != nil {
			log.Fatal().Msgf("Failed to register grpc interface: %v", err)
		}

		log.Trace().Msgf("Welcome to the guild, receptionist `%s`", r.identifier)
		log.Info().Msgf("Setting up quest board for `%s`", r.identifier)
		if err := http.ListenAndServe(httpEndpoint, mux); err != nil {
			log.Fatal().Msgf("Failed to setup http endpoint: %v", err)
		}
	}
}

type ReceptionistInterface struct {
	pb.UnimplementedReceptionistServer
}

func (r *ReceptionistInterface) Active(ctx context.Context, _param *pb.Empty) (*pb.Acknowledgement, error) {
	return &pb.Acknowledgement{Ok: true}, nil
}
