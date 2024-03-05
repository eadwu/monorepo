package receptionist

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"time"

	"github.com/google/uuid"
	"github.com/grpc-ecosystem/grpc-gateway/v2/runtime"
	"github.com/nats-io/nats.go"
	"github.com/rs/zerolog/log"
	"google.golang.org/grpc"
	"google.golang.org/grpc/credentials/insecure"

	"dtensor/scheduler/guild"
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

func (r *Receptionist) Routine(addr string, grpcPort uint64, httpPort uint64) {
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
		pb.RegisterReceptionistServer(grpcServer, &ReceptionistInterface{receptionist: r})
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
	receptionist *Receptionist
}

func (r ReceptionistInterface) NatsClient() *nats.Conn {
	return r.receptionist.nc
}

func (r *ReceptionistInterface) Active(ctx context.Context, _param *pb.Empty) (*pb.Acknowledgement, error) {
	return &pb.Acknowledgement{Ok: true}, nil
}

func (r *ReceptionistInterface) Request(stream pb.Receptionist_RequestServer) error {
	for {
		request, err := stream.Recv()
		if err == io.EOF {
			return nil
		}
		if err != nil {
			return err
		}
		log.Debug().Msgf("Got request specified as %v", request)

		questId, err := uuid.NewRandom()
		if err != nil {
			log.Error().Msgf("Failed to generate identifier for request: %v", err)
			stream.Send(&pb.RequestAcknowledgement{Success: false})
			continue
		}

		quest, err := json.Marshal(guild.GuildQuest{Identifier: questId.String()})
		if err != nil {
			log.Error().Msgf("Failed to construct quest details for request: %v", err)
			stream.Send(&pb.RequestAcknowledgement{Success: false})
			continue
		}

		if _, err := r.NatsClient().Request(
			guild.GUILD_QUEST_BOARD_TOPIC, quest, time.Duration(guild.GUILD_QUEST_TIMEOUT)*time.Millisecond,
		); err != nil {
			log.Error().Msgf("Request `%s` failed to be accepted by anyone within the guild: %v", questId, err)
			continue
		}

		log.Info().Msgf("Request `%s` has been accepted", questId)
		stream.Send(&pb.RequestAcknowledgement{
			Success:    true,
			Identifier: questId.String(),
		})
	}
}
