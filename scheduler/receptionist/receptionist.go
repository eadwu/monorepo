package receptionist

import (
	"context"
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
	"google.golang.org/protobuf/proto"

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

func (r *ReceptionistInterface) NatsClient() *nats.Conn {
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
			r.AcknowledgeFail(stream)
			continue
		}

		quest := guild.GuildQuest{Identifier: questId.String()}
		payload, err := proto.Marshal(&quest)
		if err != nil {
			log.Error().Msgf("Failed to construct quest details for request: %v", err)
			r.AcknowledgeFail(stream)
			continue
		}

		topic := guild.GUILD_QUEST_BOARD_TOPIC
		timeout := time.Duration(guild.GUILD_QUEST_TIMEOUT) * time.Millisecond
		log.Info().Msgf("Sending quest `%s` through board `%s`", questId, topic)
		reply, err := r.NatsClient().Request(topic, payload, timeout)
		if err != nil {
			log.Error().Msgf("No reply received for request `%s`: %v", questId, err)
			r.AcknowledgeFail(stream)
			continue
		}

		var acknowledgement guild.GuildQuestAcknowledgement
		if err := proto.Unmarshal(reply.Data, &acknowledgement); err != nil {
			log.Error().Msgf("Failed to determine status of request `%s`: %v", questId, err)
			r.AcknowledgeFail(stream)
			continue
		}

		if !acknowledgement.Accepted {
			log.Info().Msgf("Request `%s` was not accepted within the guild", questId)
			r.AcknowledgeFail(stream)
			continue
		}

		log.Info().Msgf("Request `%s` has been accepted by Mercenary `%s`", questId, acknowledgement.Mercenary)
		stream.Send(&pb.RequestAcknowledgement{
			Success:    true,
			Identifier: questId.String(),
		})
	}
}

func (r *ReceptionistInterface) AcknowledgeFail(stream pb.Receptionist_RequestServer) {
	stream.Send(&pb.RequestAcknowledgement{Success: false})
}
