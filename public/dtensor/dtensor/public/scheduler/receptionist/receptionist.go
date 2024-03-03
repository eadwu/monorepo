package receptionist

import (
	"github.com/google/uuid"
	"github.com/rs/zerolog/log"
)

type Receptionist struct {
	identifier uuid.UUID
}

func New() (Receptionist, error) {
	id, err := uuid.NewRandom()
	if err != nil {
		return Receptionist{
			identifier: uuid.New(),
		}, err
	}

	instance := Receptionist{
		identifier: id,
	}
	return instance, nil
}

func (r Receptionist) Routine() {
	log.Info().Msgf("Welcome to the guild, receptionist %s", r.identifier)
}
