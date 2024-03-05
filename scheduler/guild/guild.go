package guild

import "time"

const (
	GUILD_LIST_TOPIC         = "guild.members.all"
	GUILD_RECEPTIONIST_TOPIC = "guild.members.receptionist"
	GUILD_QUEST_BOARD_TOPIC  = "guild.board.quests"
	GUILD_QUEST_TIMEOUT      = 5 * time.Second
)
