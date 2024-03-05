package guild

const (
	GUILD_LIST_TOPIC         = "guild.members.all"
	GUILD_RECEPTIONIST_TOPIC = "guild.members.receptionist"
	GUILD_MERCENARY_TOPIC    = "guild.members.mercenary"
	GUILD_QUEST_BOARD_TOPIC  = "guild.board.quests"

	GUILD_ALL_MERCENARY_QUEUE_GROUP = "mercenary"

	MS_TO_SEC           uint64 = 1000
	GUILD_QUEST_TIMEOUT        = 1 * MS_TO_SEC
)
