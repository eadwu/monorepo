pub const GUILD_LIST_TOPIC: &str = "guild.members.all";
pub const GUILD_RECEPTIONIST_TOPIC: &str = "guild.members.receptionist";
pub const GUILD_MERCENARY_TOPIC: &str = "guild.members.mercenary";
pub const GUILD_QUEST_BOARD_TOPIC: &str = "guild.board.quests";

pub const GUILD_ALL_MERCENARY_QUEUE_GROUP: &str = "mercenary";

pub const MS_TO_SEC: u64 = 1000;
pub const GUILD_QUEST_TIMEOUT: u64 = 1 * MS_TO_SEC;

#[derive(Debug, Clone, serde::Deserialize)]
pub struct GuildQuest {
    pub identifier: String,
}

#[derive(Debug, Clone, serde::Serialize)]
pub struct GuildQuestAcknowledgement {
    pub accepted: bool,
    pub quest: String,
    pub mercenary: String,
}

impl GuildQuestAcknowledgement {
    pub fn denied(quest: String) -> GuildQuestAcknowledgement {
        GuildQuestAcknowledgement {
            accepted: false,
            quest: quest,
            mercenary: format!(""),
        }
    }

    pub fn accept(quest: String, mercenary: String) -> GuildQuestAcknowledgement {
        GuildQuestAcknowledgement {
            accepted: true,
            quest: quest,
            mercenary: mercenary,
        }
    }
}
