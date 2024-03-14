#![allow(non_camel_case_types)]
mod resource;
pub use resource::*;

pub const GUILD_LIST_TOPIC: &str = "guild.members";
pub const GUILD_RECEPTIONIST_TOPIC: &str = "guild.members.receptionist";
pub const GUILD_MERCENARY_TOPIC: &str = "guild.members.mercenary";
pub const GUILD_QUEST_BOARD_TOPIC: &str = "guild.board.quests";

pub const GUILD_DEFAULT_PARTY: &str = "mercenary";

pub const MS_TO_SEC: u64 = 1000;
pub const GUILD_QUEST_TIMEOUT: u64 = 1 * MS_TO_SEC;

include!(concat!(env!("OUT_DIR"), "/guild.rs"));

impl GuildQuestAcknowledgement {
    pub fn deny(quest: String) -> GuildQuestAcknowledgement {
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

impl Resources {
    fn _device_type(&self) -> DeviceType {
        DeviceType::try_from(self.device_type).unwrap()
    }

    fn _device_brand(&self) -> DeviceBrand {
        DeviceBrand::try_from(self.device_brand).unwrap()
    }

    fn _region(&self) -> Region {
        Region::try_from(self.region).unwrap()
    }
}

impl Requirement for Resources {
    fn satisfies(&self, other: &Self) -> bool {
        self.memory.satisfies(&other.memory)
            && self._device_type().satisfies(&other._device_type())
            && self._device_brand().satisfies(&other._device_brand())
            && self._region().satisfies(&other._region())
    }
}
